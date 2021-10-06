# Model fitting
library(mco)

library(HutchCOVID)

# Read in King County actual data for calibration ####
source("kc_read-data.R")

# Model calibration by sum of squared errors (not used) ####
# Calculate error at starting values and bounds
calc_sse_simple(unlist(params_kc), names(params_kc), params_fix, kc_data, kc_age_t, peak_daily_cases)
calc_sse_simple(params_kc_lower, names(params_kc_lower), params_fix, kc_data, kc_age_t, peak_daily_cases)
calc_sse_simple(params_kc_upper, names(params_kc_upper), params_fix, kc_data, kc_age_t, peak_daily_cases)

# Optimization
res = optim(par = unlist(params_kc),
            fn = calc_sse_simple,
            gr = NULL,
            names(params_kc), params_fix, kc_data, kc_age_t, peak_daily_cases,
            method = "L-BFGS-B",
            lower = params_kc_lower,
            upper = params_kc_upper)
# Calibrated parameters
res$par

# Calculate sum of squared errors with calibrated parameters
calc_sse_simple(res$par, names(params_kc_lower), params_fix, kc_data, kc_age_t, peak_daily_cases)

#-------- mco ---------------------------------------------------------------------
# Multicriteria optimizaiton
# Calculate sum of squared errors of model for best estimate paramters, lower and upper bounds
calc_sse_multi(unlist(params_kc), names(params_kc), params_fix, kc_data, kc_age_t, peak_daily_cases)
calc_sse_multi(params_kc_lower, names(params_kc_lower), params_fix, kc_data, kc_age_t, peak_daily_cases)
calc_sse_multi(params_kc_upper, names(params_kc_upper), params_fix, kc_data, kc_age_t, peak_daily_cases)

# run for bigger population to get more solutions

Sys.time()
res2 = nsga2(fn = calc_sse_multi,        # determines what to minimize
             idim = length(params_kc_lower),   # number of parameters to optimize
             odim = 5,                   # number of critria to optimize
             popsize = 100,
             generations = 200,
             names(params_kc_lower), params_fix, kc_data, kc_age_t, peak_daily_cases,   # arguments for fn
             lower.bounds = params_kc_lower, # lower bounds 
             upper.bounds = params_kc_upper) # upper bounds 
Sys.time()



# Result is 90 pareto-optimal combinations of the 11 parameters estimated
# Add names to components of results
colnames(res2$par) = names(params_kc_lower)
colnames(res2$value) = c("cases", "deaths", "age (c)", "age (d)", "peak")
save(res2, file = "out/res2_test.Rdata")

# plot pairs of cases, deaths, age(c), age(d), peak
# why are there 100 observations of these and 90 of the parameters?
plot(res2)

# plot pairs of 11 parameters: d2_1, d2_2, d2_3, d2_4, bstar, beta_d, deltaDoffset, h_3, h_4, sd, alpha 
pairs(res2$par)

# add ranking of each value of output (cases, deaths, age(c), age(d), peak) as new item in res2
res2$err_ranks = apply(res2$value, 2, rank)

# show just the combinations where each has rank in top 30% of ranks
res2$err_ranks[apply(res2$err_ranks, 1, function(x) all(x < 0.7 * nrow(res2$err_ranks))),]


# set colors for pairs plot so that those that weren't part of a high performing parameter set are black
err_pal = c("royalblue2", "firebrick3", "darkorchid3", "mediumslateblue", "orange", "green3", "gray")

# plot pairs of 11 parameters: d2_1, d2_2, d2_3, d2_4, bstar, beta_d, deltaDoffset, h_3, h_4, sd, alpha 
pdf("out/param_set.pdf", width = 10, height = 10)
pairs(res2$par, col = alpha(err_pal[res2$err_best], 0.7))
dev.off()

# plot pairs of cases, deaths, age(c), age(d), peak
pdf("out/param_set_pareto.pdf", width = 10, height = 10)
pairs(res2$value, col = alpha(err_pal[res2$err_best], 0.7))
dev.off()

# plot the parameter outcome that has the lowest mean squared error against actuals (cases and deaths)
pdf("out/best_avg_fit.pdf", width = 10, height = 5)
plot_calibration_params(res2$par[which.min(rowSums(res2$err_ranks)),], names(params_kc_lower), params_fix, kc_data, kc_age_t, 200)
dev.off()


# Plot best results of calibration in pairs of parameters and over projection period against actuals
## all plot data
all_out = get_model_data_param_sets(res2$par, names(params_kc_lower), params_fix, 150, state)

# to plot all solutions
idxs = 1:ncol(all_out$cases)

# to just plot a subset of the 25 best solutions for cases and deaths
idxs = which(res2$err_ranks[,1] <= 0.25 * nrow(res2$err_ranks) & 
               res2$err_ranks[,2] <= 0.25 * nrow(res2$err_ranks)) # good cases and deaths

# try to have compromise criteria for cases, peak and deaths
idxs = which(res2$value[,1] < 5e8 & 
               res2$value[,2] < 5e5 & 
               res2$value[,3] < 3000 &
               res2$value[,4] < 2500 &
               res2$value[,5] <= 100 )

err_legend = function(where)
{
  legend(where, legend = idxs, col = rainbow(length(idxs)), lwd = 2, 
         bty = "n", title = "idxs")
}

# plot pairs of 11 parameters: d2_1, d2_2, d2_3, d2_4, bstar, beta_d, deltaDoffset, h_3, h_4, sd, alpha 
pairs(res2$par[idxs,], col = err_pal[res2$err_best[idxs]], pch = err_pch[res2$err_best[idxs]])

# print cases, etc against ranks for visual inspection
cbind(idxs, res2$value[idxs,], res2$err_ranks[idxs,], rowSums(res2$err_ranks)[idxs])
cbind(idx = idxs, round(res2$par[idxs,], digits = 3))



# Four plots of fit over projection period, all age groups combined ####
pdf("out/calibration_all100.pdf", width = 8, height = 8)
par(mfrow = c(2,2), mar=0.1 + c(4, 4, 1, 1))

# 1. cases
plot_param_sets(all_out$doy, all_out$cases[,idxs], yday(kc_data$date), kc_data$cases, 
                y_lab = "cases", col_pal = rainbow(length(idxs)), col_idx = 1:length(idxs))
err_legend("topleft")

# 2. peak
plot_param_sets(all_out$doy[-1], apply(all_out$cases[,idxs], 2, diff), rep(peak_daily_cases, 200), 1:200, 
                y_lab = "new cases", col_pal = rainbow(length(idxs)), col_idx = 1:length(idxs))

# 3. deaths
plot_param_sets(all_out$doy, all_out$deaths[,idxs], yday(kc_data$date), kc_data$deaths, 
                y_lab = "deaths", col_pal = rainbow(length(idxs)), col_idx = 1:length(idxs))
# 4. hospitalizations
plot_param_sets(all_out$doy, all_out$hosp[,idxs], yday(hosp_data$date), hosp_data$Total, 
                y_lab = "hospital", col_pal = rainbow(length(idxs)), col_idx = 1:length(idxs), y_lim = c(0, 5 * max(hosp_data$Total)))
legend("topleft", legend = NA, title = "NOT fit, for comparison", bty = "n")
dev.off()

# Eight plots of fit over projection period: cases and deaths by age group for top 25 parameter sets ####
# % of total cases and deaths in each age group
# Should also plot absolute values to see the relative impacts of social distancing measures within each group
pdf("out/calibration_all100_age.pdf", width = 8, height = 16)
par(mfcol = c(4,2), mar=0.1 + c(4, 4, 1, 1))

plot_param_sets(all_out$doy, all_out$cases_1[,idxs] / all_out$cases[,idxs], kc_age_t$cases$doy, kc_age_t$cases$age1 / rowSums(kc_age_t$cases[,-1]), 
                y_lab = "age 1 cases", col_pal = rainbow(length(idxs)), col_idx = 1:length(idxs), y_lim = c(0, 1))
plot_param_sets(all_out$doy, all_out$cases_2[,idxs] / all_out$cases[,idxs], kc_age_t$cases$doy, kc_age_t$cases$age2 / rowSums(kc_age_t$cases[,-1]), 
                y_lab = "age 2 cases", col_pal = rainbow(length(idxs)), col_idx = 1:length(idxs), y_lim = c(0, 1))
plot_param_sets(all_out$doy, all_out$cases_3[,idxs] / all_out$cases[,idxs], kc_age_t$cases$doy, kc_age_t$cases$age3 / rowSums(kc_age_t$cases[,-1]), 
                y_lab = "age 3 cases", col_pal = rainbow(length(idxs)), col_idx = 1:length(idxs), y_lim = c(0, 1))
plot_param_sets(all_out$doy, all_out$cases_4[,idxs] / all_out$cases[,idxs], kc_age_t$cases$doy, kc_age_t$cases$age4 / rowSums(kc_age_t$cases[,-1]), 
                y_lab = "age 4 cases", col_pal = rainbow(length(idxs)), col_idx = 1:length(idxs), y_lim = c(0, 1))

plot_param_sets(all_out$doy, all_out$deaths_1[,idxs] / all_out$deaths[,idxs], kc_age_t$deaths$doy, kc_age_t$deaths$age1 / rowSums(kc_age_t$deaths[,-1]), 
                y_lab = "age 1 deaths", col_pal = rainbow(length(idxs)), col_idx = 1:length(idxs), y_lim = c(0, 1))
err_legend("topleft")
plot_param_sets(all_out$doy, all_out$deaths_2[,idxs] / all_out$deaths[,idxs], kc_age_t$deaths$doy, kc_age_t$deaths$age2 / rowSums(kc_age_t$deaths[,-1]), 
                y_lab = "age 2 deaths", col_pal = rainbow(length(idxs)), col_idx = 1:length(idxs), y_lim = c(0, 1))
plot_param_sets(all_out$doy, all_out$deaths_3[,idxs] / all_out$deaths[,idxs], kc_age_t$deaths$doy, kc_age_t$deaths$age3 / rowSums(kc_age_t$deaths[,-1]), 
                y_lab = "age 3 deaths", col_pal = rainbow(length(idxs)), col_idx = 1:length(idxs), y_lim = c(0, 1))
plot_param_sets(all_out$doy, all_out$deaths_4[,idxs] / all_out$deaths[,idxs], kc_age_t$deaths$doy, kc_age_t$deaths$age4 / rowSums(kc_age_t$deaths[,-1]), 
                y_lab = "age 4 deaths", col_pal = rainbow(length(idxs)), col_idx = 1:length(idxs), y_lim = c(0, 1))

dev.off()


# alternative coloring to plot those that meet compromise criteria
criteria_pal = alpha(c("gray40", "blue"), c(0.7, 0.9)) 
criteria_idx = (1:nrow(res2$par) %in% idxs) + 1
  
pdf("out/calibration_select_criteria.pdf", width = 8, height = 8)
par(mfrow = c(2,2), mar=0.1 + c(4, 4, 1, 1))

plot_param_sets(all_out$doy, all_out$cases, yday(kc_data$date), kc_data$cases, 
                y_lab = "cases", col_pal = criteria_pal, col_idx = criteria_idx, y_lim = c(0, 3 * max(kc_data$cases)))
plot_param_sets(all_out$doy[-1], apply(all_out$cases, 2, diff), rep(peak_daily_cases, 200), 1:200, 
                y_lab = "new cases", col_pal = criteria_pal, col_idx = criteria_idx, y_lim = c(0, 3 * max(diff(kc_data$cases))))
plot_param_sets(all_out$doy, all_out$deaths, yday(kc_data$date), kc_data$deaths, 
                y_lab = "deaths", col_pal = criteria_pal, col_idx = criteria_idx, y_lim = c(0, 3 * max(kc_data$deaths)))
plot_param_sets(all_out$doy, all_out$hosp, yday(hosp_data$date), hosp_data$Total, 
                y_lab = "hospital", col_pal = criteria_pal, col_idx = criteria_idx, y_lim = c(0, 5 * max(hosp_data$Total)))
legend("topleft", legend = NA, title = "NOT fit, for comparison", bty = "n")
dev.off()

pdf("out/calibration_select_criteria_age.pdf", width = 8, height = 16)
par(mfcol = c(4,2), mar=0.1 + c(4, 4, 1, 1))

plot_param_sets(all_out$doy, all_out$cases_1 / all_out$cases[,idxs], kc_age_t$cases$doy, kc_age_t$cases$age1 / rowSums(kc_age_t$cases[,-1]), 
                y_lab = "age 1 cases", col_pal = criteria_pal, col_idx = criteria_idx, y_lim = c(0, 1))
plot_param_sets(all_out$doy, all_out$cases_2 / all_out$cases[,idxs], kc_age_t$cases$doy, kc_age_t$cases$age2 / rowSums(kc_age_t$cases[,-1]), 
                y_lab = "age 2 cases", col_pal = criteria_pal, col_idx = criteria_idx, y_lim = c(0, 1))
plot_param_sets(all_out$doy, all_out$cases_3 / all_out$cases[,idxs], kc_age_t$cases$doy, kc_age_t$cases$age3 / rowSums(kc_age_t$cases[,-1]), 
                y_lab = "age 3 cases", col_pal = criteria_pal, col_idx = criteria_idx, y_lim = c(0, 1))
plot_param_sets(all_out$doy, all_out$cases_4 / all_out$cases[,idxs], kc_age_t$cases$doy, kc_age_t$cases$age4 / rowSums(kc_age_t$cases[,-1]), 
                y_lab = "age 4 cases", col_pal = criteria_pal, col_idx = criteria_idx, y_lim = c(0, 1))

plot_param_sets(all_out$doy, all_out$deaths_1 / all_out$deaths[,idxs], kc_age_t$deaths$doy, kc_age_t$deaths$age1 / rowSums(kc_age_t$deaths[,-1]), 
                y_lab = "age 1 deaths", col_pal = criteria_pal, col_idx = criteria_idx, y_lim = c(0, 1))
plot_param_sets(all_out$doy, all_out$deaths_2 / all_out$deaths[,idxs], kc_age_t$deaths$doy, kc_age_t$deaths$age2 / rowSums(kc_age_t$deaths[,-1]), 
                y_lab = "age 2 deaths", col_pal = criteria_pal, col_idx = criteria_idx, y_lim = c(0, 1))
plot_param_sets(all_out$doy, all_out$deaths_3 / all_out$deaths[,idxs], kc_age_t$deaths$doy, kc_age_t$deaths$age3 / rowSums(kc_age_t$deaths[,-1]), 
                y_lab = "age 3 deaths", col_pal = criteria_pal, col_idx = criteria_idx, y_lim = c(0, 1))
plot_param_sets(all_out$doy, all_out$deaths_4 / all_out$deaths[,idxs], kc_age_t$deaths$doy, kc_age_t$deaths$age4 / rowSums(kc_age_t$deaths[,-1]), 
                y_lab = "age 4 deaths", col_pal = criteria_pal, col_idx = criteria_idx, y_lim = c(0, 1))

dev.off()

pdf("out/param_set_error_select_criteria.pdf", width = 10, height = 10)
pairs(res2$value, col = criteria_pal[criteria_idx])
dev.off()

pdf("out/param_set_select_criteria.pdf", width = 10, height = 10)
pairs(res2$par, col = criteria_pal[criteria_idx])
dev.off()

# write out parameter sets that met criteria

write.csv(res2$par[idxs,], "out/parameter_select_criteria.csv", row.names = FALSE, quote = FALSE)

