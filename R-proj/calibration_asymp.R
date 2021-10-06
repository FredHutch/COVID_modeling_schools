# do across values of p and beta_a

base_params = params_fix
p_vals = c(0.9, 0.7, 0.5)
beta_a_vals = c(0.1, 0.25, 0.5)

for (i in 1:length(p_vals))
{
  for (j in 1:length(beta_a_vals))
  {
    params_to_opt = get_params(c(p_vals[i], beta_a_vals[j]), c("p", "beta_a"), base_params)
    res3 = nsga2(fn = calc_sse_multi,        # determines what to minimize
                 idim = length(params_kc),   # number of parameters to optimize
                 odim = 5,                   # number of critria to optimize
                 popsize = 100,
                 generations = 100,
                 names(params_kc), params_to_opt, kc_data, kc_age_t, peak_daily_cases,
                 lower.bounds = params_kc_lower,
                 upper.bounds = params_kc_upper)
    
    res3$err_ranks = apply(res3$value, 2, rank)
    res3_out = get_model_data_param_sets(res3$par, names(params_kc), params_to_opt, 300)
    
    save(res3, file = paste0("out/res3_i", i, "_j", j, ".Rdata"))
    save(res3_out, file = paste0("out/res3_out_i", i, "_j", j, ".Rdata"))
    print(paste("i ", i, ", j ", j))
  }
}

load("out/res3_i1_j1.Rdata")
load("out/res3_out_i1_j1.Rdata")
cur_pars = get_params(c(p_vals[1], beta_a_vals[1]), c("p", "beta_a"), base_params)

load("out/res3_i1_j2.Rdata")
load("out/res3_out_i1_j2.Rdata")
cur_pars = get_params(c(p_vals[1], beta_a_vals[2]), c("p", "beta_a"), base_params)

load("out/res3_i1_j3.Rdata")
load("out/res3_out_i1_j3.Rdata")
cur_pars = get_params(c(p_vals[1], beta_a_vals[3]), c("p", "beta_a"), base_params)

load("out/res3_i2_j1.Rdata")
load("out/res3_out_i2_j1.Rdata")
cur_pars = get_params(c(p_vals[2], beta_a_vals[1]), c("p", "beta_a"), base_params)

load("out/res3_i2_j2.Rdata")
load("out/res3_out_i2_j2.Rdata")
cur_pars = get_params(c(p_vals[2], beta_a_vals[2]), c("p", "beta_a"), base_params)

load("out/res3_i2_j3.Rdata")
load("out/res3_out_i2_j3.Rdata")
cur_pars = get_params(c(p_vals[2], beta_a_vals[3]), c("p", "beta_a"), base_params)

load("out/res3_i3_j1.Rdata")
load("out/res3_out_i3_j1.Rdata")
cur_pars = get_params(c(p_vals[3], beta_a_vals[1]), c("p", "beta_a"), base_params)

load("out/res3_i3_j2.Rdata")
load("out/res3_out_i3_j2.Rdata")
cur_pars = get_params(c(p_vals[3], beta_a_vals[2]), c("p", "beta_a"), base_params)

load("out/res3_i3_j3.Rdata")
load("out/res3_out_i3_j3.Rdata")
cur_pars = get_params(c(p_vals[3], beta_a_vals[3]), c("p", "beta_a"), base_params)

# look at results for each combination in turn

# do any solutions meet compromise criteria for cases, peak and deaths
idxs = which(res3$value[,1] < 1000000000 & 
               res3$value[,2] < 1000000 & 
               res3$value[,3] < 5000 &
               res3$value[,4] < 3000 &
               res3$value[,5] <= 169 )

# alternatively, the best for cases and deaths
idxs = which(res3$err_ranks[,1] <= 0.25 * nrow(res3$err_ranks) & 
               res3$err_ranks[,2] <= 0.25 * nrow(res3$err_ranks)) # good cases and deaths

idxs=c(3, 5, 36)

cbind(idxs, res3$value[idxs,], res3$err_ranks[idxs,], rowSums(res3$err_ranks)[idxs])


plot_calibration_params(res3$par[36,], names(params_kc), cur_pars, kc_data, kc_age_t, 200, state)

plot_param_sets(res3_out$doy[1:150], res3_out$cases[1:150,idxs], yday(kc_data$date), kc_data$cases, 
                y_lab = "cases", col_pal = rainbow(length(idxs)), col_idx = 1:length(idxs))
plot_param_sets(res3_out$doy[2:150], apply(res3_out$cases[1:150,idxs], 2, diff), rep(peak_daily_cases, 200), 1:200, 
                y_lab = "new cases", col_pal = rainbow(length(idxs)), col_idx = 1:length(idxs))
plot_param_sets(res3_out$doy[1:150], res3_out$deaths[1:150,idxs], yday(kc_data$date), kc_data$deaths, 
                y_lab = "deaths", col_pal = rainbow(length(idxs)), col_idx = 1:length(idxs))


# keep track of chosen "best fit" parameters
params_asym_set = matrix(c(0.9, 0.1, 0.093786547, 0.022083369, 0.003497344, 0.072390959, 0.240000889, 0.616285141, 40.896379747, 0.150684408, 0.164999649, 0.898071033, 1.049200717,
                           0.9, 0.25, 0.006624465, 0.008597697, 0.004116801, 0.038605738, 0.240003698, 0.591365482, 40.958964477, 0.178496978, 0.150866528, 0.768154968, 1.030299428,
                           0.9, 0.5, 0.002473805, 0.001004184, 0.001020674, 0.048355716, 0.240152407, 0.669579878, 40.392115397, 0.151402994, 0.150909577, 0.831412170, 1.139831091,
                           0.7, 0.1, 0.002064075, 0.049977809, 0.028518731, 0.062491569, 0.256675403, 0.552807259, 49.973967823, 0.150323806, 0.349398530, 0.671571088, 1.625461425,
                           0.7, 0.25, 0.029862144, 0.028508018, 0.006896715, 0.065066175, 0.275809873, 0.634824152, 40.302506521, 0.170751040, 0.274540589, 0.676925880, 1.364853996,
                           0.7, 0.5, 0.01660480, 0.09035768, 0.02374073, 0.08092494, 0.24128351, 0.51642550, 46.35980795, 0.30583103, 0.33860739, 0.63825300, 1.67701125,
                           0.5, 0.1, 0.01370789, 0.03538396, 0.02389700, 0.01025474, 0.38762830, 0.51862224, 42.54123123, 0.18387094, 0.28692134, 0.64571897, 1.61333015,
                           0.5, 0.25, 0.019136518, 0.026279117, 0.001594542, 0.064457391, 0.364347200, 0.592452668, 43.322761360, 0.150612107, 0.191374379, 0.589234047, 1.234026312,
                           0.5, 0.5, 0.03666536, 0.03051598, 0.03331321, 0.03850026, 0.32817003, 0.56246459, 42.17276966, 0.15290253, 0.34954095, 0.63191655, 1.75505997 ),
                         byrow = TRUE, nrow = 9)
colnames(params_asym_set) = c("p", "beta_a", "d2_1", "d2_2", "d2_3", "d2_4", "bstar", "beta_d", "delta0offset", "h_3", "h_4", "sd", "alpha")

write.csv(params_asym_set, file = "out/asyptomatic_calibration.csv", quote = FALSE, row.names = FALSE)

# now plot results

asymp_out = get_model_data_param_sets(params_asym_set, colnames(params_asym_set), base_params, 200)

#jpeg("out/asymp_param_matrix.jpeg", width = 750, height = 750)
pdf("out/asymp_param_matrix.pdf", width = 8, height = 8)
par(mfrow = c(2,2), mar = 0.1 + c(3, 3, 1, 1), mgp = c(2, 0.5, 0))

cols = c(paletteer_d("RColorBrewer::Set2", 3))
cols_idx = rep(1:3, each = 3)
ltys = 1:3
ltys_idx = rep(1:3, times = 3)

plot_asym(asymp_out$doy, asymp_out$cases, yday(kc_data$date), kc_data$cases, 
          y_lab = "diagnosed cases", col_pal = cols, col_idx = cols_idx, lty_pal = ltys, lty_idx = ltys_idx)
legend("topleft", 
       legend = paste("p", params_asym_set[,1], "beta_a", params_asym_set[,2]), 
       col = cols[cols_idx], lty = ltys[ltys_idx], lwd = 2, bty = "n" )

plot_asym(asymp_out$doy, asymp_out$deaths, yday(kc_data$date), kc_data$deaths, 
          y_lab = "deaths", col_pal = cols, col_idx = cols_idx, lty_pal = ltys, lty_idx = ltys_idx)
points(c(base_params$delta1_doy, base_params$delta2_doy, base_params$delta3_doy, base_params$delta4_doy), rep(0, 4),
       pch = 1:4)
legend("topleft", legend = paste0("delta", 1:4), pch = 1:4, bty = "n")

plot_asym(asymp_out$doy, asymp_out$symp + asymp_out$asymp, NA, NA, 
          y_lab = "symp + asymp infections", col_pal = cols, col_idx = cols_idx, lty_pal = ltys, lty_idx = ltys_idx)
for (i in 1:ncol(asymp_out$asymp)) 
{
  lines(x = asymp_out$doy, y = asymp_out$asymp[,i], lty = ltys[ltys_idx[i]], lwd = 0.5, col = cols[cols_idx[i]])
}
legend("topleft", legend = c("symp + asymp", "asymp"), lwd = c(1.5, 0.5), bty = "n" )

plot_asym(asymp_out$doy, asymp_out$hosp, NA, NA, 
          y_lab = "current hospitalized", col_pal = cols, col_idx = cols_idx, lty_pal = ltys, lty_idx = ltys_idx)
dev.off()

####-----------------------------------------------------------------------------------

# also look at percent hospitalized
age_cols = paletteer_d("RColorBrewer::YlOrRd", 6)[-(1:2)]

par(mfrow = c(3,3))
for (i in 1:nrow(params_asym_set))
{
  out = get_model_data(c(params_asym_set[i,], params_asym_set[which(params_asym_set[i,] == "sd")]), 
                       c(colnames(params_asym_set), "sd2"),
                       base_params, 200, state)
  
  plot_percent_hosp(out, age_cols, legend = FALSE,
                    main = paste("p", params_asym_set[i,1], "beta_a", params_asym_set[i,2]))
}

