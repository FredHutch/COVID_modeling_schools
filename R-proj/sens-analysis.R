library(sensitivity)
library(HutchCOVID)

base_params = c(params_fix, params_kc) # from globalparameters.R
calib_names = c("gamma_1", "gamma_2",
                "d2_1", "d2_2", "d2_3", "d2_4",
                "h_1", "h_2", "h_3", "h_4",
                "bstar", "beta_a", "beta_p", "beta_s", "beta_d",  
                "r_1", "r_2", "r_3", "alpha", "sd")
calib_lower = c(0.25, 0.25,
                0.05, 0.05, 0.05, 0.05,
                0.05, 0.05, 0.05, 0.05,
                0.2, 0.1, 2, 0.5, 0.5,  
                0.1, 0.05, 0.05, 1, 0.25 )
calib_upper = c(0.5, 0.5,
                0.4, 0.4, 0.4, 0.4,  
                0.4, 0.4, 0.4, 0.4,  
                0.5, 0.5, 3.5, 1, 1,  
                0.3, 0.25, 0.25, 5, 0.9)

write.csv(data.frame(calib_names, calib_lower, calib_upper), file = "out/sens_analy_bounds.csv", quote = FALSE, row.names = FALSE)

sa_times = seq(0, 150, by = 1) # roughly what we're calibrating to

ode_wrapper = function(pars)
{
  # this relies on having base_params and calib_names globally defined outside the function
  params = base_params
  for (i in 1:length(pars))
  {
    param_name = calib_names[i]
    params[[ param_name ]] = pars[i]
  }

#  print(params)
  out = ode(y = state, times = sa_times, func = model_eq, parms = params)
  
  out_day = 90
  
  deaths = sum(out[out_day, c("F_i1", "F_i2", "F_i3", "F_i4")]) # final deaths
  cases = sum(out[out_day, c("cum_diag_i1", "cum_diag_i2", "cum_diag_i3", "cum_diag_i4")])# final diag
  peak = which.max(diff(rowSums(out[, c("cum_diag_i1", "cum_diag_i2", "cum_diag_i3", "cum_diag_i4")])))
  c_age1 = out[out_day, "cum_diag_i1"] / cases
  c_age4 = out[out_day, "cum_diag_i4"] / cases
  d_age3 = out[out_day, "F_i3"] / deaths
  d_age4 = out[out_day, "F_i4"] / deaths
  
  results = c(deaths = deaths, cases = cases, peak = peak, 
              case_prop_1 = c_age1, death_prop_3 = d_age3, 
              case_prop_4 = c_age4, death_prop_4 = d_age4)
  names(results)[4:7] = c("case_prop_1", "death_prop_3", "case_prop_4", "death_prop_4")
  
  return(results) 
}

# test
model_eq(100, state + 100, base_params)
ode_wrapper(calib_lower)
ode_wrapper(calib_upper)


sa_plan = morris(model = NULL, factors = calib_names, r = 100,
            design = list(type = "oat", levels = 8, grid.jump = 2),
            binf = calib_lower, bsup = calib_upper)

mod_res = apply(sa_plan$X, 1, ode_wrapper)

sa_res = tell(sa_plan, t(mod_res))

# calculate metrics from elementary effects (from morris() help)
mu <- apply(sa_res$ee, 3, function(M){
  apply(M, 2, mean)
})
mu.star <- apply(abs(sa_res$ee), 3, function(M){
  apply(M, 2, mean)
})
sigma <- apply(sa_res$ee, 3, function(M){
  apply(M, 2, sd)
})

plot_mu_sig = function(mu.star, sigma, column, title)
{
  plot(mu.star[,column], sigma[,column], main = title, 
       xlab = "mu.star (main effect)", ylab = "sigma (nonlinear/interactions)")
  text(mu.star[,column], sigma[,column], labels = row.names(mu.star), pos = c(2,4))
}

plot_output_vs_param = function(sa_params, sa_outputs, param, output, expected)
{
  plot(sa_params[,param], sa_outputs[,output], 
       ylim = range(c(sa_outputs[,output], expected)),
      # col = rainbow(8)[as.numeric(as.factor(sa_params$X[,param]))], 
       xlab = param, ylab = output)
  abline(h = expected, col = "red")
}

pdf("out/sens_analy_calibration.pdf", width = 15, height = 5)
par(mfrow = c(1,3), mar=0.1 + c(3, 3, 1, 1))

plot_mu_sig(mu.star, sigma, 1, "Deaths")
plot_mu_sig(mu.star, sigma, 2, "Cases")
plot_mu_sig(mu.star, sigma, 3, "Peak")

dev.off()

pdf("out/sens_analy_calibration_age.pdf", width = 10, height = 10)
par(mfrow = c(2,2), mar=0.1 + c(3, 3, 1, 1))

plot_mu_sig(mu.star, sigma, 4, "Cases - proportion age 1")
plot_mu_sig(mu.star, sigma, 5, "Deaths - proportion age 3")
plot_mu_sig(mu.star, sigma, 6, "Cases - proportion age 4")
plot_mu_sig(mu.star, sigma, 7, "Deaths - proportion age 4")

dev.off()

# expected, gather here the data for each output with 45 day offset
expected = c(deaths = 300, cases = 4700, peak = peak_daily_cases - 14, 
             case_prop_1 = 0.02, death_prop_3 = 0.15, case_prop_4 = 0.22, death_prop_4 = 0.82)

pdf("out/sens_analy_output_vs_param.pdf", width = 20, height = 20)
par(mfrow = c(10,7), mar=0.1 + c(3, 3, 1, 1), mgp = c(2, 0.5, 0))

for (p in colnames(sa_plan$X))
{
  for (o in colnames(t(mod_res)))
  {
    plot_output_vs_param(sa_plan$X, t(mod_res), p, o, expected[o])
  }
}
  
dev.off() 
  


write.csv( apply(mu.star, 2, rank), file = "out/sens_analy_mu.star_rank.csv", quote = FALSE)
