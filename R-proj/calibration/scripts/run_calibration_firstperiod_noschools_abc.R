# Run abc calibration

library(EasyABC)

outFile = "out_firstperiod_mcmc.rdata"

source("calibration/scripts/setup_calibration_firstperiod_noschools_abc.R")  


retry_wrapper = function(params)
{
  err_file = sprintf("~/error_%d.txt" , Sys.getpid()) # so that multiple processes don't write to the same file
  
  model_wrapper = function(params)
  {
    # the issue is that everything needs to be defined inside the function for it to be able to be run in a cluster environment
    source("calibration/scripts/setup_calibration_firstperiod_noschools_abc.R")  
    
    # this should be unnecessary since our model is deterministic
    set.seed(params[1])
    
    # drop first params which is seed when n_cluster > 1
    res  = calc_model_stats(params[-1], abc_param_names, params_calib_abc, params_temporal_calib_abc, state_abc, 
                            TRUE, dates = dates_abc, rescale_factors = data_means)
    return(res)
  }
  
  result = NULL
  attempt = 1
  while( is.null(result) & attempt <= 3 ) 
  {
    attempt = attempt + 1
    result = tryCatch(
      {
        model_wrapper(params)
      },
      error = function(err) 
      {
        cat(paste("Error", err), file = err_file, append = TRUE, sep = "\n")
        cat(paste(params, collapse = " "), file = err_file, append = TRUE, sep = "\n")
        return(NULL)
      },
      warning = function(warn) 
      {
        cat(paste("Warning", warn), file = err_file, append = TRUE, sep = "\n")
        cat(paste(params, collapse = " "), file = err_file, append = TRUE, sep = "\n")
        return(NULL)
      }
    )
  }
  
  # if result is still null, return all 0's
  if (is.null(result))
  {
    cat("Permenant failure for the following parameters, returning 0", file = err_file, append = TRUE, sep = "\n")
    cat(paste(params, collapse = " "), file = err_file, append = TRUE, sep = "\n")
    result = rep(0, length(summary_stats_abc))
  }

  return(result)
}

# 72 sec for 100 model runs through April 30 => 2 hrs for 10,000
# 70 sec for 100 model runs through Dec 31, 8 cores

# simplest but very inefficient
# right now using 0 for summary_stat_target since we have an objective function, reconsider this
proportion_retained = 0.01
abc_rej = ABC_rejection(model = retry_wrapper,
                        prior=params_priors,
                        nb_simul =  100000,
                        summary_stat_target = summary_stats_abc,
                        tol = proportion_retained,
                        n_cluster = 10,
                        use_seed = TRUE,
                        verbose = TRUE)
save(abc_rej, file = "out_firstperiod_noschools_rej.rdata")
# 
# here's how to do the ABC-MCMC 
# Here we're doing method Majoram to calculate automatically dist_max, tab_normalization, proposal_range

# abc_mcmc = ABC_mcmc(method = "Marjoram",
#                     model = retry_wrapper,
#                     prior=params_priors,
#                     n_rec = 5000, # sampled points
#                     n_between_sampling = 10,
#                     summary_stat_target = summary_stats_abc,
#                     n_calibration= 10000,
#                     tolerance_quantile= 0.01,
#                     proposal_phi = 1,
#                     n_cluster = 10,
#                     use_seed = TRUE,
#                     verbose = TRUE)
# 
# save(abc_mcmc, abc_param_names, file = outFile)
# 
# Error in filter(., state %in% c("cum_diag", "cum_exp", "cum_hosp", "cum_death",  : bad generic call environment
