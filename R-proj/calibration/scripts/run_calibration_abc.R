# Run abc calibration

library(EasyABC)

outFile = "out_mcmc.rdata"

source("calibration/scripts/setup_calibration_abc.R")  


retry_wrapper = function(params)
{
  err_file = sprintf("~/error_%d.txt" , Sys.getpid()) # so that multiple processes don't write to the same file

  model_wrapper = function(params)
  {
    # the issue is that everything needs to be defined inside the function for it to be able to be run in a cluster environment
    source("calibration/scripts/setup_calibration_abc.R")  
    
    # this should be unnecessary since our model is deterministic
    set.seed(params[1])
    
    # drop first params which is seed when n_cluster > 1
    res  = calc_model_stats(params[-1], abc_param_names, params_calib_abc, params_temporal_calib_abc, state_abc, TRUE, dates = dates_abc, rescale_factors = data_means)
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
# 70 sec for 100 model runs through Dec 31, 8 cores => ~2 hrs for 8 cores but 15.5 hrs for 1 core

# simplest but very inefficient
# right now using 0 for summary_stat_target since we have an objective function, reconsider this
proportion_retained = 0.1
abc_rej = ABC_rejection(model = retry_wrapper,
                        prior = params_priors,
                        prior_test = param_constraint,
                        nb_simul = 100000,
                        summary_stat_target = summary_stats_abc,
                        tol = proportion_retained,
                        n_cluster = 10,
                        use_seed = TRUE,
                        verbose = TRUE)
save(abc_rej, abc_param_names, file = "out_rej_new_sd.rdata")

# here's how to do the ABC-MCMC 
# Here we're doing method Majoram to calculate automatically dist_max, tab_normalization, proposal_range

# abc_mcmc = ABC_mcmc(method = "Marjoram",
#                     model = retry_wrapper, 
#                     prior=params_priors, 
#                     n_rec = 10000, # sampled points
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

# pre-calculated from rejection sampling run
# proposal_range_simp = c(0.0007726929, 0.0958465206, 2.0793569575, 0.0602051749, 0.0498411795, 0.0590374927, 0.0591106535, 0.0304736699, 0.0288026171, 0.0281290859, 0.0306422962, 0.0454430187,
#                         0.0446558267, 0.0439992793, 0.0419371731, 0.0215807430, 0.0200880348, 0.0214489561, 0.0228650336, 0.0284375894, 0.0293295577, 0.0288895102, 0.0281622098, 0.0301918697,
#                         0.0286632048, 0.0288252936, 0.0324588943, 0.0434683112, 0.0411741580, 0.0429516048, 0.0419997368, 0.1048517506, 0.0980563806, 0.1026186190, 0.0934856348, 0.0965950841,
#                         0.0923800741, 0.1005721840, 0.1037204986, 0.0959666033, 0.0540028614, 0.1016241845, 0.1048165101, 0.1010356367, 0.0446818691, 0.0991567203, 0.1030968834, 0.1005100789,
#                         0.0760007322, 0.0969963084, 0.0991587968)
# 
# abc_mcmc = ABC_mcmc(method = "Marjoram_original",
#                     model = retry_wrapper,
#                     prior=params_priors,
#                     n_rec = 2000, # sampled points
#                     n_between_sampling = 10,
#                     summary_stat_target = summary_stats_abc,
#                     dist_max = 19.3,
#                     tab_normalization = rep(1, length(summary_stats_abc)),
#                     proposal_range = proposal_range_simp,
#                     use_seed = TRUE,
#                     verbose = TRUE)
# 
# save(abc_mcmc, abc_param_names, file = outFile)

# Error in filter(., state %in% c("cum_diag", "cum_exp", "cum_hosp", "cum_death",  : bad generic call environment

# In rbind(tab_simul_summarystat, as.numeric(list_simul_summarystat[[i]])) : number of columns of result is not a multiple of vector length (arg 2)

