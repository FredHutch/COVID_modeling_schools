#' sd constraints for calibration
#' 
#' Generates constraints for calibration to enforce the sd parameters to be in alternating order, applied by 
#' age group. This way sd for age groups moves in the same direction and matches the Reff curve in terms of 
#' whether sd should be increasing or decreasing compared to the previous period
#'
#' @param calib_param_names the names of the parameters being calibrated (in the same order as the priors)
#' @param  constraint_order a vector of strings "<" and ">" for the constraints, otherwise assumed to alternate ">" and "<"
#'
#' @return string that can be used for prior_test in EasyABC methods
generate_sd_constraints = function(calib_param_names, constraint_order = NULL)
{
  # constraints must be of the form X1 > X2 where numbering is in the same order as the prior definition
  
  # Assumptions:
  # * sd parameters are defined in temporal order
  # * the first sd parameter is by definition and increase from 0, then they alternate
  #    i.e. SD1 > SD2 & SD2 < SD3
  # * we enforce these per age-group sd, thus the age groups' sd must move in the same direction
  
  prior_test = ""
  
  sd_idxs = grep("sd_", calib_param_names)
  last_idxs = sapply(calib_param_names[sd_idxs], function(x) max(unlist(gregexpr(pattern = '_', x))))
  sd_suffixs = sapply(1:length(sd_idxs), function(x) substr(calib_param_names[sd_idxs[x]], last_idxs[x] +1, nchar(calib_param_names[sd_idxs[x]])))
  sd_suffixs = unique(sd_suffixs)

  for (age in 1:length(sd_suffixs))
  {
    sd_params = grep(paste0("sd_", sd_suffixs[age]), calib_param_names)
    
    # not all age groups might be in each time period
    if (length(sd_params) > 0)
    {
      if (!is.null(constraint_order))
      {
        stopifnot(length(constraint_order) == (length(sd_params) - 1))
      } else
      {
        constraint_order = rep( c(">", "<"), length = length(sd_params))
      }
      
      for (i in 1:(length(sd_params) - 1))
      {
        prior_test = paste(prior_test, '&', paste0('X', sd_params[i]), constraint_order[i], paste0('X', sd_params[i + 1]))
      }
    }
  }
  # now remove starting & from comparison string
  prior_test = substr(prior_test, regexpr("X", prior_test), nchar(prior_test))
  
  return(prior_test)
}
