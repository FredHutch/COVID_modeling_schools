#' Gets temporal parameters
#'
#' This is a helper function since the optimizer just uses an unnamed vector of parameters.
#' We pass an additional vector of the names of the parameters, that can be used to determine
#' what parameters are in the pars vector and this overwrite the correct parameter in the 
#' parameter list pars_base.
#' 
#' Importantly, temporal parameters must start with t to indicate they are temporal parameters
#' and include the date as well as the parameter name in pars_names. For example, t-2020.05.01-rho_A
#' indicates to update the temporal parameters list for 1 May 2020 for rho_A. Note that the dates 
#' don't use dashes internally, since dashed are used to demarcate the compnents of the parameter name.
#' 
#' For age-varying parameters, because we can only pass a single vector to the optimizer,
#' these are passed as t-date-paramname_age, for example t-2020.05.01-sd_1, 
#' which is parsed and the appropriate parameter in the vector is updated for the specified date.

#' @param pars Vector of parameter, e.g. from the optimizer
#' @param pars_names Names of the parameter in pars in the form t-date-paramname 
#' @param pars_temporal_base Base temporal parameter values, to be updated with those in pars
#'
#' @return Updated list of temporal parameters suitable for calling model_eq
#' @export
get_temporal_params = function(pars, pars_names, pars_temporal_base)
{
  stopifnot(length(pars) == length(pars_names))
  if (is.data.frame(pars))
  {
    # can happen when passing a row of a data fram but this messes up setting parameters later
    stopifnot(nrow(pars) == 1) # should just be 1 row
    pars = unlist(pars)
  }
  
  if (length(pars) > 0)
  {
    for (i in 1:length(pars))
    {
      param_name = pars_names[i]
      param_value = pars[i]
      
      # check for temporal parameters
      if (startsWith(param_name, "t-"))
      {
        # parse name into components based on -, this means dates need to not use - within
        param_components = strsplit(param_name, "-", fixed = TRUE)[[1]] # list length of char vector, we only have 1
        param_date = lubridate::ymd(param_components[2]) # first is t
        param_name = param_components[3]
        
        # check if there is already a list for the date or create one
        dates = do.call(c, lapply(pars_temporal_base, function(x) x$date))
        
        if (param_date %in% dates)
        {
          # just need to exit the existing list directly
          list_idx = which(param_date == dates)[1] # get first in case of multiple
        } else
        {
          # create a new list
          new_list = list(date = param_date)
          pars_temporal_base = append(pars_temporal_base, list(new_list))
          list_idx = length(pars_temporal_base)
        }
          
        # age-varying parameters are paramname_age (sd_1) so check parameter before last underscore
        last_ = max(unlist(gregexpr(pattern = '_', param_name)))
        if (is_age_specfic_param(substr(param_name, 1, last_ - 1)))
        {
          # look up and replace ith part of the vector, this allows setting multiple age groups to the same value
          param_name_base = substr(param_name, 1, last_ - 1)
          
          for (a in 1:(nchar(param_name) - last_))
          {
            age = as.numeric(substr(param_name, last_ + a, last_ + a))
            pars_temporal_base[[list_idx]][[ param_name_base ]][age] = param_value
          }
        }
        else
        {
          pars_temporal_base[[list_idx]][[ param_name ]] = param_value
        }
      }
    }
  }
  return(pars_temporal_base)
}
