#' Gets parameters
#' 
#' This is a helper function since the optimizer just uses an unnamed vector of parameters.
#' We pass an additional vector of the names of the parameters, that can be used to determine
#' what parameters are in the pars vector and this overwrite the correct parameter in the 
#' parameter list pars_base.
#' 
#' For age-varying parameters, because we can only pass a single vector to the optimizer,
#' these are passed as paramname_age, for example sd_1, which is parsed and the appropriate
#' parameter in the vector is updated.
#' 
#' With the vectorization of the model, it is important that get_params() is called before run_model(),
#' which will expand the age-specific parameters as necessary for strains and vaccines.
#'
#' @param pars Vector of parameter, e.g. from the optimizer
#' @param pars_names Names of the parameter in pars
#' @param pars_base Base parameter values, to be updated with those in pars
#'
#' @return List of parameters suitable for calling model_eq
#' @export
get_params = function(pars, pars_names, pars_base)
{
  # pars is a vector of parameters, need to convert these to list 
  # make the parameter setting more flexible by over-writing the base parameters,
  # so that it's easier to vary which ones are fit
  stopifnot(length(pars) == length(pars_names))
  
  if (length(pars) > 0)
  {
    for (i in 1:length(pars))
    {
      param_name = pars_names[i]
      param_value = pars[i]
      
      # skip temporal parameters
      if (!startsWith(param_name, "t-"))
      {
        # this needs to be an integer number of days
        if (param_name == "first_inf_day")
        {
          param_value = floor(param_value)
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
            pars_base[[ param_name_base ]][age] = param_value
          }
        }
        else
        {
          pars_base[[ param_name ]] = param_value
        }
      }
    }
  }
  return(pars_base)
}
