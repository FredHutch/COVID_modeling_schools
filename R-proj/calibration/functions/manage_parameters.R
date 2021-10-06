# creates a list object that holds a set of parameterizations
#'
#' @param params_calib 
#' @param params_names 
#' @param params_base 
#' @param params_temporal_base 
#'
#' @return
create_parameterset_list = function(params_calib, params_names, params_base, params_temporal_base)
{
  parameterset = list()
  parameterset_temporal = list()
  
  for (i in 1:nrow(params_calib))
  {
    new_params = get_params(params_calib[i,], params_names, params_base)
    new_params_temporal = get_temporal_params(params_calib[i,], params_names, params_temporal_base)
    
    parameterset[[i]] = new_params
    parameterset_temporal[[i]] = new_params_temporal
  }
  return(list(parameterset = parameterset, parameterset_temporal = parameterset_temporal))
}