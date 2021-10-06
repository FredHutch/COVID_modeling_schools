#' extracts the final state and the end of model output
#' 
#' suitable to using to start the next model run
#' (i.e. with out the time or sd columns)
#'
#' @param model_out matrix model output
#'
#' @return state vector
#' @export
extract_final_state = function(model_out)
{
  final_state = extract_state(model_out, nrow(model_out))
  return(final_state)
}