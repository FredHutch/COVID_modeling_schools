#' Convert state vector to list
#'
#' Utility function for manipulating states
#' 
#' @param state_vector state in vector format
#'
#' @return list
#' @export
state_vector_to_list = function(state_vector){
  state_matrix = state_vector_to_matrix(state_vector)
  lapply(split(state_matrix$value, state_matrix$state), c)
}
