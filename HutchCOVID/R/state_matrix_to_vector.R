#' Convert state matrix to vector
#'
#' Utility function for manipulating states
#'
#' @param state_matrix state in matrix format
#'
#' @return state vector
#' @importFrom tidyr unite
#' @export
state_matrix_to_vector = function(state_matrix)
{
  # use hyphen to separate state/strain/vac/age, note _ already used in cum state names
  used.columns = c("state", "strain", "vaccine", "age")
  full_names = unlist(unite(state_matrix[, used.columns], name, state, strain, vaccine, age, sep = "-"))
  
  states = state_matrix$value
  names(states) = full_names
  return(states)
}
