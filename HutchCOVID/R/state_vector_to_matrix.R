#' Convert state vector to matrix
#' 
#' Utility function for manipulating states
#'
#' @param state_vector state in vector format
#'
#' @return matrix
#' @export
state_vector_to_matrix = function(state_vector)
{
  #Reverse unite command with split
  state_matrix = Reduce('rbind', strsplit(names(state_vector), split = "-", fixed = T))
  names(state_matrix) = c("state", "strain","vaccine", "age")
  state_matrix = as_tibble(state_matrix)
  
  names(state_matrix) = c("state", "strain","vaccine", "age")
  state_matrix$value = as.numeric(state_vector)
  return(state_matrix)
}
