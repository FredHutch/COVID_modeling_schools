#' Create Y matrix for recovered states by vaccination status
#'
#' Recovered from infection goes into either recovered non vaccinated, or according to vaccine, but not by strain
#' 
#' @param n_age number of age groups
#' @param n_vaccine number of vaccines
#' @param n_strain number of strains
#'
#' @return Y matrix
#' @export
create_Y_recovered_by_vax = function(n_age, n_vaccine, n_strain)
{
  # first need n_vaccine rows of 0's, since we are only adding to the recovered part of the S vector
  Y_top = matrix(0, nrow = n_age * n_vaccine, ncol = n_age * n_vaccine * n_strain * 2)
  
  # for the bottom need a block of i_mat that is also I which is n_vaccine x n_vaccine, 
  # this is repeated horizontally n_strain * 2 times (the 2 is for the non-recovered and recovered halves of the vector)
  i_mat = diag(n_age)
  zero_mat = matrix(0, nrow = n_age, ncol = n_age)
  
  i_block = diag(n_age * n_vaccine)
  Y_bottom = repmat(i_block, 1, 2 * n_strain)
  
  Y = rbind(Y_top, Y_bottom)
  return(Y)
}
