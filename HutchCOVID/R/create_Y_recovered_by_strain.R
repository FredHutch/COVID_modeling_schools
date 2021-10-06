#' Create Y matrix for recovered compartment for each strain
#' 
#' # recovery from infection goes into susceptible for appropriate strain
#'
#' @param n_age number of age groups
#' @param n_vaccine number of vaccines
#' @param n_strain number of strains
#'
#' @return Y matrix
#' @export
create_Y_recovered_by_strain = function(n_age, n_vaccine, n_strain)
{
  n_vaccine_eff = n_vaccine + n_strain 

  # first need n_vaccine rows of 0's, since we are only adding to the recovered part of the S vector
  Y_top = matrix(0, nrow = n_age * n_vaccine, ncol = n_age * n_strain * n_vaccine_eff)
  
  # now we need a diagonal matrix of blocks, where the blocks are 1 x n_vaccine_eff, so one row per strain
  # and the block dim is the number of strains
  i_mat = diag(n_age)
  zero_mat = matrix(0, nrow = n_age, ncol = n_age)
  
  i_block = repmat(i_mat, 1, n_vaccine_eff)
  zero_block = repmat(zero_mat, 1, n_vaccine_eff)
  
  Y_bottom = create_block_diag(i_block, zero_block, n_strain)
  
  Y = rbind(Y_top, Y_bottom)
  return(Y)
}
