#' Create Y matrix for a single recovered box
#'
#' Recovery from infection goes into susceptible for single recovered compartment no matter strain
#'
#' @param n_age number of age groups
#' @param n_vaccine number of vaccines
#' @param n_strain number of strains
#' @param maintain_vax_staus TRUE for recovered vaccinated to go to vaccnated compartment, FALSE to recovered compartment
#'
#' @return Y matrix
#' @export
create_Y_single_recovered = function(n_age, n_vaccine, n_strain, maintain_vax_staus = TRUE)
{
  n_vaccine_eff = n_vaccine + 1 # + 1 for recovered box 
  
  if (maintain_vax_staus)
  {
    # need blocks of n_vaccine_eff x n_vaccine_eff that are repeated n_strain times
    i_mat = diag(n_age * n_vaccine_eff)
    Y = repmat(i_mat, 1, n_strain)
  } else
  {  
    # first need n_vaccine rows of 0's, since we are only adding to the recovered part of the S vector
    Y_top = matrix(0, nrow = n_age * n_vaccine, ncol = n_age * n_strain * n_vaccine_eff)
    
    # now we need a row of identity matrices, this moves everything to a single recovered box in S
    i_mat = diag(n_age)
    Y_bottom = repmat(i_mat, 1, n_strain * n_vaccine_eff)
    
    Y = rbind(Y_top, Y_bottom)
  }
  return(Y)
}
