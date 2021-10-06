#' Create V matrix for a single recovered box
#'
#' Recovery from infection goes into susceptible for single recovered compartment no matter strain
#'
#' @param n_age number of age groups
#' @param n_vaccine number of vaccines
#' @param vac_prop how to proportionally distribute vaccines
#'
#' @return Y matrix
#' @export
create_V_single_recovered = function(n_age, n_vaccine, vac_prop = rep(1 / (n_vaccine - 1), n_vaccine - 1))
{
  n_vaccine_eff = n_vaccine + 1 # + 1 for recovered box 
  
  # by columns, first column gores from S to vax compartments, last column goes from R to vax compartments, 0's elsewhere
  i_mat = diag(n_age)
  zero_mat = matrix(0, nrow = n_age * n_vaccine_eff, ncol = n_age * (n_vaccine - 1))
  
  # this is the vaccine distribution half column with -1 for susceptible and vac_prop for each vaccine
  vac_prop_S = c(rep(-1, n_age), rep(vac_prop, each = n_age), rep(0, n_age))
  dist_mat_S = vac_prop_S * repmat(i_mat, n_vaccine_eff, 1)

  vac_prop_R = c(rep(0, n_age), rep(vac_prop, each = n_age), rep(-1, n_age))
  dist_mat_R = vac_prop_R * repmat(i_mat, n_vaccine_eff, 1)
  
  V = cbind(dist_mat_S, zero_mat, dist_mat_R)
  return(V)
}
