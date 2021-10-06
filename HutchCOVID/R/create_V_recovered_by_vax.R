#' Create V matrix for recovered states by vaccination status
#'
#' Recovered from infection goes into either recovered non vaccinated, or according to vaccine, but not by strain
#' 
#' @param n_age number of age groups
#' @param n_vaccine number of vaccines
#' @param vac_prop how to proportionally distribute vaccines
#'
#' @return V matrix
#' @export
create_V_recovered_by_vax = function(n_age, n_vaccine, vac_prop = rep(1 / (n_vaccine - 1), n_vaccine - 1))
{
  # dim of S is n_vaccine * 2
  i_mat = diag(n_age)

  # this is the vaccine distribution half column with -1 for susceptible and vac_prop for each vaccine
  vac_prop = c(rep(-1, n_age), rep(vac_prop, each = n_age))
  dist_mat = vac_prop * repmat(i_mat, n_vaccine, 1)
  
  # for top need first column to be dist_mat
  V_top = cbind(dist_mat, matrix(0, nrow = n_age * n_vaccine, ncol = n_age * (n_vaccine * 2 - 1)))
  
  # for the bottom need first recovered column to be dist_mat
  V_bottom = cbind(matrix(0, nrow = n_age * n_vaccine, ncol = n_age * n_vaccine), dist_mat, matrix(0, nrow = n_age * n_vaccine, ncol = n_age * (n_vaccine - 1)))
  
  V = rbind(V_top, V_bottom)
  return(V)
}
