#' W with no waning
#' 
#' no waning, like in original version of model, protection from infection lasts forever
#'
#' @param n_age number of age groups
#' @param n_vaccine number of vaccines
#' @param n_recovered number of recovered groups
#'
#' @return W matrix
#' @export
create_W_no_waning = function(n_age, n_vaccine, n_recovered)
{
  W = matrix(0, nrow = n_age * (n_vaccine + n_recovered), ncol = n_age * (n_vaccine + n_recovered))
  return(W)
}
