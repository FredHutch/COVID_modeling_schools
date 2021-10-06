#' W with vaccine and natural infection waning
#' 
#' that is columns for age and rows for vaccines and strains
#' note the first vaccine is 'no vaccine' so the first row should be all 0 (like VE)
#'  
#' @param n_age number of age groups
#' @param n_vaccine number of vaccines
#' @param n_recovered number of recovered groups
#' @param waning_matrix a (m + p) x n matrix giving the waning per age for each vaccine and strain infection
#'
#' @return W matrix
#' @export
create_W_most_recent = function(n_age, n_vaccine, n_recovered, waning_matrix)
{
  waning_vec = as.vector(t(waning_matrix)) # need by row
  W_dim = n_age * (n_vaccine + n_recovered)
  stopifnot(length(waning_vec) == W_dim)
  
  # start with zero matrix, then subtract waning from diagonal and add it to first row
  W = matrix(0, nrow = W_dim, ncol = W_dim)
  I = diag(W_dim)
  W = W - waning_vec * I
  W[1,] = W[1,] + waning_vec
  
  return(W)
}
