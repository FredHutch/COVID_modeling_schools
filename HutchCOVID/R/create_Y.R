#' Generalized Y matrix creation
#' 
#' Note that n_age is the size of the block diagonals (will typically represent age groups)
#' 
#' @param immune.list a nested list
#' @param strain.list a nested list
#' @param n_age number of age groups
#'
#' @return Y matrix
#' @export
create_Y = function(immune.list, strain.list, n_age)
{
  
  .f = function(y){Reduce('cbind', 
         sapply(immune.list, 
                function(x){
                  x$Fate[y]
                }))
  }
  
  Y_base = Reduce('cbind', ###OMG This is hard to follow
                  lapply(names(strain.list), 
                         .f)
  )
  
  
  create_block(Y_base, n_age)
}
