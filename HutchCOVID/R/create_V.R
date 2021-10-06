#' Generalized V matrix creation
#' 
#' Note that n_age is the size of the block diagonals (will typically represent age groups)
#' 
#' @param immune.list a nested list
#' @param vaccination.list a nested list
#' @param n_age number of age groups
#'
#' @return V matrix
#' @export
create_V = function(immune.list, vaccination.list, n_age){
  
  rates = sapply(vaccination.list, function(x){x$fraction.of.vaccines})
  
  rates = rates/sum(rates)
  .f = function(y){ rates[y] * Reduce('cbind', 
                         sapply(immune.list, 
                                 function(x){
                                   x$Fate[y]
                                 }))
  }
  
  V_base0 = Reduce('+', ###OMG This is hard to follow
                  lapply(names(vaccination.list), 
                         .f)
  )
  
  V_base = V_base0 - diag(colSums(V_base0))
  create_block(V_base, n_age)
}
