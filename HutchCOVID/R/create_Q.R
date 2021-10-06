#' Generalized Q matrix creation
#' 
#' Generalized Q matrix creation input type has structure immune.list
#' immune.list must be a nested list. 
#' Each sublist represents an immune state and must contain an element "Coverage"
#' "Coverage" is a vector that must have an entry for each immune state listed. Those entries
#'  are the contribution towards the coverage "felt" by individuals in the immune state
#'  So an entry: immune.list$Recovered$Coverage = c("Naive" = 0, "Recovered" = 0, "Vax" = 1)
#'  means that individuals in the recovered class compute their current coverage by looking at individuals in the "Vax" class
#'  Note that the above also implies that there are only three immune states: "Naive", "Recovered", and "Vax"
#' 
#' Note that n_age is the size of the block diagonals (will typically represent age groups)
#' 
#' @param immune.list a nested list, see description
#' @param n_age number of age groups
#' @param temporal TRUE if temporal
#'
#' @return Q matrix
#' @export
create_Q = function(immune.list, n_age, temporal = FALSE)
{
  Q_base0 = get_attribute_from_specs(immune.list, "Coverage", temporal = temporal)
  
  if(sum(sapply(Q_base0, is.null))>1){
    return(NULL)
  }
  get_Q = function(x){
    Q_base = x
    
    create_block(Q_base, n_age)
  }
  
  if(!temporal){
    return(get_Q(Q_base0))
  }
  
  lapply(Q_base0, get_Q)
}
