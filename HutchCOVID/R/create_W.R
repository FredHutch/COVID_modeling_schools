#' Generalized W matrix creation
#' 
#' Generalized W matrix creation input type has structure immune.list
#' immune.list must be a nested list. 
#' Each sublist represents an immune state and must contain an element "Waning"
#' "Waning" is a vector that must have an entry for each immune state listed. Those entries
#'  are the rate at which one immune state decays into the other
#'  So an entry: immune.list$Recovered$Waning = c("Naive" = .1, "Recovered = 0)
#'  means that individuals travel Recovered->Naive at a rate of 0.1/day.
#'  Note that the above also implies that there are only two immune states: "Naive" and "Recovered"
#' 
#' Note that n_age is the size of the block diagonals (will typically represent age groups)
#' 
#' @param immune.list a nested list, see description
#' @param n_age number of age groups
#' @param temporal TRUE if temporal
#'
#' @return W matrix
#' @export
create_W = function(immune.list, n_age, temporal = FALSE)
{
  W_base0 = get_attribute_from_specs(immune.list, "Waning", temporal = temporal)
  
  get_W = function(W_base){
    x = create_block(W_base, n_age)
    x - diag(colSums(x))
  }
  
  if(!temporal){
    return(get_W(W_base0))
  }
  
  lapply(W_base0, get_W)
}
