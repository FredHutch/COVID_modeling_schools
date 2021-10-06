#' Model states
#' 
#' note that matching to order here and in the model definition is critical and thus we define indices
#' unfortunately can't pass as list of vectors like parameters or ode will only allow returning 
#' results the same length as the list 
#' 
#' @return vector of state names
#' @export
get_state_order = function()
{
  return( c("S", "E", "A1", "A2", "P", "IM", "IS", "H", "F",
            "DA1", "DA2", "DP", "DM", "DS", "DH", "DF",
            "cum_exp", "cum_asym", "cum_sym", "cum_diag", "cum_hosp", "cum_death",
            "cum_testpos", "cum_testneg", "cum_vac") )
}