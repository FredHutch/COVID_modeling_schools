#' R_effective
#'
#' @param parameters parameters
#' @param state current state
#' @param single.Reff return a single Reffective, or strain specific
#' @param sd.mult.side whether sd is applied to both sides of contact matrix
#'
#' @return R effective
#' @export
R_effective = function(parameters, 
                       state, 
                       single.Reff = T,
                       sd.mult.side = "both"){
  
  #Vectorize parameters
  parameters = expand_vector_params(parameters)
  
  state_matrix = state_vector_to_matrix(state)
  with(parameters,{

  
  #Construct Trans Matrix
  Trans.matrix = Get.Trans.matrix(parameters, state, sd.mult.side)
  
  if(single.Reff){
    return(Re(eigen(Trans.matrix)$values[1]))
  }
  
  strains = setdiff(unique(state_matrix$strain), "")

  sapply(strains,
         function(x){
           .y = subset(state_matrix, state == "E")
           iv = which(.y$strain == x)
           
           Re(eigen(Trans.matrix[iv,iv])$values[1])
         })
  
  })
}
