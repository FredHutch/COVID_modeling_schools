#functions to expand vectors/matrices
# these assume that the vectors/matrices are already by age, so they just need to be
# expanded for the number of new strains and vaccines
# NOTE: n_vaccine = 1 is no vaccine, and n_vaccine = 2 is no vaccine and just 1 type of vaccine, etc.
# assumed order is age (n), vaccine (p), strain (m)

#' Expands vectorized parameters
#'
#' Rewrites parameter vectors to be the necessary length for vaccines and strains
#  and also adds M matrix
#'
#' @param pars parameters pre-expansion
#'
#' @return parameters expanded as well as matrices added
expand_vector_params = function(pars)
{
  n_age = pars$n_age
  n_vaccine = pars$n_vaccine
  n_strain = pars$n_strain
  # to allow recovered as a quasi vaccine
  n_vaccine_eff = n_vaccine + pars$n_recovered 
  
  
  # simple parameters that just need to be repeated
  # use _i notation to denote that a parameter has been replicated (this also avoids problem of expanding param multiple times)
  n_rep = pars$n_strain * n_vaccine_eff
  simple_pars = get_simple_vectorized_params()
  
  for (p in simple_pars)
  {
    pars[[paste0(p, "_i")]] = rep(pars[[p]], times = n_rep)
  }
  
  # parameters that need vaccine eff: bstar, p (asymp), kappa
  # bstar and VEi
  stopifnot(length(pars$vei) == n_vaccine_eff * n_strain)
  stopifnot(length(pars$strain_infectivity) == n_strain)
  stopifnot(length(pars$strain_severity) == n_strain)
  pars$bstar_i = expand_vector_vaceff(rep(pars$bstar, times = n_age), pars$vei)
  pars$bstar_i = pars$bstar_i * rep(pars$strain_infectivity, each = n_age * n_vaccine_eff)
  
  # p and VEp
  stopifnot(length(pars$vep) == n_vaccine_eff * n_strain)
  pars$p_i = expand_vector_vaceff(pars$p, pars$vep)
  
  # kappa and VEs
  stopifnot(length(pars$ves) == n_vaccine_eff * n_strain)
  pars$kappa_i = expand_vector_vaceff(pars$kappa, pars$ves)
  
  # contact matrix
  pars$C = assemble_contact_matrix(pars)
  pars$C_ij = expand_contact_matrix(pars$C, n_strain, n_vaccine_eff)
  
  # modify m by vaccine efficacy against severe disease (VEh) and strain severity
  stopifnot(length(pars$veh) == n_vaccine_eff * n_strain)
  stopifnot(length(pars$strain_severity) == n_strain)
  severity = rep(1 - as.vector(t(pars$veh), mode = "numeric"), each = n_age) # transpose since by row
  severity = severity * rep(pars$strain_severity, each = n_age * n_vaccine_eff) 
  pars$m_i = pars$m_i / (pars$m_i + severity * (1 - pars$m_i))
  
  # strain imports
  pars$strain_import_i = expand_strain_import(pars$strain_import, n_age, n_vaccine_eff)
  
  # M conversion matrices to convert between
  # lambda and S
  # tests and S
  # sum population across strains and vaccines and redistribute (for lambda)
  # sum S population and redistribute (for lambda)
  pars$M_conv_replicate = create_conv_by_replicate(n_age = n_age, n_strain = n_strain, n_vaccine_eff = n_vaccine_eff)
  pars$M_conv_zeros = create_conv_by_zeros(n_age = n_age, n_strain = n_strain, n_vaccine_eff = n_vaccine_eff)
  pars$M_conv_pop_full = create_pop_conv_full(n_age = n_age, n_strain = n_strain, n_vaccine_eff = n_vaccine_eff)
  pars$M_conv_pop_susceptible = create_pop_conv_susceptible(n_age = n_age, n_strain = n_strain, n_vaccine_eff = n_vaccine_eff)
  
  
  return(pars)
}

#' Gets parameters that are vectorized by age and thus need to be expanded for strains and vaccines
#'
#' @return vector of parameter names
get_simple_vectorized_params = function()
{
  return(c("bfact_d", "h", "hfr", "m", "delta_IM", "delta_IS", "delta_notI", "rho_A", "sd", "dynamic_sd_min", "dynamic_sd_max" ))
}

#' Gets if parameter p is vectorized or not
#'
#' @param p parameter name
#'
#' @return TRUE if vectorized, FALSE otherwise
is_simple_vectorized_param = function(p)
{
  return(p %in% get_simple_vectorized_params())
}

#' Gets parameters that are vectorized by age and thus need to be expanded for strains and vaccines,
#' that also are affected by vaccine efficacy
#'
#' @return vector of parameter names
get_veff_vectorized_params = function()
{
  return(c("bstar", "p", "kappa"))
}

#' Gets if parameter p is vectorized and impacted by vaccine efficacy or not
#'
#' @param p parameter name
#'
#' @return TRUE if vectorized and affected by vaccine efficacy, FALSE otherwise
is_veff_vectorized_param = function(p)
{
  return(p %in% get_veff_vectorized_params())
}

#' Is parameter vectorized in terms of age
#' 
#' Note that bstar is vectorized but not age-specific
#'
#' @param p parameter name
#'
#' @return TRUE if vectorized by age, FALSE otherwise
is_age_specfic_param = function(p)
{
  return( ( is_simple_vectorized_param(p) | is_veff_vectorized_param(p) ) & p != "bstar" )
}

#' Does parameter change the contact matrix C
#' 
#' @param p parameter name
#'
#' @return TRUE if changes contact matrix, FALSE otherwise
is_contact_param = function(p)
{
  return( p %in% c("C.work", "C.home", "C.school", "C.other", "work_sd", "home_sd", "school_sd", "other_sd"))
}

#' Expand a vector of age specific parameters
#' 
#' @param v vector to expand
#' @param n_strain number of strains
#' @param n_vaccine number of vaccines
#' @param n_recovered number of recovered states
#'
#' @return expanded vector
expand_vector = function(v, n_strain, n_vaccine, n_recovered)
{
  return( rep( v, times = n_strain * (n_vaccine + n_recovered) ) )
}

#' Expand a vector of age specific parameters by a set of vaccine efficacies
#'
#' NOTE: the first vac_eff should be 0 to correspond to no vaccine
#' 
#' @param v vector to expand
#' @param vac_eff matrix of vaccine efficacies with rows for each strain
#'
#' @return expanded vector
expand_vector_vaceff = function(v, vac_eff)
{
  vac_eff = as.vector(t(vac_eff), mode = "numeric") # transpose since by row
  v_new = v * (1 - rep(vac_eff, each = length(v)) )
  return(v_new)
}

#' Repeats matrix X row and column wise
#'
#' @param X matrix
#' @param rows how many times to repeat
#' @param cols how many times to repeat
#'
#' @return repeated matrix
repmat = function(X, rows, cols)
{
  row_dim = dim(X)[1]
  col_dim = dim(X)[2]
  matrix(t(matrix(X, row_dim, col_dim * cols)), row_dim * rows, col_dim * cols, byrow = TRUE)
} 

#' creates a diagonal matrix with diag_matrix on the diagonal and zero_matrix everywhere else, with dim x dim blocks
#'
#' @param diag_block block for diagonal
#' @param zero_block block for everywhere else
#' @param dim how many blocks
#'
#' @return contructed matrix
create_block_diag = function(diag_block, zero_block, dim)
{
  # handle special case when strain = 1
  if (dim == 1)
  {
    mat_new = diag_block
  } else
  {
    mat_new = NULL
    for (i in 1:dim)
    {
      # start new row
      the_row = if (i == 1) { diag_block } else { zero_block }
      
      for (j in 2:dim)
      {
        the_row = cbind(the_row, if (i == j) { diag_block } else { zero_block })
      }
      mat_new = rbind(mat_new, the_row)
    }
  }
  
  return(mat_new)
}

#' Assembles different parts of contact matrix, also accounting for location-specific sd
#'
#' @param pars parameters
#'
#' @return contact matrix
assemble_contact_matrix = function(pars)
{
  return((1 - pars$home_sd) * pars$C.home + (1 - pars$other_sd) * pars$C.other + (1 - pars$school_sd) * pars$C.school + (1 - pars$work_sd) * pars$C.work)
}

#' Expand contact matrix for multiple strains and vaccines
#'
#' @param c contact matrix
#' @param n_strain number of strains
#' @param n_vaccine_eff number of vaccines
#'
#' @return expanded contact matrix
expand_contact_matrix = function(c, n_strain, n_vaccine_eff)
{
  # c must be square
  c_dim = nrow(c)
  zero_fill = matrix(0, nrow = c_dim, ncol = c_dim)
  
  # what we need is to replicate the C and 0 matrices into a larger square matrix
  # that is n_vaccine x n_vaccine big
  # then these larger blocks of C's and 0's make up a larger square matrix
  # that is n_strain x n_strain with C on the diagonal and 0 elsewhere
  
  # I build up the blocks then bind them into the larger matrix, not the most efficient, but more clear
  c_block = repmat(c, n_vaccine_eff, n_vaccine_eff)
  zero_block = repmat(zero_fill, n_vaccine_eff, n_vaccine_eff)

  # now make the larger matrix with c_block on the diagonal
  c_new = create_block_diag(c_block, zero_block, n_strain)
  return(c_new)
}

#' Expands that strain import so that 0's are added for vaccine entries
#' 
#' result is a n x p x m vector
#' strain_import_matrix is a m x n matrix
#' 
#' @param strain_import_matrix strain imports per day
#' @param n_age number of age groups
#' @param n_vaccine_eff how many vaccine and recovered states
#'
#' @return expanded strain import vector
expand_strain_import = function(strain_import_matrix, n_age, n_vaccine_eff)
{
  imports = NULL
  zero_vec = rep(0, ncol(strain_import_matrix))
  stopifnot(n_age == ncol(strain_import_matrix))
  
  for (s in 1:nrow(strain_import_matrix))
  {
    imports = c(imports, strain_import_matrix[s,])
    imports = c(imports, rep(zero_vec, times = n_vaccine_eff - 1))
  }
  return(imports)
}

#' Creates M that converts between S and other boxes that are by strain
#'
#'M %*% S will be the dim for E, I etc.
#' and t(M) %*% param_vec will be the dim for S
#' 
#' @param n_age number of age groups
#' @param n_strain number of strains
#' @param n_vaccine_eff number of vaccine and recovered states
#'
#' @return M matrix
create_conv_by_replicate = function(n_age, n_strain, n_vaccine_eff)
{
  # we need to replicate blocks of identity matrices, so I is an nxn identity matrix, and 0 is nxn 0 matrix
  # then we have as many columns in the block identity matrix made up of I and 0 as there are vaccines
  # and this block is replicated vertically for as many strains as there are
  i_block = diag(n_age)
  zero_block = matrix(0, nrow = n_age, ncol = n_age)
  
  M = create_block_diag(i_block, zero_block, n_vaccine_eff)
  M = repmat(M, n_strain, 1)
  return(M)
}

#' Creates an alternative M that converts between S and other boxes that are by strain
#'
#' in this case the additional dimensions are filled with 0's
#' this avoids double (or triple) counting for calculating things like negative tests
#' 
#' @param n_age number of age groups
#' @param n_strain number of strains
#' @param n_vaccine_eff number of vaccine and recovered states
#'
#' @return M matrix with 0's for non-S compartments
create_conv_by_zeros = function(n_age, n_strain, n_vaccine_eff)
{
  # like create_conv_by_replicate() above, but instead of replicating the created block
  # just use it once and fill therest with 0's to avoid double counting
  i_mat = diag(n_age * n_vaccine_eff)
  zero_mat = matrix(0, nrow = n_age * n_vaccine_eff * (n_strain - 1), ncol = n_age * n_vaccine_eff)
  
  M = if (n_strain > 1 ) { rbind(i_mat, zero_mat) } else { i_mat }
  return(M)
}

#' Creates matrix to sum and replicate population across boxes, keeping it age structured
#' 
#' So it will sum across strains and vaccines, but replicate the final 
#' age-structured population across strains and vaccines. Used to calculate lambda.
#'
#' @param n_age number of age groups
#' @param n_strain number of strains
#' @param n_vaccine_eff number of vaccine and recovered states
#'
#' @return matrix
create_pop_conv_full = function(n_age, n_strain, n_vaccine_eff)
{
  # here we need to replicate I an nxn identity matrix, into a square matrix with
  # n_strain * n_vaccine rows and columns
  i_mat = diag(n_age)
  block_dim = n_strain * n_vaccine_eff
  M = repmat(i_mat, block_dim, block_dim)
  return(M)
}

#' Creates matrix to sum and replicate population the susceptible (S) population 
#' across boxes, keeping it age structured
#' 
#' So it will sum across vaccines, but replicate the final 
#' age-structured population across strains and vaccines. 
#'
#' @param n_age number of age groups
#' @param n_strain number of strains
#' @param n_vaccine_eff number of vaccine and recovered states
#'
#' @return matrix
create_pop_conv_susceptible = function(n_age, n_strain, n_vaccine_eff)
{
  # here we need to replicate I an nxn identity matrix, into a matrix with
  # n_strain * n_vaccine rows and only n_vaccine columns (because S is just by vaccine and not strain)
  i_mat = diag(n_age)
  M = repmat(i_mat, n_strain * n_vaccine_eff, n_vaccine_eff)
  return(M)
}

# W matrix is square, the same dimension as S (that is n * (p + m))

# Y matrix is not square, it translates from other compartments to S
# so dimension is n * (p + m) X n * m * (p + m)

# V matrix is square, the same dimension as S, and needs to be paired wih Y so that Y describes how
# distribute recovered back to S, V describes how to vaccinate S (including the recovered compartments)



#' Generalized creation for W and Y matrices
#'
#' @param M base matrix
#' @param n_age number of age groups 
#'
#' @return matrix
create_block = function(M, n_age)
{
  
  
  M.out = matrix(0, nrow = nrow(M) * n_age, ncol = ncol(M) * n_age)
  
  for(i in seq(nrow(M))){
    i.vals = seq((i - 1) * n_age + 1, i * n_age)
    for(j in seq(ncol(M))){
      j.vals = seq((j - 1) * n_age + 1, j * n_age)
      block = M[i,j]
      
      if(is.list(block)){
        block = unlist(block)
      }
      
      if(length(block)==1){
        block = rep(block, n_age)
      }
      M.out[i.vals, j.vals] = diag(block)
    }
  }
  M.out
}


#' Get attribute from specs
#'
#' @param input.list input
#' @param attribute attribute
#' @param temporal TRUE if temporal
#'
#' @return attribute
get_attribute_from_specs = function(input.list, attribute, temporal = F){
  attribute_list = lapply(input.list, function(x){x[[attribute]]})
  
  date_list = Reduce("union",
                     lapply(attribute_list, 
                            function(x){if("base"%in%names(x)){setdiff(names(x), "base")}}
                            )
  )
  
  date_list = c("base", as.character(sort(Reduce('c', lapply(date_list, lubridate::ymd)))))
  
  get_temporal_list = function(date){
    sapply(attribute_list, 
                         function(x){
                           n.x = names(x)
                           if("base"%in%names(x)){
                             date.to.use = tail(which(sapply(n.x, function(y)which(date_list == y)[1])<=date), 1)
                             x[[date.to.use]]
                           }
                           else{
                             x
                           }
                           })
  }
  
  .out = lapply(seq_along(date_list), get_temporal_list)
  names(.out) = date_list
  
  if(temporal){
    return(.out[-1])
  }
  .out[[1]]
}

#' Augment temporal
#'
#' @param params_temporal temporal parameters
#' @param temp_list temporal parameter list
#' @param param_name parameter name
#'
#' @return temporal list with additional parameters added
augment_temporal = function(params_temporal, temp_list, param_name){
  
  for(j in seq_along(temp_list)){
    date.entry = names(temp_list)[j]
    
    entry = temp_list[[j]]
    K = length(params_temporal)
    i = 1
    matched = 0
    while(i < K & !matched){
      match = date.entry == params_temporal[[i]]$date
      
      if(match){
        params_temporal[[i]][[param_name]] = entry
        matched = 1
      }
      else{
        i = i + 1
      }
    }
    
    if(!matched){
      params_temporal[[K+1]] = list(date = lubridate::ymd(date.entry))
      params_temporal[[K+1]][[param_name]] = entry
    }
    
  }
  params_temporal
}
