
# Set starting infected and susceptible counts ####


# for now handle age groups, strains, and vaccines, there is not currently support for risk groups

#' Gets indices into state vector of state
#'
#' @param state state, see state_order
#' @param n_age number of age groups
#' @param n_strain number of strains
#' @param n_vaccine number of vaccines
#' @param n_recovered number of recovered
#'
#' @return indices
state_idx = function(state, n_age, n_strain, n_vaccine, n_recovered)
{
  # to allow recovered as a quasi vaccine
  n_vaccine_eff = n_vaccine + n_recovered
  
  # S vector is shorter than others since no strains
  if (state == "S")
  {
    idxs = seq(1, n_age * n_vaccine_eff)
  }
  else
  {
    # index relative to S
    state_i = which(get_state_order() == state) - 1
    state_len =  n_age * n_strain * n_vaccine_eff
    idx_start = (state_i - 1) * state_len + 1 + n_age * n_vaccine_eff # previous regular states + 1 + S state
    idxs = seq(from = idx_start, len = state_len)
  }
  return(idxs)
}

#' extracts just the state variables that can be used to start running model from that state or to calculate Reff
#'
#' @param model_out matrix model output
#' @param row_idx which row
#'
#' @return state vector
extract_state = function(model_out, row_idx)
{
  idx_col_to_drop = which(colnames(model_out) == "time")
  idx_col_to_drop = c(idx_col_to_drop, which(startsWith(colnames(model_out), "sd")))
  state = model_out[row_idx, -idx_col_to_drop]
  return(state)
}
