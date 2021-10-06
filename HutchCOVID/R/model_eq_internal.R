#' Calculate vaccine distribution for S and R vectors for this time step
#'
#' Calculated to vectors to add to S and R to redistribute amongst the boxes in order to vaccinate. The tricky part
#' is that we need to not exceed the intended coverage rate and also not allow any box to go negative. However, we need
#' to spit the available doses by age group but also by S and R (and by strain for R). Further, even if we haven't yet 
#' hit coverage limits, there may not be enough individuals to vaccinate because some may be in varaious infected boxes
#' and not yet available in the R box.
#'
#' @param S The current vector for the S box
#' @param N The current vector for the N box, i.e. age distribution of everyone living
#' @param V_vac_prop Matrix that describes how to distribute vaccines within susceptible and recovered parts of S, each column should sum to 0 (neg to remove, pos to add, also gives propotion doses per vaccine)
#' @param n_age Number of age groups
#' @param n_strain Number of strains
#' @param n_vaccine Number of vaccines (1 = no vaccine)
#' @param n_recovered Number of recovered states
#' @param vac_total Number of doses to distribute (per vaccine #Mia note: combine vac_num and vac_dist)total across all vaccines)
#' @param vac_dist Percentage to distribute to each age group 
#' @param vac_priority Vector of age groups in priority order to distribute doses once groups saturate doses #Mia note: Please Remove
#' @param vac_coverage What percentage of each age group to vaccinate before considering saturated #Mia note: This could be handled by subdividing population?
#'
#' @return list with a several vectors: S_vaccines: vector to add to S to redistribute from unvaccinated box to
#' the different vaccinated boxes, R_vaccines: vector to add to R to redistribute from unvaccinated box to
#' the different vaccinated boxes also by strain, total_vaccinated: the total number vaccinated by age group, so the
#' rest of the vector (the spots for strains and vaccines) are zero
#' 
calculate_vaccine_distribution = function(S, N, V_vac_prop,
                                          n_age, n_strain, n_vaccine, n_recovered,
                                          vac_total, vac_dist, vac_priority, vac_coverage)
{
  n_vaccine_eff = n_vaccine + n_recovered
  
  if (vac_total > 0)
  {
    # check coverage
    # note for now coverage calculated on basis on total numbers vaccinated by age group, not per vaccine
    vaccines = vac_total * vac_dist
    N = N[1:length(vaccines)] # N is replicated
    
    # the unvaccinated idx is the rows with a negative number in V
    non_vax_idx = which(apply(V_vac_prop, 1, function(x) any(x < 0)))

    # how many to leave unvaccinated, count it this way instead of how many vaccinated, because if we start vaccinating
    # in the middle of an epidemic, there can be so many in the intermediate boxes, that we try to vaccinate too many
    leave_unvaccinated = N * (1 - vac_coverage)
    currently_unvaccinated = colSums(matrix(S[non_vax_idx], ncol = n_age, byrow = TRUE)) # add up across strains by age
    
    if (any( currently_unvaccinated - vaccines < leave_unvaccinated ))
    {
      already_done = which((currently_unvaccinated - vaccines) < leave_unvaccinated )
      extra_vaccine = 0
      
      # remove vaccine that is too much
      for (i in already_done)
      {
        # don't let vaccines go negative in case we have already exceeded coverage with people dying since the last time step
        surplus = vaccines[i] - max(0, (currently_unvaccinated[i]  - leave_unvaccinated[i]))
        vaccines[i] = vaccines[i] - surplus
        extra_vaccine = extra_vaccine + surplus
      }
      
      # add to other groups if there is room 
      if (! is.null(vac_priority))
      {
        for (i in vac_priority)
        {
          # also don't negatively vaccinate if currently_unvaccinated already smaller than leave_unvaccinated
          amt_to_add = min(extra_vaccine,  max(0, currently_unvaccinated[i]  - leave_unvaccinated[i]) - vaccines[i])
          vaccines[i] = vaccines[i] + amt_to_add
          extra_vaccine = extra_vaccine - amt_to_add
        }
      }
    }
    
    # now build repartition vector for vaccines
    
    # U is the vector that describes the proportional distribution between recovered and non-recovered (repeated for as many strains/vaccines necessary)
    U = ceiling(abs(V_vac_prop)) # first get matrix, turn anything non-zero into a 1
    U = U %*% S / rep(currently_unvaccinated, length = length(S))
    
    # now the final redistribution vector is 
    to_vaccinate = V_vac_prop %*% U * rep(vaccines, length = length(S))
    
    # now check that we haven't vaccinated more than there are left, this shouldn't be a problem 
    # since we are vaccinating proportionally
    if (any(S + to_vaccinate < 0))
    {
      warning(c("Problem with vaccine distribution for S\n", paste(S), "\nwith distribution\n", paste(to_vaccinate)))
    }
    if (abs(sum(to_vaccinate)) > 1e-10)
    {
      warning(c("Problem with vaccine distribution sum to 0 with distribution\n", paste(to_vaccinate)))
    }

    # for now just track total vaccinated by age not strain (so empty boxes)
    results = list(vaccines = to_vaccinate, 
                   total_vaccinated = c(vaccines, rep(0,  n_age * (n_vaccine_eff) * n_strain - length(vaccines))))
    
  } else
  {
    results = list(vaccines = rep(0, n_age * (n_vaccine_eff)), 
                   total_vaccinated = rep(0, n_age * (n_vaccine_eff) * n_strain))
  }
  return(results)
}

# this is the definition of the model

#' The core model. 
#'
#' Note that it should generally be run via run_model() which handles time-varying parameters. social distancing, etc. rather than directly
#' 
#' @param time vector of times 
#' @param state inital state
#' @param parameters model parameters
#'
#' @return matrix with value of compartments at each point in time
model_eq = function(time, state, parameters) {
  
  par <- as.list(c(state, parameters))
  with(par, { 
    
    S = matrix( state[state_idx("S", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Current and cumulative number of Susceptible individuals
    E = matrix( state[state_idx("E", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Current number of Exposed individuals
    A1 = matrix( state[state_idx("A1", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Current number of Asymptomatic 1 individuals
    A2 = matrix( state[state_idx("A2", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Current number of Asymptomatic 2 individuals
    P = matrix( state[state_idx("P", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Current number of Pre-mild individuals
    IM = matrix( state[state_idx("IM", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Current number of Mild individuals
    IS = matrix( state[state_idx("IS", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Current number of Severe individuals
    H = matrix( state[state_idx("H", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Current number of Hospitalized individuals
    F = matrix( state[state_idx("F", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Current and cumulative number of Deceased individuals
    DA1 = matrix( state[state_idx("DA1", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Current number of diagnosed Asymptomatic 1 individuals
    DA2 = matrix( state[state_idx("DA2", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Current number of diagnosed Asymptomatic 2 individuals
    DP = matrix( state[state_idx("DP", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Current number of diagnosed Presymptomatic individuals
    DM = matrix( state[state_idx("DM", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Current number of diagnosed Mild individuals
    DS = matrix( state[state_idx("DS", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Current number of diagnosed Severe individuals
    DH = matrix( state[state_idx("DH", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Current number of Diagnosed Hospitalized individuals
    DF = matrix( state[state_idx("DF", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Current and cumulative number of Diagnosed Deceased individuals
    
    cum_exp = matrix( state[state_idx("cum_exp", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Cumulative number of Exposed individuals
    cum_asym = matrix( state[state_idx("cum_asym", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)       # Cumulative number of Asymptomatic individuals
    cum_sym = matrix( state[state_idx("cum_sym", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)         # Cumulative number of Symptomatic individuals
    cum_diag = matrix( state[state_idx("cum_diag", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)       # Cumulative number of Diagnosed individuals
    cum_hosp = matrix( state[state_idx("cum_hosp", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)       # Cumulative number of Hospitalized individuals
    cum_death =  matrix( state[state_idx("cum_death", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)       # Cumulative number of Deaths
    cum_testpos = matrix( state[state_idx("cum_testpos", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)       # Cumulative number of positive tests
    cum_testneg = matrix( state[state_idx("cum_testneg", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)       # Cumulative number of negative tests
    cum_vac = matrix( state[state_idx("cum_vac", n_age, n_strain, n_vaccine, n_recovered)], ncol = 1)       # Cumulative number of vaccinated, by age but not by vaccine
    

    # calculated parameters
    
    ### cfr/hfr/ifr ###
    # TODO need to decide what to do about cfr (or other implementation)
    # here it's using hfr but could consider switching to ifr (though this would make us more dependent on asyptomatic proportion)
    # also need to add in possibility of dying outside hospital
    
    # calculate hospital mortality
    mort_H_i = r_h * hfr_i / (1 - hfr_i)
    mort_nonH_i = mort_H_i
    
    # in vaccine branch its 
    # mort_i = cfr / hosp_frac / hd
    # mortality of diagnosed but not hospitalized
    # die_i = cfr / hosp_frac / nhd
    
    ### betas (trnasmission) ###
    
    # calculate transmission decrease based on time period
    bfact = 1 - sd_i

    # need to reduce transmission rate if diagnosed
    beta_ad1 = beta_a1 * beta_d
    beta_ad2 = beta_a2 * beta_d
    beta_pd = beta_p * beta_d
    beta_md = beta_m * beta_d
    beta_sd = beta_s * beta_d
    
    ### calculate tests ###
    
    # calculate positive and negative tests from per capita testing rates 
    # for those with and without symptoms
    pos_tests = rho_A * delta_IM_i * (A1 + A2 + P) + delta_IM_i * IM + delta_IS_i * IS + delta_H * H
    # not currently fitting negative tests, but leave here in case we decide to use it
    neg_tests = rep(0, length(pos_tests)) # delta_notI_i * (M_conv_zeros %*% S + E + R) 
    
    # Calculated, state dependent parameters: lambda_i
    # Risk of the susceptible individuals in age group i to acquire infection through contacts with infected individuals from different age group and infection status
    N = M_conv_pop_susceptible %*% S +
      M_conv_pop_full %*% (E + A1 + A2 + P + IM + IS + H + DA1 + DA2 + DP + DM + DS + DH)

    # force of infection, LHS = susceptibility, RHS = transmission
    # bfact (reduction from social distancing) applied to both sides, reduces susceptibility and transmission
    lambda_i = bfact * kappa_i * (C_ij %*%  
                                    (bstar_i * 
                                      (bfact * (beta_a1 * beta_p * A1 + beta_a2 * beta_m * A2 + beta_p * P + 
                                                beta_s * IS + beta_m * IM + beta_h * H) + 
                                         bfact_d * (beta_ad1 * beta_p * DA1 + beta_ad2 * beta_m * DA2 + 
                                                    beta_md * DM + beta_pd * DP + beta_sd * DS + beta_h * DH)
                                      ) / N ))
    ### vaccines ###
    
    # calculate distribution of vaccines, this can be complicated by the fact that the population are 
    # separated by S and R as well as strains, so need to be careful not to go negative
    vaccination_plan = calculate_vaccine_distribution(S - (t(M_conv_replicate) %*% lambda_i) * S - t(M_conv_replicate) %*% strain_import_i, 
                                                      N, V, n_age, n_strain, n_vaccine, n_recovered,
                                                      vac_rate, vac_dist, vac_priority, vac_coverage)


    # Formulas for change in each state 
    # All terms should be either vectors with 4 elements (subscript j) or single values
    dS = -1 * (t(M_conv_replicate) %*% lambda_i) * S - t(M_conv_replicate) %*% strain_import_i + vaccination_plan$vaccines +
      Y %*% (r_a * A2 + r_m * IM  + r_h * H + r_a * DA2 + r_m * DM + r_h * DH) + W %*% S 
    dE = lambda_i * (M_conv_replicate %*% S) + strain_import_i - gamma_1 * E  
    dA1 = (1 - p_i) * gamma_1 * E - rho_A * delta_IM_i * A1 - gamma_2 * A1  
    dA2 = gamma_2 * A1 - rho_A * delta_IM_i * A2 - r_a * A2    
    dP = p_i * gamma_1 * E - rho_A * delta_IM_i * P - gamma_2 * P   
    dIM = gamma_2 * m_i * P - delta_IM_i * IM - r_m * IM  
    dIS = gamma_2 * (1 - m_i) * P - delta_IS_i * IS - h_i * IS - mort_nonH_i * IS 
    dH = h_i * IS - r_h * H - mort_H_i * H - delta_H * H  
    dF = mort_H_i * H + mort_nonH_i * IS    
    dDA1 = rho_A * delta_IM_i * A1 - gamma_2 * DA1    
    dDA2 = gamma_2 * DA1 + rho_A * delta_IM_i * A2 - r_a * DA2  
    dDP = rho_A * delta_IM_i * P - gamma_2 * DP  
    dDM = gamma_2 * m_i * DP + delta_IM_i * IM - r_m * DM   
    dDS = gamma_2 * (1 - m_i) * DP + delta_IS_i * IS - h_i * DS - mort_nonH_i * DS
    dDH = h_i * DS - r_h * DH - mort_H_i * DH + delta_H * H 
    dDF = mort_H_i * DH + mort_nonH_i * DS  
    
    # summary statistics 
    dcum_exp = lambda_i * (M_conv_replicate %*% S) + strain_import_i   # new E infections (exposed)
    dcum_asym = (1 - p_i) * gamma_1 * E                           # new A asymptomatic
    dcum_sym = p_i * gamma_1 * E                                  # new I symptomatic
    dcum_diag = pos_tests                                         # new cases (diagnosed + directly hospitalized)
    dcum_hosp = h_i * IS + h_i * DS                               # new hospitalizations
    dcum_death = dF + dDF                                         # new deaths
    dcum_testpos = pos_tests                                      # new positve tests
    dcum_testneg = neg_tests                                      # new negative tests
    dcum_vac = vaccination_plan$total_vaccinated
    

    # this order needs to match state_order
    list(c(dS, # Change in Susceptibles
           dE, # Change in Exposed
           dA1, # Change in Asymptomatic 1
           dA2, # Change in Asymptomatic 2
           dP, # Change in Pre-symptomatic
           dIM, # Change in Mild
           dIS, # Change in Severe
           dH, # Change in Hospitalized
           dF, # Change in Deaths
           dDA1, # Change in Diagnosed Asymptomatic
           dDA2, # Change in Diagnosed Asymptomatic
           dDP, # Change in Diagnosed Pre-symptomatic
           dDM, # Change in Diagnosed Mild
           dDS, # Change in Diagnosis Severe
           dDH, # Change in Diagnosed Hospitalized
           dDF, # Change in Diagnosed Deaths
           dcum_exp,    # new infections (exposed)
           dcum_asym,   # new asymptomatic
           dcum_sym,    # new symptomatic
           dcum_diag,   # new cases (diagnosed + directly hosptalized)
           dcum_hosp,    # new hospitalizations
           dcum_death,   # new deaths
           dcum_testpos, # poitive tests
           dcum_testneg, # negative tests
           dcum_vac      # newly vaccinated
         )) 
  })
}

#' Calculate vaccine distribution for S and R vectors for this time step
#'
#' Calculated to vectors to add to S and R to redistribute amongst the boxes in order to vaccinate. Simplification of the
#' previous function that should still prevent compartments from going negative
#'
#' @param S The current vector for the S box
#' @param M Matrix describing how to sum up the S
#' @param V_vac_prop Matrix that describes how to distribute vaccines within susceptible and recovered parts of S, each column should sum to 0 (neg to remove, pos to add, also gives propotion doses per vaccine)
#' @param Q Matrix that describes how to compute current coverage
#' @param nu Max per-capita rate of vaccination
#' @param n_age Number of age groups
#' @param n_strain Number of strains
#' @param n_vaccine Number of vaccines (1 = no vaccine)
#' @param n_recovered Number of recovered states
#' @param vac_total Number of doses to distribute (per vaccine #Mia note: combine vac_num and vac_dist)total across all vaccines)
#' @param vac_dist Percentage to distribute to each age group 
#' @param vac_coverage What percentage of each age group to vaccinate before considering saturated #Mia note: This could be handled by subdividing population?
#'
#' @return list with a several vectors: S_vaccines: vector to add to S to redistribute from unvaccinated box to
#' the different vaccinated boxes, R_vaccines: vector to add to R to redistribute from unvaccinated box to
#' the different vaccinated boxes also by strain, total_vaccinated: the total number vaccinated by age group, so the
#' rest of the vector (the spots for strains and vaccines) are zero
#' 
calculate_vaccine_distribution_smooth = function(S, M, V_vac_prop, Q, nu,
                                          n_age, n_strain, n_vaccine, n_recovered,
                                          vac_total, vac_dist, vac_coverage)
{
    
    currently_vaccinated = Q%*%S
    need_vaccination = pmax(0, M%*%S * vac_coverage - currently_vaccinated)
    
    # calculate the relative effort put in by each group to seek vaccination
    seek_vaccination = diag(V_vac_prop) * need_vaccination * vac_dist
    
    #redistribute vaccines based on total available
    kv = vac_total * nu/(vac_total + nu * sum(seek_vaccination))
    
    to_vaccinate = kv * V_vac_prop %*% (need_vaccination * vac_dist)
    
    
    # for now just track total vaccinated by age not strain (so empty boxes)
    results = list(vaccines = to_vaccinate, 
                   total_vaccinated = kv * seek_vaccination)
    

  return(results)
}