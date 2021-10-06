####Calculate R-effective####

#Author: Mia Moore
#Date: 4/6/2021
#Purpose: Define a function to calculate R-effective



#' Return the full next generation matrix
#'
#' @param parameters model parameters
#' @param state current state
#' @param sd.mult.side whether sd is applied to both sides of contact matrix
#'
#' @return matrix
Next_Generation_Matrix = function(parameters,
                                  state,
                                  sd.mult.side){
  #Vectorize parameters
  parameters = expand_vector_params(parameters)
  
  Get.Trans.matrix(parameters, state, sd.mult.side = sd.mult.side)
}

#' Get Trans matrix
#'
#' @param parameters model parameters
#' @param state current state
#' @param sd.mult.side whether sd is applied to both sides of contact matrix
#'
#' @return matrix
Get.Trans.matrix = function(parameters, state, sd.mult.side = "both"){
  
  #Compute Proportions in each state
  proportions = Proportion.by.state(parameters)
  
  state.list = state_vector_to_list(state)
  with(as.list(c(proportions, parameters, state.list)),{
    
    # in vaccine branch its 
    # mort_i = cfr / hosp_frac / hd
    # mortality of diagnosed but not hospitalized
    # die_i = cfr / hosp_frac / nhd
    
    ### betas (transmission) ###
    
    # calculate transmission decrease based on time period
    bfact = 1 - sd_i
    
    # need to reduce transmission rate if diagnosed
    beta_ad1 = beta_a1 * beta_d
    beta_ad2 = beta_a2 * beta_d
    beta_pd = beta_p * beta_d
    beta_md = beta_m * beta_d
    beta_sd = beta_s * beta_d
    
    
    #Transmission by state
    Trans_Ei = 0 * Prop_Ei
    Trans_A1i = beta_a1 * beta_p * Prop_A1i
    Trans_A2i = beta_a2 * beta_m * Prop_A2i 
    Trans_Pi = beta_p * Prop_Pi
    Trans_Mi = beta_m * Prop_Mi
    Trans_Si = beta_s * Prop_Si
    Trans_DPi = beta_pd * Prop_DPi
    Trans_DMi = beta_md * Prop_DMi
    Trans_DSi = beta_sd * Prop_DSi
    Trans_DA1i = beta_ad1 * Prop_DA1i
    Trans_DA2i = beta_ad2 * Prop_DA2i 
    Trans_Hi = beta_h * Prop_Hi
    Trans_DHi = beta_h * Prop_DHi
    
    
    #Transition matrix satisfying y = Ax where x is the number of infected people (by age group) and  y is the number of people they infection.
    #We wish to solve y = Reff x = Ax SO Reff is the dominant eigen value of A.
    
    #Compute Population
    N = as.vector(M_conv_pop_susceptible %*% S +
                    M_conv_pop_full %*% (E + A1 + A2 + P + IM + IS + H + DA1 + DA2 + DP + DM))
    
    #Compute First Eigenvalue
    Effective_S = as.vector(M_conv_replicate %*% S)
    
    #Transmission
    lambda = (Trans_Ei + Trans_A1i + Trans_A2i + Trans_Pi + Trans_Mi + Trans_Si) *  bstar_i / N
    lambda_d = (Trans_DPi + Trans_DMi + Trans_DSi + Trans_DA1i + Trans_DA2i) *  bstar_i / N
    
    if(sd.mult.side == "right"){
      return(diag(as.numeric(Effective_S * kappa_i))%*%C_ij%*%diag(bfact * lambda + bfact_d * lambda_d))
    }
    
    if(sd.mult.side == "left"){
      return(diag(as.numeric(bfact * Effective_S * kappa_i))%*%C_ij%*%diag(lambda + lambda_d))
    }
    
    if(sd.mult.side == "both"){
      return(diag(as.numeric((bfact) * Effective_S * kappa_i))%*%C_ij%*%diag(bfact * lambda + bfact_d * lambda_d))
    }
    
  })
}


#' Proportion by state
#'
#' @param parameters parameters
#'
#' @return proportions
Proportion.by.state = function(parameters){
  with(as.list(parameters),{
    
    
    # calculate hospital mortality
    mort = r_h * hfr/(1 - hfr) 
    mort_nonH = mort
    
    delta_A_i = rho_A * delta_IM_i
    
    Time_Ei = 1/gamma_1
    Time_A1i = 1/(delta_A_i + gamma_2)
    Time_A2i = 1/(delta_A_i + r_a)
    Time_Pi = 1/(delta_A_i + gamma_2)
    Time_Mi = 1/(delta_IM_i + r_m)
    Time_Si = 1/(delta_IS_i + h_i + mort_nonH)
    Time_DPi = 1/gamma_2
    Time_DMi = 1/r_m
    Time_DSi = 1/(h_i + mort_nonH)
    Time_DA1i = 1/gamma_2
    Time_DA2i = 1/r_a
    Time_Hi = 1/(r_h + mort + delta_H)
    Time_DHi = 1/(r_h + mort)
    
    #Fraction that enter each state
    Frac_Ei = 1
    Frac_A1i = 1 - p_i
    Frac_A2i = gamma_2 * Time_A1i
    Frac_Pi = p_i
    Frac_Mi = Frac_Pi * gamma_2 * Time_Pi * m_i
    Frac_Si = Frac_Pi * gamma_2 * Time_Pi * (1 - m_i)
    Frac_DPi = delta_A_i * Frac_Pi * Time_Pi
    Frac_DMi = Frac_DPi * m_i + delta_IM_i * Frac_Mi * Time_Mi
    Frac_DSi = Frac_DPi * (1 - m_i) + delta_IS_i * Frac_Si * Time_Si
    Frac_DA1i = delta_A_i * Frac_A1i * Time_A1i
    Frac_DA2i = Frac_DA1i + delta_A_i * Frac_A2i * Time_A2i
    Frac_Hi = h_i * Frac_Si * Time_Si
    Frac_DHi = h_i * Frac_DSi * Time_DSi + delta_H * Frac_Hi * Time_Hi
    
    
    
    list(Prop_Ei = Frac_Ei * Time_Ei,
    Prop_A1i = Frac_A1i * Time_A1i,
    Prop_A2i = Frac_A2i * Time_A2i,
    Prop_Pi = Frac_Pi * Time_Pi,
    Prop_Mi = Frac_Mi * Time_Mi,
    Prop_Si = Frac_Si * Time_Si,
    Prop_DEi = 0,
    Prop_DPi = Frac_DPi * Time_DPi,
    Prop_DMi = Frac_DMi * Time_DMi,
    Prop_DSi = Frac_DSi * Time_DSi,
    Prop_DA1i = Frac_DA1i * Time_DA1i,
    Prop_DA2i = Frac_DA2i * Time_DA2i,
    Prop_Hi = Frac_Hi * Time_Hi,
    Prop_DHi = Frac_DHi * Time_DHi
    )
    
    
  })
}

