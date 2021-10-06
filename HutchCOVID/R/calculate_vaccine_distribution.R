calculate_vaccine_distribution <-
function(S, N, V_vac_prop,
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
                   total_vaccinated = c(to_vaccinate, rep(0,  n_age * (n_vaccine_eff) * n_strain - length(to_vaccinate))))
    
  } else
  {
    results = list(vaccines = rep(0, n_age * (n_vaccine_eff)), 
                   total_vaccinated = rep(0, n_age * (n_vaccine_eff) * n_strain))
  }
  return(results)
}
