# Parameter sets metadata: this describes the parameter sets in this folder and the calibration conditions

### params_firstperiod_rej.csv / params_firstperiod_mcmc.csv
These two calibrations date to the end of April, so are likely no longer work well with model changes in the meantime (sd, etc.) and further do not have parameter names. The corresponding rdata files have the parameter lists, but are also likewise out of date with the current model.

### params_out_firstperiod_rej.csv
This calibration was done after the change of sd to both sides (transmission and susceptibility). And was done with a simple rejection algorithm to the first period data through 30-April-2020 fitting to weekly cases, hospitalizations, deaths. It could be improved with either more samples or a better algorithm (MCMC, etc), but provides a reasonable fit.

### params_out_firstperiod_hosponly_rej.csv
This was done exactly the same as params_out_firstperiod_rej.csv, though the fitting criteria was hospitalizations only. However it has a lower DIC (calculated against all metrics for comparability)

### params_out_firstperiod_newbounds_rej.csv
This was done exactly the same as params_out_firstperiod_rej.csv, except with adjusted bounds for some parameters for the prior distributions. bstar was changed to (0.02, 0.04) from (0.01, 0.025), that is implying R0 2-4 rather than 1-2.5 from the rough calculation, the lower bound for delta_IM was changed from 0.05 to 0.01 to try to improve the problem of overestimating cases, especially for age 1, and the lower bound for sd was changed from 0.05 to 0.3 to force a higher initial sd (equivalent to 0.5 on the old scale). This calibration has a higher DIC but fit reasonable well. bstar is against the lower end of it's range, and the posteriors of beta_d and first_inf_day are more informative, likely to compensate for bstar not able to go as low as it would unconstrained.

### params_out_firstperiod_noschools_rej.csv
This is similar to params_out_firstperiod_rej.csv and params_out_firstperiod_newbounds_rej.csv, in that it keeps most of the new bounds, but relaxes the bounds on bstar (now that we've calculated R0 and are happy with the wider prior range). There are now sd paramsters for the different components of the contact matrix, and for this calibration, sd_school is set to 0 on March 12 (all others remain at 1 and the regualr sd age paramaeters are also fit). Thus there are no new parameters. The fit is similar to others.

### params_out_lastperiod_rej
This is a calibration of the ending period Apr 1 - June 15 2021 in order to better set sd before scenario start. Assumptions include an initial prevalence of delta at 0.01 %, vaccination of 10000 per day (calculated from the vaccination prevalence at start and end with age distribution based on data), schools closed, and 2 values for sd to capture the peak in cases at the end of April. The posteriors of the first period calibration with schools closed were used for bstar, beta_d, and m.
