![Data Exploration Scripts](/data/Data&#32;Exploration&#32;Scripts/Data_Exploration_Fig.png)

This "exploration scripts" are intended to be used to calculate parameters directly from the aggregated Washington Data (or subsets thereof). See "Data Aggregation Scripts" for information on how to create these aggregated data sets.

All scripts are run on a "working folder" which should contain a file called DATA_AGGR_WEEKLY.Rdata.

**Estimate_Diagnostic_Rate.R**
This script estimates the diagnostic rate by age group and disease severity level using a mixed effects model.
The output parameter estimates are stored in "workingfolder/diagnosis.data.Rdata" as the structure diagnostic.rates.

**Estimate_HFR.R**
This script estimates the hospital fatality rate by age group and month using a mixed effects model.
The output parameter estimates are stored in "workingfolder/HFR_estimates.Rdata"

**Estimate_Severe_Cases.R**
This script estimates the proportion of diagnosed cases which are hospitalized by age and month. Note that this parameter can be hard to interpret as not all cases are diagnosed.
The output parameter estimates are stored in "workingfolder/Fraction_Severe.Rdata"


**Estimate_True_Infections.R**
This script combines all parameter estimates to attempt to estimate "true" rates of hospitalization among other things.
The output parameter estimates are stored in "workingfolder/monthly_rates.Rdata"


**Analyze_Diagnostic_Delay.Rd**
This script analyzes the delays between symptom onset and hospitalization, hospitalization and death, as well as symptom onset and diagnosis.
The output parameter estimates are stored in "workingfolder/optimized.delay.Rdata"
