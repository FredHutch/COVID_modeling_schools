This folder is for the comparison between Washington DOH linelist data and seroprevalence. It contains all data needed to make the comparison as well as derived parameter estimates.


**CDC_SERO_PREVALENCE_COMPARISON.Rmd/docx**
This summary document shows the comparison between the datasets. It also runs an MCMC fit to estimate parameters consistent with

**DATA_AGGR_(WEEKLY).csv/.Rdata**
The WA line list data aggregated by age group and day (or week for the WEEKLY files).

**Nationwide_Commercial_Laboratory_Seroprevalence_Survey.csv**
CDC sero prevalence data (note that we know read this directly from the CDC website so it is not needed).

**Process_Sero_Prevelance.R**
Script for reading the CDC sero prevalence data into R

**sero_prevalence.Rdata**
CDC seroprevalence data in usable format

**Sero_Prevalence_function.R**
Helper functions for projecting seroprevalence data.

**SERO_MCMC.Rdata**
Output of MCMC fit to sero-prevalence data.
