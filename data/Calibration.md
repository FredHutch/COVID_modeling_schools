# Calibration notes/question

## Parameters to calibrate 

Here are the current parameters that are being calibrated and their bounds. 

             d2 bstar beta_d delta0offset delta1   sd
      lower 0.1   0.3   0.50           35     50 0.50
      upper 0.3   0.7   0.75           55     60 0.75


delta0offset is the new parameter to fit the number of days in the model before the first casse on Feb 28.

alpha fixed to 1, beta_h fixed to 0, r_2, and r_3 updated in model equations (no more h_i), updated CFR

### Some questions about the calibration:

1. Parameter bounds? Need adjusting?


1. We are no longer fitting any age-specific parameters, this may be a problem for trying to fit the age proportions

1. Are we concerned about the time lag from onset of symptoms to testing? Ideally cases by date of onset would be best (like WA dashboard). Consider lagging the data?

1. Model peak, consider adding this to calibration when we have one?

### Data to use to calibrate

Currently using the nytimes county data becasue both cases and deaths are available. Currently only cases on king county dashboard and not downloadable. Using snapshots of king county dashboard age data (cases and deaths) to use as proportions to fit age distribution.

Still investigating data sources/options, as this is obviously not ideal.



### Validation

Consider using alternative estimates of diagnostic rate to validate/compare our estimate

* [Estimating the number of infections and the impact of nonpharmaceutical interventions on COVID-19 in 11 European countries](https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-Europe-estimates-and-NPI-impact-30-03-2020.pdf)
* [Using a delay-adjusted case fatality ratio to estimate under-reporting](https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html)


Hospitalization data (diagnosed + suspected cases)


### Data desired for calibration

* Cases (positive tests) - and by age (at least for a subset of days)
* Deaths - and by age (at least for a subset of days)
* Hospitalizations - and by age (at least for a subset of days)

### Previous questions/answers:

* Fit day0, fix dates for gradual introduction of socail distancing. Some important dates
    - March 5/6 local companies institute work from home
    - Monday March 9 UW goes to online classes
    - March 16 bars/restaurants closed
    - March 23 lockdown
    - IDM mobility data https://covid.idmod.org/data/Physical_distancing_working_and_still_needed_to_prevent_COVID-19_resurgence.pdf
    - Propose March 5 and March 24 for delta1 and delta2


* Where is the cases data from? Are there deaths too? (so I can get more recent data) The age data is just a snap shot from the dashboard?
    - The file "king county data.xslx" came from what was available on the king county dashboard at the time.

* Hospitalization data would be good too which king county must have (as it's on the WA state dashboard). 
    - This is hopefully coming

* We need to line up the model start date and the king country data. Right now we have delta1 calibrated between 50-60 (also used for testing onset) and delta2 = delta1 + 20. But the model start date is unclear to me (currently I have day 1 = Jan 14 but not sure what this should be). Do we want to reparameterize this?
    - Yes, this is now fit with the delta0offset
    
* Can we request data directly from king country so that we can get the age data too? Trying to get data from the daily dashboard is not reproducible.
    - Unknown
    
### Other parameters / data

Some other questions:

- q_i (proportion hospitalized needing ICU) if there are data on hospitalizations by age, both normal and ICU, we could estimate this directly?
- icu (number of ICU beds) this might need to change with time as beds are increasing, or just use max assuming no saturation before
- So far I've just been using the file "king county data.xslx" to calibrate with. I don't understand king_age_pyramid.xslx?
- What about data on number of tests performed? Could use to inform d?

####Other possible locations:
We know we definitely want to start with king country since there are potential applications there. For now save for later other regions. Consider based on interest in that region or perhaps for data quality - if we use a second location with high quality data are there model parameters that we think would be location invariant that we could better estimate that way?

* New York - epicenter for US

* France (or just Île-de-France/Paris) - data on hospitalizations available (normal and ICU), age data for hospitalizations and deaths, deaths is messy becasue hospitals report differently than ehpad 

* Iceland - very high [testing rate](https://edition.cnn.com/2020/04/01/europe/iceland-testing-coronavirus-intl/index.html) (and suggests 50% asymptomatic), [age data available](https://www.covid.is/data) 

* elsewhere?
