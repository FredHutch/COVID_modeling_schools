# Current best data sources

The Atlantic - tests, positives, hospitalizations by state

NY Times - positive cases by US county


# Government sites

### King County

Publich health [summary dashboard](https://www.kingcounty.gov/depts/health/communicable-diseases/disease-control/novel-coronavirus.aspx) has total positives and total deaths, as well as increases in each in prior 24 hours.

[Dashboard](https://www.kingcounty.gov/depts/health/communicable-diseases/disease-control/novel-coronavirus/data-dashboard.aspx) has five facets.

1. Summary of counts (same as main page): Total positives and total deaths, as well as increases in each in prior 24 hours.
2. Cumulative counts by test result date.
3. (Map) Rate of positive results by zip or city to date, per 100,000 residents. Deaths also provided in tool tip.
4. Count by test result date, discrete and cumulative, on a log scale.
5. Positive and death rate to date per 100,000 residents, split by sex at birth and separately by age group.

<span style="color:red">Requested data:

- <span style="color:red">Facet 5 data, for historical dates back to inception of disease in King County, most importantly by age group.
- <span style="color:red">Facet 3 data, for historical dates back to inception of disease in King County.
- <span style="color:red">Ideal dataset would include tests, positives, hospitalizations, in ICU, on ventilator, recovered, and deaths, by zip, age group, and sex, by test result date.
- <span style="color:red">Explanation for why Facet 4 does not tie to data from NY Times (is it simply a lag from 4pm Eastern reporting?)

https://data.kingcounty.gov/Health-Wellness/King-County-Covid-19-Dashboard-link-/hwrx-ns5c

### Snohomish County

https://www.snohd.org/499/COVID-19-Case-Count-Info

https://snohomishcountywa.gov/198/Medical-Examiner

### Washington state

[Dashboard](https://www.doh.wa.gov/Emergencies/Coronavirus) has six tabs.

1. Current status: State map of current cases, tests, deaths, and positive % of tests. Map can be split by county, and can show deaths or positive cases. Table view also available. Note: % positive tests does not equal current cases/tests.
2. Epidemiological curves: Confirmed cases or Deaths by date of onset. Graph can be filtered to set of counties.
3. Cumulative case and death counts: Confirmed cases or Deaths by report date. Graph can be filtered to set of counties.
4. Testing: Positive and negative tests by specimen collection date. Graph can be filtered to set of counties.
5. Demographics: Confirmed cases or Deaths, split by sex, separately by age group (0-19, 20-39, 40-59, 60-79, 80+). Cannot be filtered by county.
6. Hospitalizations: Percent of emergency inpatient admissions with complaints of fever and repiratory symptoms, by week hospitalization began. Graph can be split by gender or by age group. Notes indicate values for children are not much higher than normal hospitalizations for influenza.

<span style="color:red">Requested data:

- <span style="color:red">Ideal dataset will include tests (shown on tab 1), positives (tab 1), hospitalizations (tab 6), in ICU (unknown), on ventilator (unknown), recovered (unknown), and deaths (tab 1), age group (tab 5), and sex (tab 5), by test result date. 
- <span style="color:red">We would wish to split by zip code, city, or county; county appears to be available for some data.

### WHSA

https://www.wsha.org/for-patients/coronavirus/coronavirus-tracker/.

### CDC

(https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/testing-in-us.html)

https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/cases-in-us.html

https://www.cdc.gov/coronavirus/2019-ncov/covid-data/covidview.html#outpatient

Downloadable data (but didn't work) https://gis.cdc.gov/grasp/COVIDNet/COVID19_3.html


### Iceland

Govenment dashboard: https://www.covid.is/data

Can downlad a variety of data, including, cases, number of tests, age distribution as csv.

Social distancing information: https://www.covid.is/announcements

- March 16: ban on gatherings over 100
- March 19: anyone arriving the country must go into quarantine for 14 days
- March 24: ban on gatherings more than 20; maintain 2m distance; closure of swimming pools, gyms, pubs and dance halls, slot machines, museums, sport clubs, hairdressers, beauty salons, massage parlours and similar activities; schools not closed but restrictions and parents encouraged to limit chilren's contacts
- May 4: gradual lifting of restrictions

Population: 364,134 (wikipedia)

Age structure in 2019: https://px.hagstofa.is/pxen/sq/35245ffa-026a-4fd4-9f14-d69c4f478a0e
By going to statistics of Iceland website (https://www.statice.is/statistics/population)  > population > family > Average population by sex, age and marital status 1998-2019

Social contact matrix: ??

Thoughts on iceland parameters:

* fit beta_d as (0, 0.5) since those diagnosed and their close contacts are required to quarantine, as well as arrival from high risk areas
* first case 28 Feb, limited testing in Feb (41 in 1-26 Feb) then gradual ramp of of testing in March and big jump starting 15 March with deCODE)
* social distancing could be fit lower (range 0.1 - 0.75?) since there is not a complete lockdown but closures and encouraging 
* delat1 = 9 March (increases in testing, though before official measures), delata2 = 24 March


# US and global sites

### The Atlantic

[Spreadsheet](https://docs.google.com/spreadsheets/u/2/d/e/2PACX-1vRwAqp96T9sYYq2-i7Tj0pvTf6XVHjDSMIKBdZHXiCGGdNC0ypEU9NbngS8mxea55JuCFuua1MUeOj5/pubhtml#) has state level testing data loaded through sources.R. Data includes the following by date and state, both new and accumulated values. The 'states' tab also contains notes on data quality by state (not all values are provided daily, or at all, by state).

1. positive
2. negative
3. pending
4. hospitalized
5. in ICU
6. on ventilator
7. recovered
8. death

The [main website](https://covidtracking.com/) for the Atlantic has a global focus. It contains [helpful information](https://covidtracking.com/why-it-matters) for users, a few [maps and statistics](https://ourworldindata.org/coronavirus#covid-19-cases-deaths-and-recoveries-by-country), and [links to source data by country](https://ourworldindata.org/coronavirus-testing-source-data).

### NY Times

[API](https://github.com/nytimes/covid-19-data) has county data loaded through sources.R

<span style="color:red">Issue: Values here don't match the King County reported values.

### IHME

[Predictions](https://covid19.healthdata.org/) 

http://www.healthdata.org/covid/updates - explanations for changes to projected values, some info on parameters, links to methodology (will be updated 4/7)

http://www.healthdata.org/sites/default/files/files/Projects/COVID/Estimation_update_040120.pdf - updates to model on 4/1

http://www.healthdata.org/research-article/forecasting-covid-19-impact-hospital-bed-days-icu-days-ventilator-days-and-deaths - explanation for study

https://github.com/ihmeuw-msca/CurveFit - supporting curve fitting

http://ghdx.healthdata.org/ - global health data exchange

### Johns Hopkins

(https://github.com/CSSEGISandData/COVID-19)

### Aggregated: C3 Data lake (available 4/13)
[C3](https://c3.ai/covid/) has pledged to aggregate data for users from many of the most cited sources by mid-April, with plans to include NYT, Johns Hopkins, The Atlantic, IHME, etc. [Here](https://github.com/beoutbreakprepared/nCoV2019/tree/master/covid19) is the GitHub site.

### US & Canada dashboard

https://coronavirus.1point3acres.com/en

### Kaggle

https://www.kaggle.com/covid-19-contributions

https://www.kaggle.com/jeegarmaru/covid19-new-jersey-nj-local-dataset

https://registry.opendata.aws/cord-19/
  

## Social distancing

BuzzFeedarticle on Kentucky vs Tennessee response ignoring that Tennessee's testing coverage is twice as high.
https://www.buzzfeednews.com/article/danvergano/coronavirus-kentucky-tennessee-social-distancing

https://www.unacast.com/covid19/social-distancing-scoreboard

https://www.safegraph.com/dashboard/covid19-shelter-in-place


