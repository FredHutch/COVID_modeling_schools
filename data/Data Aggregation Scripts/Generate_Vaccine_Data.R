####Generate County Vaccines####

#Author: Mia Moore
#Date: 7/08/2021
#Purpose: Import CDC vaccination numbers and adapt to our age groups

#input:
# wa_vaccines_master.Rdata

load("../wa_counties_master.Rdata")

#Divide up counties by age groups
wa.counties.master$AGEGROUP = as.numeric(wa.counties.master$AGEGROUP)
wa.counties.master$POP = as.numeric(wa.counties.master$POP)

wa.counties.master.by.county = split(wa.counties.master, wa.counties.master$County)

age1.prop.by.county = lapply(wa.counties.master.by.county, function(x)
{c(
  'age12-15' = 0.6 * filter(x, AGEGROUP == 3)$POP + 0.2 * filter(x, AGEGROUP == 4)$POP,
  'age16-17' = 0.4 * filter(x, AGEGROUP == 4)$POP,
  'age18-19' = 0.4 * filter(x, AGEGROUP == 4)$POP,
  'age20-49' = sum(filter(x, AGEGROUP %in% 5:10)$POP),
  'age50-65' = sum(filter(x, AGEGROUP %in% 11:13)$POP),
  'age65-69' = sum(filter(x, AGEGROUP %in% 14)$POP),
  'age70plus' = sum(filter(x, AGEGROUP %in% 15:18)$POP)
)}) 

#Relative vates of vaccinating age groups during different time intervals (note that only relative values matter)
#This is based approximately on the overall WA per capita rates presented by the DOH
vaccine.rate = list(
  #During the first period only cover health care workers
  "2020-01-01" = c(
    'age12-15' = 0,
    'age16-17' = 0,
    'age18-19' = 0,
    'age20-49' = 1,
    'age50-65' = 1,
    'age65-69' = 0,
    'age70plus' = 0
  ),
  #Shift to primarily older people
  "2021-01-15" = c(
    'age12-15' = 0,
    'age16-17' = 0,
    'age18-19' = .1,
    'age20-49' = .1,
    'age50-65' = .2,
    'age65-69' = 2,
    'age70plus' = 2
  ),
  #Shift age group again
  "2021-03-01" = c(
    'age12-15' = 0,
    'age16-17' = 0.1,
    'age18-19' = 0.1,
    'age20-49' = 0.5,
    'age50-65' = 1,
    'age65-69' = 3,
    'age70plus' = 3
  ),
  #Open to 16+
  "2021-04-15" = c(
    'age12-15' = 0,
    'age16-17' = 1,
    'age18-19' = 1,
    'age20-49' = 1,
    'age50-65' = 1,
    'age65-69' = 0.5,
    'age70plus' = 0.5
  ),
  #open to 12+
  "2021-05-15" = c(
    'age12-15' = 3,
    'age16-17' = 1,
    'age18-19' = 1,
    'age20-49' = 1,
    'age50-65' = 1,
    'age65-69' = 0.5,
    'age70plus' = 0.5
  ),
  #Moving forward, everyone the same, but older people are slightly lower
  "2021-06-01" = c(
    'age12-15' = 1,
    'age16-17' = 1,
    'age18-19' = 1,
    'age20-49' = 1,
    'age50-65' = 1,
    'age65-69' = 0.5,
    'age70plus' = 0.5
  )
)


#CDC provides the number of vaccines in three groups 12-17, 18-64, 65+.
#Here we specify the relationship between age groups
age.conversion = list(
  "12_17" = c(
    'age12-15' = 1, #This means that 100% of 12-15 year olds are between 12 and 17
    'age16-17' = 1,
    'age18-19' = 0,
    'age20-49' = 0,
    'age50-65' = 0,
    'age65-69' = 0,
    'age70plus' = 0
  ),
  "18_64" = c(
    'age12-15' = 0,
    'age16-17' = 0,
    'age18-19' = 1, #100% of 18-19 yearolds are 18-64
    'age20-49' = 1,
    'age50-65' = 1,
    'age65-69' = 0,
    'age70plus' = 0
  ),
  "65plus" = c(
    'age12-15' = 0,
    'age16-17' = 0,
    'age18-19' = 0,
    'age20-49' = 0,
    'age50-65' = 0,
    'age65-69' = 1,
    'age70plus' = 1
  )
)

get_CDC_vaccines = function(county){
  
  #API call to CDC
  vax_url = paste0("https://data.cdc.gov/resource/8xkx-amqh.csv?recip_state=WA&recip_county=", county)
  
  #Convert to data table
  vax.df = data.table::data.table(read.csv(vax_url))
  
  #Do some disaggegration
  
  #Step1: Calculate the populations (according to the CDC)
  #vax.df[, pop:= administered_dose1_recip/administered_dose1_pop_pct]
  #vax.df[, pop_12plus:= 100 * administered_dose1_recip_12plus/administered_dose1_recip_12pluspop_pct]
  #vax.df[, pop_18plus:= 100 * administered_dose1_recip_18plus/administered_dose1_recip_18pluspop_pct]
  #vax.df[, pop_65plus:= 100 * administered_dose1_recip_65plus/administered_dose1_recip_65pluspop_pct]
  
  #Step2: Derive further population sizes
  #vax.df[, pop_0_11:= pop - pop_12plus]
  #vax.df[, pop_12_17:= pop_12plus - pop_18plus]
  #vax.df[, pop_18_64:= pop_18plus - pop_65plus]
  
  #Isolate the number of doses administered to each age group
  vax.df[, administered_dose1_recip_0_11:= administered_dose1_recip - administered_dose1_recip_12plus]
  vax.df[, administered_dose1_recip_12_17:= administered_dose1_recip_12plus - administered_dose1_recip_18plus]
  vax.df[, administered_dose1_recip_18_64:= administered_dose1_recip_18plus - administered_dose1_recip_65plus]
  
  #vax.df[, administered_dose1_recip_0_11pop_pct:= 100 * administered_dose1_recip_0_11/pop_0_11]
  #vax.df[, administered_dose1_recip_12_17pop_pct:= 100 * administered_dose1_recip_12_17/pop_12_17]
  #vax.df[, administered_dose1_recip_18_64pop_pct:= 100 * administered_dose1_recip_18_64/pop_18_64]
  
  #Take the diff to get daily numbers
  vax.df[, daily_administered_dose1_recip_0_11:=-diff(c(administered_dose1_recip_0_11,0))]
  vax.df[, daily_administered_dose1_recip_12_17:=-diff(c(administered_dose1_recip_12_17,0))]
  vax.df[, daily_administered_dose1_recip_18_64:=-diff(c(administered_dose1_recip_18_64,0))]
  vax.df[, daily_administered_dose1_recip_65plus:=-diff(c(administered_dose1_recip_65plus,0))]
  
  
  dates = sort(as.Date(vax.df$date))
  
  #Keep track of how many are unvaccinated
  unvaccinated = age1.prop.by.county[[county]]
  
  #Initialize vax.by.age, which will store the number vaccinated by age and day
  vax.by.age = matrix(nrow = length(dates), ncol = length(unvaccinated))
  for(i in seq_along(dates)){
    
    #From the date, figure out which set of relative rates to use (adjust for different rollout to different age groups)
    entry.to.use = tail(which(dates[i]>=names(vaccine.rate)), 1)
    
    #Calculate the relative rate at which different age groups were vaccinated
    #Should be proportional to number of unvaccinated people ( an adjustment for age)
    #Note that we treat ages 12-17, 18-64, and 65+ as essentially three different vaccines
    rel.rate_12_17 = vaccine.rate[[entry.to.use]] * unvaccinated * age.conversion[["12_17"]]
    rel.rate_18_64 = vaccine.rate[[entry.to.use]] * unvaccinated * age.conversion[["18_64"]]
    rel.rate_65plus = vaccine.rate[[entry.to.use]] * unvaccinated * age.conversion[["65plus"]]
    
    
    #The absolute number of vaccines given on each day
    abs.number_12_17 = vax.df[as.Date(date)==dates[i]]$daily_administered_dose1_recip_12_17
    abs.number_18_64 = vax.df[as.Date(date)==dates[i]]$daily_administered_dose1_recip_18_64
    abs.number_65plus = vax.df[as.Date(date)==dates[i]]$daily_administered_dose1_recip_65plus
    
    #Normalize relative rates to sum to one (and avoid dividing by zero)
    rel.rate_12_17 = rel.rate_12_17/ifelse(sum(rel.rate_12_17)>0, sum(rel.rate_12_17), 1)
    rel.rate_18_64 = rel.rate_18_64/ifelse(sum(rel.rate_18_64)>0, sum(rel.rate_18_64), 1)
    rel.rate_65plus = rel.rate_65plus/ifelse(sum(rel.rate_65plus)>0, sum(rel.rate_65plus), 1)
    
    #Number vaccinated by age group
    newly.vaccinated = rel.rate_12_17 * abs.number_12_17 + rel.rate_18_64 * abs.number_18_64 + rel.rate_65plus * abs.number_65plus
    
    #Remove from unvaccinated class
    unvaccinated = unvaccinated - newly.vaccinated
    
    #Store result
    vax.by.age[i,] = newly.vaccinated
  }
  
  #Compile into data frame
  data.frame(
    date = dates,
    AGEGR1 = round(cumsum(rowSums(vax.by.age[, 1:3]))),
    AGEGR2 = round(cumsum(vax.by.age[, 4])),
    AGEGR3 = round(cumsum(rowSums(vax.by.age[, 5:6]))),
    AGEGR4 = round(cumsum(vax.by.age[, 7]))
  )
  
}


wa_county_vaccines = lapply(names(wa.counties.master.by.county), get_CDC_vaccines)

names(wa_county_vaccines) = names(wa.counties.master.by.county)

save(wa_county_vaccines, file = "../wa_county_vaccines.Rdata")
