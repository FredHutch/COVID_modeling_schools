# calculate the proportion of age slices in age group 1
# we want to know the following: 0-4, 5-11, 12-15, 16-17, 18-19 for vaccine eligibility of 5+, 12+, 16+, 18+
# we have data on population by county of ag1 = 0-4, ag2 = 5-9, ag3 = 10-14, ag4 = 15-19 and ag0 = total pop
# we will assume linear distribution of ages giving
# prop1-4 = ag1 / sum(ag1:4)
# prop5-11 = (ag2 + 0.4 * ag3) / sum(ag1:4)
# prop12-15 = (0.6 * ag3 + 0.2 * ag4) / sum(ag1:4)
# prop16-17 = 0.4 * ag4 / sum(ag1:4)
# prop18-19 = 0.4 * ag4 /sum(ag1:4)


#input:
# wa_counties_master.Rdata

load("../wa_counties_master.Rdata")
wa.counties.master$AGEGROUP = as.numeric(wa.counties.master$AGEGROUP)
wa.counties.master$POP = as.numeric(wa.counties.master$POP)

wa.counties.master.by.county = split(wa.counties.master, wa.counties.master$County)

age1.prop.by.county = lapply(wa.counties.master.by.county, function(x)
  {list(
    'age1-4' = filter(x, AGEGROUP == 1)$POP / sum(filter(x, AGEGROUP %in% 1:4)$POP),
    'age5-11' = (filter(x, AGEGROUP == 2)$POP + 0.4 * filter(x, AGEGROUP == 3)$POP) / sum(filter(x, AGEGROUP %in% 1:4)$POP),
    'age12-15' = (0.6 * filter(x, AGEGROUP == 3)$POP + 0.2 * filter(x, AGEGROUP == 4)$POP) / sum(filter(x, AGEGROUP %in% 1:4)$POP),
    'age16-17' = 0.4 * filter(x, AGEGROUP == 4)$POP / sum(filter(x, AGEGROUP %in% 1:4)$POP),
    'age18-19' = 0.4 * filter(x, AGEGROUP == 4)$POP / sum(filter(x, AGEGROUP %in% 1:4)$POP)
  )}) 

# verify should all sum to 1
lapply(age1.prop.by.county, function(x) sum(unlist(x)))

save(age1.prop.by.county, file = "../wa_counties_age1_proportions.Rdata")