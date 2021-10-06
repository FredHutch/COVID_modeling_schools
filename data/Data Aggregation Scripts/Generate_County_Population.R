####Generate County Population####

#Author: Mia Moore
#Date: 6/7/2021
#Purpose: Get census populations by age group and county

#input:
# wa_counties_master.Rdata

load("../wa_counties_master.Rdata") #Note that this is from a census API call that is not stored on github

collate_counties = function(AGEGROUPINGS = list(
  list(AGEGR = "AGEGR1", AGEGROUPS = seq(4)),
  list(AGEGR = "AGEGR2", AGEGROUPS = seq(5, 10)),
       list(AGEGR = "AGEGR3", AGEGROUPS = seq(11, 14)),
            list(AGEGR = "AGEGR4", AGEGROUPS = seq(15, 18))
)){
  
  
  get.single.age = function(grouping, x){
    data.frame(
      population = sum(as.numeric(subset(x, AGEGROUP %in% grouping$AGEGROUPS)$POP)),
      AGEGR = grouping$AGEGR
    )
  }
  
  get.single.county = function(x){
    .x = Reduce('rbind', lapply(AGEGROUPINGS, get.single.age, x))
    .y = .x$population
    names(.y) = .x$AGEGR
    .y
  }
  
  plyr::dlply(wa.counties.master, "County", get.single.county)
}


wa.counties.pop = collate_counties()

wa.counties.pop.extend = collate_counties(
  AGEGROUPINGS = list(
    list(AGEGR = "V1", AGEGROUPS = 1),
    list(AGEGR = "V2", AGEGROUPS = 2),
    list(AGEGR = "V3", AGEGROUPS = 3),
    list(AGEGR = "V4", AGEGROUPS = 4),
    list(AGEGR = "V5", AGEGROUPS = 5),
    list(AGEGR = "V6", AGEGROUPS = 6),
    list(AGEGR = "V7", AGEGROUPS = 7),
    list(AGEGR = "V8", AGEGROUPS = 8),
    list(AGEGR = "V9", AGEGROUPS = 9),
    list(AGEGR = "V10", AGEGROUPS = 10),
    list(AGEGR = "V11", AGEGROUPS = 11),
    list(AGEGR = "V12", AGEGROUPS = 12),
    list(AGEGR = "V13", AGEGROUPS = 13),
    list(AGEGR = "V14", AGEGROUPS = 14),
    list(AGEGR = "V15", AGEGROUPS = 15),
    list(AGEGR = "V16", AGEGROUPS = 16),
    list(AGEGR = "V17", AGEGROUPS = seq(17, 18))
  )
)

save(wa.counties.pop, file = "../wa_counties_aggr.Rdata")
save(wa.counties.pop.extend, file = "../wa_counties_aggr_extend.Rdata")