####Setup App Data####

#Author: Mia Moore
#Date: 7/23/2021
#Purpose: Setup other data objects to be passed to the app

#input:
# data/KC_Data
# wa_county_vaccines.Rdata
# wa_counties_master.csv


#output:
# actuals.rds
# ds.maxvals.rds


#Contains the following immune states:

load("../data/KC_Data/DATA_AGGR.Rdata")
load("../data/wa_county_vaccines.Rdata")
load("../data/wa_counties_aggr.Rdata")




County = "King County"

vaccine.history = data.table::data.table(wa_county_vaccines[[County]])

vaccine.by.date = vaccine.history[,vax := AGEGR1 + AGEGR2 + AGEGR3 + AGEGR4,by = "date"]

DATA_AGGR = data.table::data.table(DATA_AGGR)

DATA_AGGR[,DEATHSUM := sum(DEATHS),by = "DATE"]
DATA_AGGR[,CASESUM := sum(CASES),by = "DATE"]
DATA_AGGR[,HOSPSUM := sum(HOSPITALIZATIONS),by = "DATE"]

DATA_AGGR = DATA_AGGR[AGEGR == 0, , ]


DATA_AGGR = merge(DATA_AGGR, subset(vaccine.by.date, select = c("date", "vax")), by.x = "DATE", by.y = "date", all.x = T)
DATA_AGGR[is.na(vax),vax:=0,]
DATA_AGGR[,c("AGEGR", "RACE", "DEATHS", "CASES", "HOSPITALIZATIONS", "WEEK", "COUNTY"):=NULL]

ds.actuals = data.table::data.table(
  Date = rep(DATA_AGGR$DATE, 4),
  change = c(DATA_AGGR$DEATHSUM, DATA_AGGR$CASESUM, DATA_AGGR$HOSPSUM, pmax(0, diff(c(0, DATA_AGGR$vax)))),
  series = rep(c("deaths", "cases", "cum_hosp", "vax"), each = nrow(DATA_AGGR))
)

ds.actuals[,value := cumsum(change),by = series]

saveRDS(ds.actuals, file = "data/actual.rds")
