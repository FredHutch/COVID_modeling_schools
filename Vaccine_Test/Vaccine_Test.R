
load("../data/wa_counties_age1_proportions.Rdata")


County = "King County"

totalpopulation = wa.counties.pop[[County]]

pop.proportion = totalpopulation/(sum(totalpopulation))
vaccines.table = data.table::data.table(wa_county_vaccines[[County]])
coverage = c()

vaccines.table.long = reshape(vaccines.table, 
        varying = c("AGEGR1", "AGEGR2", "AGEGR3", "AGEGR4"), 
        v.names = c("number_vaccines"),timevar = "age", 
        times = c("AGEGR1", "AGEGR2", "AGEGR3", "AGEGR4"), 
        direction = "long")


vaccines.table.long[,proportion.vaccinated := number_vaccines/totalpopulation[age],]


vaccines.table.long[,proportion.vaccinated.daily := c(diff(proportion.vaccinated, lag = 1),rep(0, 1))/1, by = age]


vaccines.table.long[,proportion.remaining.vaccinated := proportion.vaccinated.daily/(1 - proportion.vaccinated), by = age]

vaccines.table.long[age == "AGEGR4", coverage:=0.30,]
vaccines.table.long[age == "AGEGR4"& date>ymd("2021-01-15"), 
                    coverage:=0.9,]
vaccines.table.long[age == "AGEGR3", coverage:=0.30,]
vaccines.table.long[age == "AGEGR3"& date>ymd("2021-03-01"), 
                    coverage:=0.9 * 0.80,]
vaccines.table.long[age == "AGEGR3"& date>ymd("2021-04-01"), 
                    coverage:=0.9,]
vaccines.table.long[age == "AGEGR3"& date>ymd("2021-04-15"), 
                    coverage:=0.9,]

vaccines.table.long[age == "AGEGR2", coverage:=0.0,]
vaccines.table.long[age == "AGEGR2" & date>ymd("2021-02-28"), 
                    coverage:=0.5,]
vaccines.table.long[age == "AGEGR2"& date>ymd("2021-04-15"), 
                    coverage:=0.85,]
vaccines.table.long[age == "AGEGR1", coverage:=0.0,]
vaccines.table.long[age == "AGEGR1" & date>ymd("2021-04-15"), 
                    coverage:=.70 * (age1_proportion$`age18-19` + age1_proportion$`age16-17`),]
vaccines.table.long[age == "AGEGR1" & date>ymd("2021-05-15"), 
                    coverage:=.70 * (age1_proportion$`age18-19` + age1_proportion$`age16-17` + age1_proportion$`age12-15`),]

vaccines.table.long[,total_vaccines := 20000,]
vaccines.table.long[date<ymd("2021-04-01"),total_vaccines := 20000 * (date - ymd("2021-01-13"))/(as.numeric(ymd("2021-04-01") - ymd("2021-01-13"))),]
vaccines.table.long[date<ymd("2021-01-13"),total_vaccines := 0,]
nu = 0.05
vaccination.rate.base = function(fraction.vaccinated, coverage, proportion){
  pmax(0, coverage - fraction.vaccinated) *  proportion
}



vaccines.table.long[,vaccination.rate.base:=vaccination.rate.base(proportion.vaccinated, coverage, totalpopulation[age]),]
vaccines.table.long[,kv:=nu * total_vaccines/(total_vaccines + nu *sum(vaccination.rate.base)), by = date]

vaccines.table.long[,vaccination.rate.modeled:= vaccination.rate.base * kv,]


vaccines.table.long[,vaccination.rate.modeled.per.unvaccinated:= vaccination.rate.modeled/(totalpopulation[age] * (1 - proportion.vaccinated)),]


plot(vaccination.rate.modeled.per.unvaccinated~date, vaccines.table.long, subset = age == "AGEGR1", ylim = c(0, 0.03), type = 'l', lwd = 2)
points(proportion.remaining.vaccinated~date, vaccines.table.long, subset = age == "AGEGR1", pch = 16)

plot(vaccination.rate.modeled.per.unvaccinated~date, vaccines.table.long, subset = age == "AGEGR2", ylim = c(0, 0.03), type = 'l', lwd = 2)
points(proportion.remaining.vaccinated~date, vaccines.table.long, subset = age == "AGEGR2", pch = 16)

plot(vaccination.rate.modeled.per.unvaccinated~date, vaccines.table.long, subset = age == "AGEGR3", ylim = c(0, 0.03), type = 'l', lwd = 2)
points(proportion.remaining.vaccinated~date, vaccines.table.long, subset = age == "AGEGR3", pch = 16)

plot(vaccination.rate.modeled.per.unvaccinated~date, vaccines.table.long, subset = age == "AGEGR4", ylim = c(0, 0.03), type = 'l', lwd = 2)
points(proportion.remaining.vaccinated~date, vaccines.table.long, subset = age == "AGEGR4", pch = 16)
