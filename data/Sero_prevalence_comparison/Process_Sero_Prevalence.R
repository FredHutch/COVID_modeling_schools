
library(stringr)

#sero.raw = read.csv("Nationwide_Commercial_Laboratory_Seroprevalence_Survey.csv")

sero.raw = read.csv("https://data.cdc.gov/resource/d2tw-32xv.csv")
sero.raw$n..0.17.Years.Prevalence. * sero.raw$Rate......0.17.Years.Prevalence.


sero.wa = subset(sero.raw, site=="WA")

dates.raw = Reduce('rbind', strsplit(sero.wa$date_range_of_specimen, " - "))

end.date = as.Date(str_trim(dates.raw[,2]), "%b %d, %Y")
start.date1 = as.Date(dates.raw[,1], "%b %d, %Y")
start.date2 = as.Date(dates.raw[,1], "%b %d")
year(start.date2) = year(end.date)
start.date = start.date1
start.date[is.na(start.date1)] = start.date2[is.na(start.date1)]

# sero.wa$n..0.17.Years.Prevalence.
# 
# sero.wa$Rate......Cumulative.Prevalence.[sero.wa$Rate......Cumulative.Prevalence.>100] = 0
# sero.wa$Rate......0.17.Years.Prevalence.[sero.wa$Rate......0.17.Years.Prevalence.>100] = 0
# sero.wa$Rate......18.49.Years.Prevalence.[sero.wa$Rate......18.49.Years.Prevalence.>100] = 0
# sero.wa$Rate......50.64.Years.Prevalence.[sero.wa$Rate......50.64.Years.Prevalence.>100] = 0
# sero.wa$Rate......65..Years.Prevalence.[sero.wa$Rate......65.Years.Prevalence.>100] = 0
# 
# sero.data.WA = data.frame(window.start = start.date, 
#                     window.end = end.date,
#                     tests = sero.wa$n..Cumulative.Prevalence.,
#                     tests.0.17 = sero.wa$n..0.17.Years.Prevalence.,
#                     tests.18.49 = sero.wa$n..18.49.Years.Prevalence.,
#                     tests.50.64 = sero.wa$n..50.64.Years.Prevalence.,
#                     tests.65 = sero.wa$n..65..Years.Prevalence.,
#                     positive = round(0.01 * sero.wa$n..Cumulative.Prevalence. * sero.wa$Rate......Cumulative.Prevalence.),
#                     positive.0.17 = round(0.01 * sero.wa$n..0.17.Years.Prevalence. * sero.wa$Rate......0.17.Years.Prevalence.),
#                     positive.18.49 = round(0.01 * sero.wa$n..18.49.Years.Prevalence. * sero.wa$Rate......18.49.Years.Prevalence.),
#                     positive.50.64 = round(0.01 * sero.wa$n..50.64.Years.Prevalence. * sero.wa$Rate......50.64.Years.Prevalence.),
#                     positive.65 = round(0.01 * sero.wa$n..65..Years.Prevalence. * sero.wa$Rate......65..Years.Prevalence.)
# )

AGEGR.list = seq(0, 3)


sero.data.WA = data.frame(
  AGEGR = rep(AGEGR.list, each = length(start.date)),
  window.start = rep(start.date, length(AGEGR.list)), 
                          window.end = rep(end.date, length(AGEGR.list)),
                          sero.lower = c(sero.wa$lower_ci_0_17_prevalence,
                                         sero.wa$lower_ci_18_49_prevalence,
                                         sero.wa$lower_ci_50_64_prevalence,
                                         sero.wa$lower_ci_65_prevalence),
                          sero.upper = c(sero.wa$upper_ci_0_17_prevalence,
                                         sero.wa$upper_ci_18_49_prevalence,
                                         sero.wa$upper_ci_50_64_prevalence,
                                         sero.wa$upper_ci_65_prevalence)
)


sero.data.WA$week.start = ceiling(as.numeric(sero.data.WA$window.start - as.Date("2019-12-31"))/7)

sero.data.WA$week.end = ceiling(as.numeric(sero.data.WA$window.end - as.Date("2019-12-31"))/7)
#sero.data.WA$tests.extra = sero.data.WA$tests - (sero.data.WA$tests.65 + sero.data.WA$tests.50.64 + sero.data.WA$tests.18.49 + sero.data.WA$tests.0.17)

#sero.data.WA$positive.extra = sero.data.WA$positive - (sero.data.WA$positive.65 + sero.data.WA$positive.50.64 + sero.data.WA$positive.18.49 + sero.data.WA$positive.0.17)

#save(sero.data.WA, file = "sero_prevalence.Rdata")

#write.csv(sero.data.WA, file = "sero_prevalence.csv")