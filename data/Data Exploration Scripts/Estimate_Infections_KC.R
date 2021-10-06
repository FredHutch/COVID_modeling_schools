target_dir = "../KC Data Exploration/"
resamples = 200
weighting = 1
#load(paste0(target_dir, "DATA_AGGR_WEEKLY.Rdata"))

Slice_Date = "2021-04-01"
kc_pop = 2190000 #2118119                         # King County population
kc_age_prop = c(0.2293,	0.4552,	0.2350,	0.0805)   # King County age distribution


#source("Assess_Reff.R")
#col.list = c("indianred", "skyblue", "seagreen", "goldenrod")
data.infections.full = Reduce('rbind', lapply(rep(weighting, resamples), Compute.Infections, Resample = T, Normalization = kc_pop*kc_age_prop))


get.quantiles = function(X){
  q = quantile(X$INFECTIONS, probs = c(0.5, .025, .975))
  q2 = quantile(X$CUM.INFECTIONS, probs = c(0.5, .025, .975))
  data.frame(WEEK.ENDING = X$ENDDT[1],
             INFECTIONS.MED = q[1],
             INFECTIONS.LO = q[2],
             INFECTIONS.HI = q[3],
             CUM.INFECTIONS.MED = q2[1],
             CUM.INFECTIONS.LO = q2[2],
             CUM.INFECTIONS.HI = q2[3])
}

date.list = as.Date(c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01", "2021-04-01"))
data.infections = ddply(data.infections.full, .variables = c("WEEK", "AGEGR"), .fun = get.quantiles)

png(paste0(target_dir, "Infections_History.png"), units = "in", width = 9, height = 6, res = 300)
par(mfrow = c(1, 2))
plot(I(100000 * INFECTIONS.MED)~WEEK.ENDING, 
     data.infections, 
     pch = 16, 
     col = col.list[AGEGR + 1], 
     type ='n', 
     xaxt = 'n',
     ylim = c(0, max(100000*data.infections$INFECTIONS.HI)),
     ylab = "Infections per 100K",
     las = 1,
     xlab = "Date",
     main = "Weekly Infections By Age")


legend("topleft", legend = c("0-19 Years", "20-49 Years", "50-69 Years", "70+ Years"), pch = 15, col = col.list)
for(i in seq(0, 3)){
  dat.i = subset(data.infections, AGEGR == i)
  polygon(c(dat.i$WEEK.ENDING, rev(dat.i$WEEK.ENDING)), 
          c(100000 * dat.i$INFECTIONS.LO, rev(100000 * dat.i$INFECTIONS.HI)), 
          col = col.list[i + 1], 
          border = NA)
}

axis(1, at = date.list, format(date.list, "%b-%Y"))


plot(CUM.INFECTIONS.MED~WEEK.ENDING, 
     data.infections, 
     pch = 16, 
     col = col.list[AGEGR + 1], 
     type ='n',
     xaxt = 'n',
     ylab = "Cumulative Infections",
     ylim = c(0, max(data.infections$CUM.INFECTIONS.HI)),
     las = 1,
     xlab = "Date",
     main = "Cumulative Incidence By Age")
axis(1, at = date.list, format(date.list, "%b-%Y"))
for(i in seq(0, 3)){
  dat.i = subset(data.infections, AGEGR == i)
  polygon(c(dat.i$WEEK.ENDING, rev(dat.i$WEEK.ENDING)), 
          c(dat.i$CUM.INFECTIONS.LO, rev(dat.i$CUM.INFECTIONS.HI)), 
          col = col.list[i + 1],
          border = NA)
}

legend("topleft", legend = c("0-19 Years", "20-49 Years", "50-69 Years", "70+ Years"), pch = 15, col = col.list)
dev.off()