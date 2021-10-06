####Project Sero-Prevalence####

#Author: Mia Moore
#Date: 4/18/12021
#Purpose: Compare projected sero prevalence to data

#Input:
# WA_Data_ALT1_WEEKLY.Rdata

#Output:


#Generate 100 samples from priors
#SERO.samples = ldply(seq(100), SERO.samp, c(.004, .012, .05, .20))

collapse.by.week = ddply(SERO.samples, 
                         .variables = c("AGEGR", "week.start"),
                         .fun = function(x){
                           data.frame(
                             data.hi = median(x$SERO.UPPER),
                             data.lo = median(x$SERO.LOWER),
                             sero.med = quantile(x$SEROPOSITIVE, .5),
                             sero.lo = quantile(x$SEROPOSITIVE, .025),
                             sero.hi = quantile(x$SEROPOSITIVE, .975),
                             cum.med = quantile(x$CUMINC, .5),
                             cum.lo = quantile(x$CUMINC, .025),
                             cum.hi = quantile(x$CUMINC, .975))
                         })

col.list2 = c("lightpink", "lightblue", "darkseagreen", "yellow")
offset = 0.2

par(mfrow=c(2, 2))
for(i in seq(4)){
plot(data.hi~week.start, 
     col = col.list[AGEGR + 1], 
     data = subset(collapse.by.week, AGEGR == i-1), 
     pch = 16, 
     ylim = c(0, 20),
     ylab = "Sero-Positive",
     xlab = "Date")

points(data.lo~week.start, col = col.list[AGEGR + 1], data = subset(collapse.by.week, AGEGR == i-1), pch = 16)

segments(subset(collapse.by.week, AGEGR == i-1)$week.start, 
         subset(collapse.by.week, AGEGR == i-1)$data.lo, 
         y1 = subset(collapse.by.week, AGEGR == i-1)$data.hi)

points(sero.lo~I(week.start + offset), col = col.list2[AGEGR + 1], data = subset(collapse.by.week, AGEGR == i-1), pch = 16)

points(sero.hi~I(week.start + offset), col = col.list2[AGEGR + 1], data = subset(collapse.by.week, AGEGR == i-1), pch = 16)

segments(subset(collapse.by.week, AGEGR == i-1)$week.start + offset, 
         subset(collapse.by.week, AGEGR == i-1)$sero.lo, 
         y1 = subset(collapse.by.week, AGEGR == i-1)$sero.hi,
         lty = 2,
         lwd = 2,
         col = col.list2[i])
}