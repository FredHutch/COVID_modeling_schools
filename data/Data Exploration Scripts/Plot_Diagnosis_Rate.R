####Plot Diagnosis Rate####

#Author: Mia Moore
#Date: 4/11/2021
#Purpose: Visualize WA Data

#Input:
# KINGCOV_Data_ALT1_WEEKLY.Rdata


Plot_Diagnostic_Rate = function(working_folder = "../WA Data Exploration/",
                                    MONTHS_TO_PLOT = seq(3, 14)
){

   load(paste0(working_folder, "diagnosis.data.Rdata"))
col.list = c("indianred", "skyblue", "seagreen", "goldenrod")

diagnosis.data$WEIGHT = log(diagnosis.data$WAITING + 1)/3
png(paste0(working_folder, "Diagnosis.png"), width = 12, height = 5, units = "in", res = 300)
par(mfrow = c(1, 3), cex.main = 1.5, cex.lab = 1.5)

plot(I(log((DIAGNOSED + WAITING)/WAITING))~WEEK, diagnosis.data,
     subset = MONTH%in%MONTHS_TO_PLOT & SEVERITY == "MILD", ylim = c(0.02, 10), cex = WEIGHT, col = col.list[AGEGR + 1], pch = 16, log = 'y', main = "Mild Diagnosis", ylab = "DELTA")

lines(DELTA~WEEK, diagnosis.data,
      subset = AGEGR == 0 & MONTH%in%MONTHS_TO_PLOT  & SEVERITY == "MILD" , col = col.list[1])

lines(DELTA~WEEK, diagnosis.data,
     subset = AGEGR == 1 & MONTH%in%MONTHS_TO_PLOT & SEVERITY == "MILD" , col = col.list[2] )

lines(DELTA~WEEK, diagnosis.data,
      subset = AGEGR == 2 & MONTH%in%MONTHS_TO_PLOT  & SEVERITY == "MILD" , col = col.list[3] )


lines(DELTA~WEEK, diagnosis.data,
      subset = AGEGR == 3 & MONTH%in%MONTHS_TO_PLOT & SEVERITY == "MILD" , col = col.list[4] )

legend("topleft", legend = c("0-19", "20-49", "50-69", "70+"), col = col.list, pch = 16, cex = 1.5)

plot(I(log((DIAGNOSED + WAITING)/WAITING))~WEEK, diagnosis.data,
     subset = MONTH%in%MONTHS_TO_PLOT & SEVERITY == "SEVERE_EARLY", ylim = c(0.02, 10), cex = WEIGHT, col = col.list[AGEGR + 1], pch = 16, log = 'y', main = "Severe Diagnosis\n Prior to Hospitalization/Death", ylab = "DELTA")

lines(DELTA~WEEK, diagnosis.data,
      subset = AGEGR == 0 & MONTH%in%MONTHS_TO_PLOT  & SEVERITY == "SEVERE_EARLY" , col = col.list[1])

lines(DELTA~WEEK, diagnosis.data,
      subset = AGEGR == 1 & MONTH%in%MONTHS_TO_PLOT & SEVERITY == "SEVERE_EARLY" , col = col.list[2] )

lines(DELTA~WEEK, diagnosis.data,
      subset = AGEGR == 2 & MONTH%in%MONTHS_TO_PLOT  & SEVERITY == "SEVERE_EARLY" , col = col.list[3] )


lines(DELTA~WEEK, diagnosis.data,
      subset = AGEGR == 3 & MONTH%in%MONTHS_TO_PLOT & SEVERITY == "SEVERE_EARLY" , col = col.list[4] )


plot(I(log((DIAGNOSED + WAITING)/WAITING))~WEEK, diagnosis.data,
     subset = MONTH%in%MONTHS_TO_PLOT & SEVERITY == "SEVERE_LATE", ylim = c(0.02, 10), cex = WEIGHT, col = col.list[AGEGR + 1], pch = 16, log = 'y', main = "Severe Diagnosis\n After Hospitalization/Death", ylab = "DELTA")

lines(DELTA~WEEK, diagnosis.data,
      subset = AGEGR == 0 & MONTH%in%MONTHS_TO_PLOT  & SEVERITY == "SEVERE_LATE" , col = col.list[1])

lines(DELTA~WEEK, diagnosis.data,
      subset = AGEGR == 1 & MONTH%in%MONTHS_TO_PLOT & SEVERITY == "SEVERE_LATE" , col = col.list[2] )

lines(DELTA~WEEK, diagnosis.data,
      subset = AGEGR == 2 & MONTH%in%MONTHS_TO_PLOT  & SEVERITY == "SEVERE_LATE" , col = col.list[3] )


lines(DELTA~WEEK, diagnosis.data,
      subset = AGEGR == 3 & MONTH%in%MONTHS_TO_PLOT & SEVERITY == "SEVERE_LATE" , col = col.list[4] )

dev.off()
}