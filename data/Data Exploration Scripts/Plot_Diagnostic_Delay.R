####Plot Diagnostic Delay####

#Author: Mia Moore
#Date: 3/25/2021
#Purpose: Plot delay between symptoms and diagnosis

#Input:
# delay.by.date.Rdata

#Output:
# Days_since_symptoms.png
# Time_to_hospitalization.png
# Time_to_symptoms.png

Plot_Diagnostic_Delay = function(working_folder = "../WA Data Exploration/"
){
  
load(paste0(working_folder, "optimized.delay.Rdata"))

age.groups = c("Age 0-19", "Age 20-49", "Age 50-69", "Age 70+")
plot.delay = function(delay.data, age.group, names.parameters = c("Slope 1", "Slope 2", "Rel. Asym. Diag."),
                      parameters.to.plot = seq(3)){
  plot(NUMBER~DELAY, 
       data = delay.data$data, 
       xlim = c(-7, 21), 
       xlab = "Day Post Symptom Onset",
       ylab = "Cases", 
       main = age.group, log = 'y')
  
  lines(fitted.value~DELAY, 
        data = delay.data$data, 
        col = "red")
  
  y2 = max(delay.data$data$NUMBER)
  y1 = pmax(min(delay.data$data$NUMBER), 1)
  text(-3,
       y1 * (y2/y1)^seq(.25, by = -.1, length.out = length(names.parameters)),
       paste(names.parameters, 
              "=", 
              round(delay.data$optimal.parameter[parameters.to.plot], 2), 
              "+/-",
              round(delay.data$standard.errors[parameters.to.plot], 3)),
       pos = 4)
}


png(paste0(working_folder,"Days_since_symptoms.png"), units = "in", res = 300, width = 12, height = 8)
par(mfrow=c(2, 2))
mapply(plot.delay, optimized.delay, age.groups)
dev.off()

png(paste0(working_folder,"Time_to_hospitalization.png"), units = "in", res = 300, width = 12, height = 8)
par(mfrow=c(2, 2))
mapply(plot.delay, optimized.delay.hosp, age.groups, names.parameters = "Slope 2", parameters.to.plot = 2)
dev.off()

png(paste0(working_folder,"Time_to_death.png"), units = "in", res = 300, width = 12, height = 4)
par(mfrow=c(1, 2))
mapply(plot.delay, optimized.delay.death[3:4], age.groups, names.parameters = "Slope 2", parameters.to.plot = 1)
dev.off()
}