# analyze/ plot results of RunRollout.R

library(HutchCOVID)
source("kc_read_data.R")
source("covid-model-plotting.R")
load("../data/wa_counties_aggr.Rdata")

# data from Run_Rollout needed for plotting
County = "King County" 

#Population pulled from census data
total_population = wa.counties.pop[[County]]
names(total_population) = paste0("a", 1:4)

scenario_names = c("A", "B", "C", "D")
#scenario_names = c("A")

# load scenario results
out_long = list()
out_wide = list()


for (i in 1:length(scenario_names))
{
  load(file = paste0("out/scenario_", scenario_names[i], "_data.rdata"))
  out_long[[scenario_names[i]]] = scen_out
  rm(list = c("scen_out", "wide_out"))
  gc()
  print(paste("long", i))
}

pdf("out/scenario_strain_inf_coverage_detail.pdf", width = 8, height = 8)
par(mfrow = c(2,2))
# all cases
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=-1,metric="cases",title = paste("Scenario", scen,"Daily cases"),by_age=FALSE,y_lim=c(0,1500))
}

# vaccinated cases
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=1,metric="cases",title = paste("Scenario", scen,"Daily vaccinated cases"),by_age=FALSE,y_lim=c(0,1500))
}
# unvaccinated cases
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=0,metric="cases",title = paste("Scenario", scen,"Daily unvaccinated cases"),by_age=FALSE,y_lim=c(0,1500))
}
# vaccinated cases
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=1,metric="cases",title = paste("Scenario", scen,"Daily vaccinated cases"),y_lim=c(0,1500))
}
# unvaccinated cases
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=0,metric="cases",title = paste("Scenario", scen,"Daily unvaccinated cases"),y_lim=c(0,1500))
}

# all infections
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=-1,metric="inf",title = paste("Scenario", scen,"Daily infs"),by_age=FALSE,y_lim=c(0,6000))
}

# vaccinated infections
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=1,metric="inf",title = paste("Scenario", scen,"Daily vaccinated infs"),by_age=FALSE,y_lim=c(0,6000))
}
# unvaccinated infections
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=0,metric="inf",title = paste("Scenario", scen,"Daily unvaccinated infs"),by_age=FALSE,y_lim=c(0,6000))
}
# vaccinated infections
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=1,metric="inf",title = paste("Scenario", scen,"Daily vaccinated infs"),by_age=TRUE,y_lim=c(0,3000))
}
# unvaccinated infections
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=0,metric="inf",title = paste("Scenario", scen,"Daily unvaccinated infs"),by_age=TRUE,y_lim=c(0,3000))
}

# all hospitalizations
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=-1,metric="hosp",title = paste("Scenario", scen,"Daily hosp admins"),by_age=FALSE,y_lim=c(0,60))
}
# vaccinated hospitalizations
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=1,metric="hosp",title = paste("Scenario", scen,"Daily vaccinated hosp admins"),by_age=FALSE,y_lim=c(0,60))
}
# unvaccinated hospitalizations
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=0,metric="hosp",title = paste("Scenario", scen,"Daily unvaccinated hosp admins"),by_age=FALSE,y_lim=c(0,60))
}

# vaccinated hospitalizations
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=1,metric="hosp",title = paste("Scenario", scen,"Daily vaccinated hosp admins"),y_lim=c(0,50))
}
# unvaccinated hospitalizations
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=0,metric="hosp",title = paste("Scenario", scen,"Daily unvaccinated hosp admins"),y_lim=c(0,50))
}

# all deaths
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=-1,metric="death",title = paste("Scenario", scen,"Daily deaths"), by_age=FALSE,y_lim=c(0,15))
}
# vaccinated deaths
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=1,metric="death",title = paste("Scenario", scen,"Daily vaccinated deaths"), by_age=FALSE,y_lim=c(0,12))
}
# unvaccinated deaths
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=0,metric="death",title = paste("Scenario", scen,"Daily unvaccinated deaths"), by_age=FALSE,y_lim=c(0,12))
}

# vaccinated deaths
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=1,metric="death",title = paste("Scenario", scen,"Daily vaccinated deaths"), y_lim=c(0,12))
}
# unvaccinated deaths
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_long[[scen]], vac_status=0,metric="death",title = paste("Scenario", scen,"Daily unvaccinated deaths"), y_lim=c(0,12))
}

# vaccine coverage
for (scen in scenario_names)
{
  plot_vac_across_compartments_from_long_set(out_long[[scen]], total_population, c("mRNA", "R_vax"), sum_vacs = TRUE, title = paste("Scenario", scen))
}

# strains
for (scen in scenario_names)
{
  plot_strain_proportion_current_inf(out_long[[scen]], title = paste("Scenario", scen))
}

# sd
for (scen in scenario_names)
{
  plot_sd_from_long_set(out_long[[scen]], title = paste("Scenario", scen))
}

# Reff
for (scen in scenario_names)
{
  plot_r_eff_from_long_list(out_long[[scen]], title = paste("Scenario", scen))
}

dev.off()

rm(list = c("out_long"))
gc()

############# KIDS ################################################################################################

out_kids_long = list()
out_kids_wide = list()

for (i in 1:length(scenario_names))
{
  load(file = paste0("out/scenario_KIDS_", scenario_names[i], "_data.rdata"))
  out_kids_long[[scenario_names[i]]] = scen_out
  rm(list = c("scen_out", "wide_out"))
  gc()
  print(i)
}

pdf("out/scenario_KIDS_strain_inf_coverage_details.pdf", width = 8, height = 8)
par(mfrow = c(2,2))

# all cases
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=-1,metric="cases",title = paste("Scenario", scen,"Daily cases"),by_age=FALSE,y_lim=c(0,1500))
}
# vaccinated cases
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=1,metric="cases",title = paste("Scenario", scen,"Daily vaccinated cases"),by_age=FALSE,y_lim=c(0,1500))
}
# unvaccinated cases
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=0,metric="cases",title = paste("Scenario", scen,"Daily unvaccinated cases"),by_age=FALSE,y_lim=c(0,1500))
}
# vaccinated cases
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=1,metric="cases",title = paste("Scenario", scen,"Daily vaccinated cases"),y_lim=c(0,1500))
}
# unvaccinated cases
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=0,metric="cases",title = paste("Scenario", scen,"Daily unvaccinated cases"),y_lim=c(0,1500))
}

# all infections
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=-1,metric="inf",title = paste("Scenario", scen,"Daily infs"),by_age=FALSE,y_lim=c(0,6000))
}
# vaccinated infections
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=1,metric="inf",title = paste("Scenario", scen,"Daily vaccinated infs"),by_age=FALSE,y_lim=c(0,6000))
}
# unvaccinated infections
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=0,metric="inf",title = paste("Scenario", scen,"Daily unvaccinated infs"),by_age=FALSE,y_lim=c(0,6000))
}
# vaccinated infections
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=1,metric="inf",title = paste("Scenario", scen,"Daily vaccinated infs"),by_age=TRUE,y_lim=c(0,3000))
}
# unvaccinated infections
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=0,metric="inf",title = paste("Scenario", scen,"Daily unvaccinated infs"),by_age=TRUE,y_lim=c(0,3000))
}

# all hospitalizations
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=-1,metric="hosp",title = paste("Scenario", scen,"Daily hosp admins"),by_age=FALSE,y_lim=c(0,60))
}
# vaccinated hospitalizations
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=1,metric="hosp",title = paste("Scenario", scen,"Daily vaccinated hosp admins"),by_age=FALSE,y_lim=c(0,60))
}
# unvaccinated hospitalizations
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=0,metric="hosp",title = paste("Scenario", scen,"Daily unvaccinated hosp admins"),by_age=FALSE,y_lim=c(0,60))
}

# vaccinated hospitalizations
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=1,metric="hosp",title = paste("Scenario", scen,"Daily vaccinated hosp admins"),y_lim=c(0,50))
}
# unvaccinated hospitalizations
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=0,metric="hosp",title = paste("Scenario", scen,"Daily unvaccinated hosp admins"),y_lim=c(0,50))
}

# all deaths
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=-1,metric="death",title = paste("Scenario", scen,"Daily deaths"), by_age=FALSE,y_lim=c(0,15))
}
# vaccinated deaths
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=1,metric="death",title = paste("Scenario", scen,"Daily vaccinated deaths"), by_age=FALSE,y_lim=c(0,12))
}
# unvaccinated deaths
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=0,metric="death",title = paste("Scenario", scen,"Daily unvaccinated deaths"), by_age=FALSE,y_lim=c(0,12))
}

# vaccinated deaths
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=1,metric="death",title = paste("Scenario", scen,"Daily vaccinated deaths"), y_lim=c(0,12))
}
# unvaccinated deaths
for (scen in scenario_names)
{
  plot_metric_by_vac_status(out_kids_long[[scen]], vac_status=0,metric="death",title = paste("Scenario", scen,"Daily unvaccinated deaths"), y_lim=c(0,12))
}


# vaccine coverage
for (scen in scenario_names)
{
  plot_vac_across_compartments_from_long_set(out_kids_long[[scen]], total_population, c("mRNA", "R_vax"), sum_vacs = TRUE, title = paste("Scenario", scen))
}

# strains
for (scen in scenario_names)
{
  plot_strain_proportion_current_inf(out_kids_long[[scen]], title = paste("Scenario", scen))
}

# sd
for (scen in scenario_names)
{
  plot_sd_from_long_set(out_kids_long[[scen]], title = paste("Scenario", scen))
}

# Reff
for (scen in scenario_names)
{
  plot_r_eff_from_long_list(out_kids_long[[scen]], title = paste("Scenario", scen))
}

dev.off()


