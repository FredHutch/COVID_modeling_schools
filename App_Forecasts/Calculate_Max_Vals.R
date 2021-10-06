cwd = getwd()
containing_dir = strsplit(cwd, "COVID_modeling", fixed = T)[[1]][1]
setwd(paste0(containing_dir, "COVID_modeling/R-proj"))

source("../App_Forecasts/functions.R")

#Shift back to where you started
setwd(cwd)

library(R.utils)

args = commandArgs(trailingOnly=TRUE, asValues = TRUE, 
                   defaults = c(input_path = "data/scenarios/",
		   output_path = "data/")
)


scen_file_list = list.files(args$input_path, pattern = "*.rds", full.names = T)


rds_list = lapply(scen_file_list, readRDS)
rds_complete = Reduce('rbind', rds_list)
ds.maxvals = get.max(rds_complete)

saveRDS(ds.maxvals, file = paste0(args$output_path, "ds.maxvals.rds"))