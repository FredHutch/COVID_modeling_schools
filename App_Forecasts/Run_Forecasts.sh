#!/bin/sh
source /app/lmod/lmod/init/profile
ml R/4.1.0-foss-2020b

input_dir="scenario_definition/"
output_dir="data/"

mkdir "$output_dir"
scenario_folder="${output_dir}/scenarios/"
mkdir "$scenario_folder"


Rscript Setup_Scenarios.R --args input_path="${input_dir}" output_path="${output_dir}" scenario_folder="${scenario_folder}"

for scenario in "$scenario_folder"/*.Rdata; do
Rscript PreRun_Scenarios.R --args scenario_file="${scenario}" output_path="${scenario_folder}" n_cores=$1
done

Rscript Calculate_Max_Vals.R --args input_path="${scenario_folder}" output_path="${output_dir}"