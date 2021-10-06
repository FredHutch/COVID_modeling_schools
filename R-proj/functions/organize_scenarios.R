
#' Consolidate summary metrics across multiple scenarios run in different rdata files
#'
#' @param scenario_rdata_files list of file names, loading each rdata file load scen_out, 
#' which is a list of long form model results (one for each parameter set)
#' @param scenario_names corresponding list of scenario names for plotting
#' @param state_to_extract which state to get the cumulative data from
#' @param start_date (optional) start date to calculate cumulative total
#' @param end_date (optional) end date to calculate cumulative total
#'
#' @return data frame with value for the metric, sim for the simulation number (i.e. parameter set), and scenario_name for the scenario name
consolidate_scenarios = function(scenario_rdata_files, scenario_names, state_to_extract, start_date = NULL, end_date = NULL)
{
  summary_data = NULL 

  for (i in 1:length(scenario_rdata_files))
  {
    # this loads scen_out and wide_out
    load(scenario_rdata_files[i])
    if (is.null(start_date)) { start_date = min(scen_out[[1]]$date) }
    if (is.null(end_date)) { end_date = max(scen_out[[1]]$date) }
    
    data_start = lapply(scen_out, function(x) dplyr::filter(x, state == state_to_extract & date == start_date))
    data_end = lapply(scen_out, function(x) dplyr::filter(x, state == state_to_extract & date == end_date))
    the_data = lapply(1:length(scen_out), function(x) dplyr::full_join(data_start[[x]], data_end[[x]], by = c("state", "strain", "vac", "age"), suffix = c("_start", "_end")) )
    the_data = lapply(the_data, function(x) dplyr::mutate(x, value = value_end - value_start))
    
    names(the_data) = 1:length(the_data)
    the_data = bind_rows(the_data, .id = "sim")
     
    # also get totals at end for per capita reporting
    data_totals = lapply(scen_out, function(x) dplyr::filter(x, date == end_date))
    data_totals = lapply(data_totals, filter_non_compartments)
    data_totals = lapply(data_totals, function(x) dplyr::group_by(x, strain, vac, age))
    data_totals = lapply(data_totals, function(x) dplyr::summarise(x, total = sum(value), .groups = "drop") )
    names(data_totals) = 1:length(data_totals)
    data_totals = bind_rows(data_totals, .id = "sim")
    
    # need full join since all cum_hosp have strain but totals don't
    the_data = full_join(the_data, data_totals, by = c("sim", "strain", "vac", "age"))
    the_data$scenario_name = scenario_names[i]
    
    summary_data = rbind(summary_data, the_data)
    rm(list = c("scen_out", "wide_out"))
  }
  
  return(summary_data)
}

#' Calculates max daily value of metric (which can be summed across states) summed across age, vaccine, and strain
#'
#' @param scenario_rdata_files list of file names, loading each rdata file load scen_out, 
#' which is a list of long form model results (one for each parameter set)
#' @param scenario_names corresponding list of scenario names for plotting
#' @param states_to_extract vector of states to sum together before calculating max daily value
#' @param start_date (optional) start date to calculate max daily value
#' @param end_date (optional) end date to calculate max daily value
#'
#' @return data frame with value for the metric, sim for the simulation number (i.e. parameter set), and scenario_name for the scenario name
consolidate_max_scenarios = function(scenario_rdata_files, scenario_names, states_to_extract, start_date = NULL, end_date = NULL)
{
  summary_data = NULL 
  
  for (i in 1:length(scenario_rdata_files))
  {
    # this loads scen_out and wide_out
    load(scenario_rdata_files[i])
    if (is.null(start_date)) { start_date = min(scen_out[[1]]$date) }
    if (is.null(end_date)) { end_date = max(scen_out[[1]]$date) }
    
    the_data = lapply(scen_out, function(x) dplyr::filter(x, state %in% states_to_extract & date >= start_date & date <= end_date))

    # just children
    data_child = lapply(the_data, function(x) dplyr::filter(x, age == 1))
    data_child = lapply(data_child, function(x) dplyr::group_by(x, date))
    data_child = lapply(data_child, function(x) dplyr::summarise(x, value = sum(value), .groups = "drop") )
    data_child = lapply(data_child, function(x) dplyr::summarise(x, children = max(value)))
    names(data_child) = 1:length(data_child)
    data_child = bind_rows(data_child, .id = "sim")
    
    # adults
    data_adult = lapply(the_data, function(x) dplyr::filter(x, age != 1))
    data_adult = lapply(data_adult, function(x) dplyr::group_by(x, date))
    data_adult = lapply(data_adult, function(x) dplyr::summarise(x, value = sum(value), .groups = "drop") )
    data_adult = lapply(data_adult, function(x) dplyr::summarise(x, adult = max(value)))
    names(data_adult) = 1:length(data_adult)
    data_adult = bind_rows(data_adult, .id = "sim")
    
    # now for all
    the_data = lapply(the_data, function(x) dplyr::group_by(x, date))
    the_data = lapply(the_data, function(x) dplyr::summarise(x, value = sum(value), .groups = "drop") )
    the_data = lapply(the_data, function(x) dplyr::summarise(x, value = max(value)))

    names(the_data) = 1:length(the_data)
    the_data = bind_rows(the_data, .id = "sim")
    the_data$scenario_name = scenario_names[i]
    
    # combine them
    the_data = left_join(the_data, data_child, by = "sim")
    the_data = left_join(the_data, data_adult, by = "sim")
    
    summary_data = rbind(summary_data, the_data)
    rm(list = c("scen_out", "wide_out"))
  }
  
  return(summary_data)
}

#' Consolidate number of days at the max sd values across multiple scenarios run in different rdata files
#'
#' @param scenario_rdata_files list of file names, loading each rdata file load scen_out, 
#' which is a list of long form model results (one for each parameter set)
#' @param scenario_names corresponding list of scenario names for plotting
#' @param max_sd_age_1 the max sd value for the age 1 group (it's assumed that this does not vary by age, thus only calcualted for first age group)
#' @param report_as_percentage TRUE to report percentage of days, FALSE to report raw count of days
#' @param start_date (optional) start date to use
#' @param end_date (optional) end date to use
#'
#' @return data frame with value for the metric, sim for the simulation number (i.e. parameter set), and scenario_name for the scenario name
consolidate_max_sd = function(scenario_rdata_files, scenario_names, max_sd_age_1, report_as_percentage = FALSE, start_date = NULL, end_date = NULL)
{
  summary_data = NULL 
  
  for (i in 1:length(scenario_rdata_files))
  {
    # this loads scen_out and wide_out
    load(scenario_rdata_files[i])
    if (is.null(start_date)) { start_date = min(scen_out[[1]]$date) }
    if (is.null(end_date)) { start_date = max(scen_out[[1]]$date) }
    
    data = lapply(scen_out, function(x) dplyr::filter(x, state == "sd" & age == 1 & date >= start_date & date <= end_date))
    days_at_max_sd = unlist(lapply(data, function(x) dplyr::summarise(x, sum(value == max_sd_age_1))))
    if (report_as_percentage)
    {
      days_at_max_sd = days_at_max_sd / unlist(lapply(data, nrow)) * 100
    }

    the_data = data.frame(value = days_at_max_sd, sim = 1:length(days_at_max_sd))
    the_data$scenario_name = scenario_names[i]
    
    summary_data = rbind(summary_data, the_data)
    rm(list = c("scen_out", "wide_out"))
  }
  
  return(summary_data)
  
}