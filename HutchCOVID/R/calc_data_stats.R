#' Calculates metrics for calibration from the data
#'
#' @param data_actual the data to use
#' @param dates which dates to include
#' @param rescale_factors i.e. the mean of each time series to normalize by
#' @param stats_to_include which of cases, deaths and hosp to include
#'
#' @return vector of statistics
#' @export
calc_data_stats = function(data_actual, dates,
                           rescale_factors = list( cases = rep(1, 4), deaths = rep(1, 4), hosp = rep(1, 4), negtests = rep(1, 4) ), 
                           stats_to_include = c("cases", "deaths", "hosp"))
{
  data_actual = data_actual %>% filter(date %in% dates)
  data_cases = dplyr::select(data_actual, starts_with("ma_pos")) 
  n_age = ncol(data_cases)
  # TODO: right now combining hosp deaths and deaths, this will need to be updated depending on how deaths occur in model
  data_deaths = data_actual[,paste0("ma_deaths", 1:n_age)] + data_actual[,paste0("ma_hosp_deaths", 1:n_age)]
  data_hosp = dplyr::select(data_actual, starts_with("ma_hosps")) 
  # data_negtests = dplyr::select(data_actual, starts_with("ma_neg"))  
  
  # normalize data (transpose is because otherwise division is by cols)
  data_cases = t(t(data_cases) / rescale_factors$cases)
  data_deaths = t(t(data_deaths) / rescale_factors$deaths)
  data_hosp = t(t(data_hosp) / rescale_factors$hosp)
  # data_negtests = t(t(data_negtests) / rescale_factors$negtests)
  
  # important that these are in the same order as model!
  stats = NULL
  if ("cases" %in% stats_to_include) { stats = c(stats, data_cases)}
  if ("deaths" %in% stats_to_include) { stats = c(stats, data_deaths)}
  if ("hosp" %in% stats_to_include) { stats = c(stats, data_hosp)}
  # if ("negtests" %in% stats_to_include) { stats = c(stats, data_negtests)}
  
  return(stats) 
}
