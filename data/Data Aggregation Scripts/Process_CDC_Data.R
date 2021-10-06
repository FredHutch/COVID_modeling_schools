library(lubridate)

#' Get CDC surveillance data
#'
#' This function gets the CDC case surveillance data with geographic information described here:
#'  https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data-with-Ge/n8mc-b4w4 (data set)
#'  https://dev.socrata.com/foundry/data.cdc.gov/n8mc-b4w4 (API documentation)
#'  
#'  HOWEVER, there are several problems with this data set:
#'    - only 1000 rows are returned
#'    - data is aggregated by month
#'    - date is only for positive test, there are columns for is the case was later hospitalized or died, but no dates, and death data is all missing
#'
#' @param county which Washington county to get data for, should be in all caps, i.e. KING
#'
#' @return date frame with data
get_CDC_epidata = function(county)
{
  data_url = paste0("https://data.cdc.gov/resource/n8mc-b4w4.csv?res_state=WA&res_county=", county)
  case_data = read.csv(data_url)
  return(case_data)
}


#' Process an already downloaded CDC data file
#'
#' This function processes data downloaded from the CDC Covid Data Tracker, for King county, the link is
#' https://covid.cdc.gov/covid-data-tracker/#county-view|Washington|53033|Risk|community_transmission_level 
#' then click Download Data at the bottom of the page
#' 
#' There is data on cases, hospitalizations, and deaths, but not age-specific data. Data is only reported as 7-day moving averages
#' and is sometimes 'supressed' when it would be a small value near or less than 1.
#' 
#' @param filename the csv file to process
#'
#' @return data fram with data
process_CDC_data_tracker_data = function(filename)
{
  data = read.csv(filename, skip = 2, na.strings = "null")
  
  # rename columns to be simpler
  # note that all are moving averages!! but of - 7 days, not +/-3 days like we calculate
  names(data) = c("FIPS_code", "State_abbr", "State", "County", "Cases", "Deaths", "Tests", "Test_positivity", 
                  "Hosp", "Percent_beds_covid", "Percent_ICU_beds_COVID", "Date", "Date_window_start", "Date_window_end", "Level_community_transmission")
  
  # values are sometimes marked as suppressed, this seems to be when when data isthey would be between 0 and 1, 
  # so replace with average value that it would be, i.e. 0.5
  # This seems to be true only for Cases and Deaths and not for Hosp
  data$Cases[data$Cases == "suppressed"] = "0.5"
  data$Cases = as.numeric(data$Cases)
  data$Deaths[data$Deaths == "suppressed"] = "0.5"
  data$Deaths = as.numeric(data$Deaths)
  
  data$Date = ymd(data$Date)
  data$Date_window_start= ymd(data$Date_window_start)
  data$Date_window_end = ymd(data$Date_window_end)
  
  return(data)
}

