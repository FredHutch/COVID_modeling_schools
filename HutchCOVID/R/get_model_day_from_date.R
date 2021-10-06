#' Gets day in model time from a date object from defined model day 0
#'
#' @param date date to translate
#' @param model_day0_date calendar date for model day0
#'
#' @return model day
#' @export
get_model_day_from_date = function(date, model_day0_date)
{
  # convert to doy that continues to increase at year boundaries
  days = as.numeric(date - model_day0_date)
  return(days)
}
