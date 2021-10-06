#' Gets date in calendar time from a day in model time
#'
#' @param model_day day to translate
#' @param model_day0_date calendar date for model day0
#'
#' @return calendar date
#' @export
get_date_from_model_day = function(model_day, model_day0_date)
{
  date = model_day + model_day0_date
  return(date)
}
