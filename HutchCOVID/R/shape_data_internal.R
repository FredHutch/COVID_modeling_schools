# helper function to go from cumulative to daily counts

#' helper function to go from cumulative to daily counts
#'
#' @param x vector of cumulative
#'
#' @return daily values
#' @importFrom dplyr lag
diff_col = function(x) { x - lag(x) }
