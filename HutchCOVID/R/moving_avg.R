#' Calculate centered moving average
#'
#' @param x The vector of whcih to calculate the moving average
#' @param n The number of elements to include in the moving average. Must be odd.
#'
#' This function calculates the centered moving average, that is, including data before and after the focal data
#' point. It will return a vector the same length as x with no NA's (NAs are not supprted in x). FOr the beginning and end 
#' of the series, a moving average of less than n is used (that is, n = 1 for the first and last data point, n = 3 for the 
#' 2nd and 2nd to last data point).
#' 
#' @return A vector of length x that is the moving average of x
#' @export
#' 
moving_avg = function(x, n = 7) 
{ 
  # assume n odd
  stopifnot(n %% 2 == 1)
  
  cx = c(0,cumsum(x))
  odd_to_n = seq(from = 1, to = n - 2, by = 2) # this gives the index into cum sum (adj) and how many to divide by
  rsum = c(cx[odd_to_n + 1] / odd_to_n, # beginning when not enough for full moving avg
           (cx[(n+1):length(cx)] - cx[1:(length(cx) - n)]) / n, # the main part
           (cx[length(cx)] - cx[length(cx) - rev(odd_to_n)]) / rev(odd_to_n)) # the end where there is also not enough
  return(rsum)
}
