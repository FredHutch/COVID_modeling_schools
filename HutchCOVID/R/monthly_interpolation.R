monthly_interpolation <-
function(time, data) 
{
  na_for_empty = function(func)
  {
    if(!is.infinite(x <- suppressWarnings( func() )))  x else NA 
  }
  
  idx_before = na_for_empty( function() { max(which(time > data$day)) } )
  idx_after = na_for_empty( function() { min(which(time < data$day)) } )
  col_idx = which(startsWith(names(data), "value")) # could be one or more columns
  
  value = NA
  if (is.na(idx_before)) # before all
  {
    value = data[1, col_idx]
  } else if (is.na(idx_after)) # after all
  {
    value = data[nrow(data), col_idx]
  } else    # interpolate between
  {
    value = data[idx_before, col_idx] + 
      (time - data$day[idx_before]) / 
      (data$day[idx_after] - data$day[idx_before]) * 
      (data[idx_after, col_idx] - data[idx_before, col_idx])
  }
  # unlist so this is a vector not a 1x4 df which cause issues with matrix multiplication  
  return (unlist(value))
}
