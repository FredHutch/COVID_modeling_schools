set_monthly <-
function(date, data) 
{
  value = NA
  col_idx = which(startsWith(names(data), "value")) # could be one or more columns
  
  if (date < min(data$date) ) 
  { 
    value = data[1, col_idx]
  }
  else 
  {
    row_idx = max(which(date >= data$date))
    value = data[row_idx, col_idx]
  }
  # unlist so this is a vector not a 1x4 df which cause issues with matrix multiplication  
  return (unlist(value))
}
