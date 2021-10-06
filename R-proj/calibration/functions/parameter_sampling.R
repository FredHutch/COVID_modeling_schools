# calculate distance between parameter sets to pick best spanning subset
# uses brute force appraoch to calcualte the distance between parameter sets, 
# then the overall distance for each possible subset

# the distance between all parameter sets
calculate_distance_matrix = function(param_sets)
{
  dist = matrix(0, nrow = nrow(param_sets), ncol = nrow(param_sets))
  
  for (i in 1:(nrow(param_sets) - 1))
    for (j in (i + 1):nrow(param_sets))
    {
      d = sqrt(sum( (param_sets[i,] - param_sets[j,])^2 ))
      dist[i,j] = d
      dist[j,i] = d
    }
  return(dist)
}

# which subset of parameter sets best spans the parameter space
# note that n cannot be too large or there are too many combinations
calculate_best_subset = function(dist_matrix, n)
{
  stopifnot(nrow(dist_matrix) == ncol(dist_matrix))
  p = nrow(dist_matrix) # number of parameter sets
  
  all_comb = t(combn(1:p, n))
  dist = vector("numeric", nrow(all_comb))
  
  for (i in 1:nrow(all_comb))
  {
    d = 0
    for (j in 1:(n-1))
      for (k in (j+1):n)
      {
        d = d + dist_matrix[all_comb[i,j], all_comb[i,k]]^2
      }
    dist[i] = sqrt(d)
  }
  return(all_comb[which.max(dist),])
}