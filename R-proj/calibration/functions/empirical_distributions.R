empirical_pdf = function(x)
{
  return( approxfun(density(x)) )
}

empirical_generator = function(x, interval = range(x) + c(-0.1, 0.1))
{
  emprical_cdf = ecdf(x)
  inverse_cdf = function(y) { uniroot(function(z){emprical_cdf(z)-y}, interval=interval)$root }
  inverse_cdf = Vectorize(inverse_cdf)
  generator = function(n) {vals = runif(n); return(inverse_cdf(vals))}
  return(generator)
}