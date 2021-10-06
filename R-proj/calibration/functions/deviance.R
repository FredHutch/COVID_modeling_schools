# functions to calculate deviance and DIC

# calculates theta_hat, a point estimate of theta, i.e. median of posterior, and also runs model to calculate summary stats for theta_hat
theta_hat_stats = function(parameter_set, estimator = c("median", "mean"),  pars_names, pars_base, pars_temporal, state,
                           dates, rescale_factors, stats_to_include = c("cases", "deaths", "hosp"))
{
  estimator = match.fun(estimator)
  theta_hat = apply(parameter_set, 2, estimator)
  theta_hat_summary_stat = calc_model_stats(theta_hat, pars_names, pars_base, pars_temporal, state, start_from_first_inf = TRUE, 
                                            dates = dates, rescale_factors = rescale_factors, stats_to_include = stats_to_include)
  return(list(theta_hat = theta_hat, theta_hat_summary_stat = theta_hat_summary_stat))
}
  
# calculate DIC from deviance, if model already run for theta_hat
calculate_DIC = function(data_summary_stat, model_summary_stat, theta_hat_summary_stat, kernel = "gaussian")
{
  D_1 = calculate_deviance(data_summary_stat, model_summary_stat, kernel)
  
  D_1_theta_hat = calculate_deviance(data_summary_stat, t(theta_hat_summary_stat), kernel, epsilon = D_1$epsilon)
  
  # DIC = D_1 + p_D = 2 * D_1 - D_1_theta_hat
  DIC = 2 * D_1$expected.deviance - D_1_theta_hat$expected.deviance
  return(DIC)
}

# calculate p_D, the effective number of parameter
calculate_pD = function(data_summary_stat, model_summary_stat, theta_hat_summary_stat, kernel = "gaussian")
{
  D_1 = calculate_deviance(data_summary_stat, model_summary_stat, kernel)
  
  D_1_theta_hat = calculate_deviance(data_summary_stat, t(theta_hat_summary_stat), kernel, epsilon = D_1$epsilon)
  
  # DIC = D_1 + p_D = 2 * D_1 - D_1_theta_hat
  # this implies that p_D = D_1 - D_1_theta_hat
  p_D = D_1$expected.deviance - D_1_theta_hat$expected.deviance
  return(p_D)
}

# this is taken from the expected.deviance() function from the abc package with the following changes
# - just post a warning if called with a single set of summary stats (so we can calculate deviance of theta_hat)
# - allow passing in epsilon (the max distance) again to calculate deviance of theta_hat
# - remove normalization (TODO or optionally pass in normalization statistic too?)
calculate_deviance = function (target, postsumstat, kernel = "gaussian", subset = NULL,  print = TRUE, epsilon = NULL) 
{
  if (missing(target)) 
    stop("'target' is missing with no default", call. = F)
  if (missing(postsumstat)) 
    stop("'postsumstat' is missing with no default", call. = F)
  if (!is.matrix(postsumstat) && !is.data.frame(postsumstat) && 
      !is.vector(postsumstat)) 
    stop("'postsumstat' has to be a matrix, data.frame or vector.", 
         call. = F)
  if (!any(kernel == c("gaussian", "epanechnikov", "rectangular", 
                       "triangular", "biweight", "cosine"))) {
    kernel <- "gaussian"
    warning("Kernel is incorrectly defined. Setting to default (gaussian)", 
            call. = F, immediate. = T)
  }
  if (is.data.frame(postsumstat)) 
    postsumstat <- as.matrix(postsumstat)
  if (is.list(target)) 
    target <- unlist(target)
  if (is.vector(postsumstat)) 
    postsumstat <- matrix(postsumstat, ncol = 1)
  if (length(target) != dim(postsumstat)[2]) 
    stop("Number of summary statistics in 'target' has to be the same as in 'postsumstat'.", 
         call. = F)
  nss <- length(postsumstat[1, ])
  numsim <- length(postsumstat[, 1])
  cond1 <- !any(as.logical(apply(postsumstat, 2, function(x) length(unique(x)) - 1)))
  if (cond1) 
    warning("Zero variance in the summary statistics.", call. = F)
  gwt <- rep(TRUE, length(postsumstat[, 1]))
  gwt[attributes(na.omit(postsumstat))$na.action] <- FALSE
  if (is.null(subset)) 
    subset <- rep(TRUE, length(postsumstat[, 1]))
  gwt <- as.logical(gwt * subset)
  scaled.postsumstat <- postsumstat
  # TODO can reconsider normalization. normalise(x, y) divides x by mad(y)
  # for (j in 1:nss) scaled.postsumstat[, j] <- abc:::normalise(postsumstat[, j], postsumstat[, j][gwt])
  # for (j in 1:nss) target[j] <- abc:::normalise(target[j], postsumstat[, j][gwt])
  sum1 <- 0
  for (j in 1:nss) {
    sum1 <- sum1 + (scaled.postsumstat[, j] - target[j])^2
  }
  dist <- sqrt(sum1)
  if (is.null(epsilon))
    epsilon <- max(dist)
  dist <- dist/epsilon
  if (kernel == "gaussian") 
    dist <- exp(-0.5 * dist^2)/sqrt(2 * pi)
  if (kernel == "epanechnikov") 
    dist <- 1 - (dist)^2
  if (kernel == "rectangular") 
    dist <- dist
  if (kernel == "triangular") 
    dist <- 1 - abs(dist)
  if (kernel == "biweight") 
    dist <- (1 - (dist)^2)^2
  if (kernel == "cosine") 
    dist <- cos(pi/2 * dist)
  dist <- dist/epsilon
  dist[dist == 0] <- NA
  if (print) 
    cat(sum(is.na(dist)), "/", length(dist), " replicated data were excluded.\n",  sep = "")
  dist[is.na(dist)] <- 4e-44
  dist <- log(dist)
  dev <- -2 * mean(dist)
  return(list(expected.deviance = dev, dist = dist, epsilon = epsilon))
}
