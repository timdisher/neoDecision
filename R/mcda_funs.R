

do_mcda <- function(n_outcomes, mcda_dat){
  pref <- simplex.sample(n_outcomes, 10000)$samples
  
  # Calculate SMAA metrics
  # Using the full range of support assumes linear partial utility over a wide
  # range. Could use 95% hull as an alternative which would say it's at least
  # linear over the smaller range. Also could do nonlinear function but would
  # need to have a multivariate model then.
  vals <- smaa.values(mcda_dat, pref)
  ranks <- smaa.ranks(vals)
  cw <- smaa.cw(ranks, pref)
  cf <- smaa.cf(mcda_dat, cw)
  ra <- smaa.ra(ranks)
  dplyr::lst(vals, ranks, cw, cf, ra)
}

mcda_prep <- function(outs, data){
  map(purrr::set_names(outs), ~ {
    form <- as.formula(paste0(., "~ trt"))
    
    fit <- glm(form, family = binomial, data = data)
    
    MASS::mvrnorm(n = 10000, mu = coef(fit), Sigma = vcov(fit)) %>%
      as.data.frame() %>%
      rename(ctrl = `(Intercept)`) %>%
      mutate(trt = ctrl + trt) %>%
      mutate_all(~ 1 - plogis(.))
    
  }) %>%
    abind(., along = 3)
}