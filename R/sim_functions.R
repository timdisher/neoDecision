#' sim_mv
#' 
#' Take a correlation matrix for mixed binomial and continuous data and simulate
#' data from a multivariate probit model. A nice introduction to these can be
#' found here. \insertCite{mcparland2016model}{neoDecision}. The reason I use
#' this approach is because later I will be simulating data from a model that
#' assumes this and then trying to recover parameters.
#' 
#' @param cor_mat A correlation matrix. With 1s on the diagonal and correlations
#' on the off diagonal. Assumes that binomial variables all come first, then continuous
#' ones. Correlations between continuous and binomial variables can be 0 since they 
#' aren't used.
#' @param ses a vector of standard errors
#' @param mus a vector of means
#' @param tes a vector of treatment effects on binomial variables. Treatment effects
#'   on continuous variables are assumed to be entirely mediated through binomial
#'   ones.
#' @param n_trial total n in the trial
#' 
#' @return an n_trial x num_vars matrix of simulated patient outcomes. Note that
#' correlations are on a latent scale will not translate directly.
#' 
#' @references{
#' \insertAllCited{}
#'   }
sim_mv <- function(cor_mat, 
                   bin_mus,
                   cont_mus, 
                   cont_sds, 
                   tes = 0,
                   n_trial){

# drop any names from correlation matrix
cor <-  cor_mat %>% dplyr::select_if(is.numeric) %>%
    as.matrix

# Find the probit scores needed to create desired average rate    
mus_bin <- purrr::map_dbl(bin_mus, find_mu)


nbin <- length(mus_bin)
sds <- c(rep(1, nbin))

# Create correlation matrix from lower triangle
lower <- as.matrix(cor_mat[1:nbin, 3:(3+(nbin - 1))])
upper <- t(lower)

bin_cor <- lower + upper
diag(bin_cor) <- 1

# Multivariate probit 
trt <- rbinom(n_trial, 1, 0.5)


sim_patients_c <- data.frame(trt = 0,
                             MASS::mvrnorm(
                              n = round(n_trial/2),
                              mu = mus_bin,
                              Sigma = bin_cor))

sim_patients_t<- data.frame(trt = 1,
                            MASS::mvrnorm(n = round(n_trial/2),
                                mu = mus_bin + tes,
                                Sigma = bin_cor))


bin_events <- cbind(
            trt = c(sim_patients_c$trt, sim_patients_t$trt),
            apply(rbind(sim_patients_c[,-1], sim_patients_t[,-1]), 2, function(x) ifelse(x > 0, 1, 0))
)

bin_events %>% 
  as.data.frame %>%
  dplyr::group_by(trt) %>%
  dplyr::summarise(across(dplyr::everything(), mean))

# For simulation take a bit of an easy way out and pick some mostly reasonable
# covariates and then find intercept to give the following:
# 
#   "dur_mv" = 31.6, #1 
# "dur_rs" = 45.3, #1
# "full_feed" = 12.9, #1
# "reg_bw" = 19, #1
# "dur_hosp" = 80 #1

# Vector of betas for the effect of binomial variables on the log ratio of means scale
# We then draw 5 sets of these (one for each continuous outcome) from uncorrelated
# multivariate normal

cont_betas <- log(c(0.5, 0.8, 0.7, 1.2, 1.3, 1.2, 0.8, 1.2, 1.3, 1.1, 1.1, 1.2,1.1))

nb <- length(cont_betas)

simb <- MASS::mvrnorm(n = 5, 
                      mu = cont_betas,
                      Sigma = diag(rep(0.2^2, nb)))


# Calculate the predicted mean for each individual based on binomial events
# and cont betas for each outcome
preds_unadj <- purrr::map(1:5, ~ { 
  
  exp(bin_events[,2:(nb+1)] %*% simb[.,])
  
})


# Lazy way to set an intercept to get the desired overall mean
ints <- cont_mus / (purrr::map(preds_unadj, mean) %>% unlist )

# Predicted mean continuous outcomes for each person in the trial
preds <- purrr::map(1:5, ~ { 
  
  ints[[.]] * exp(bin_events[,1:nb] %*% simb[.,])
  
}) %>% do.call(cbind, .)


# Create continuous correlation matrix for lower triangle
lwr_cont <- as.matrix(cor_mat[(nbin+1):nrow(cor_mat), (3+(nbin)): ((3+(nbin)) + 4)])
upr_cont <- t(lwr_cont)

cont_cor <- lwr_cont + upr_cont
diag(cont_cor) <- 1

# Simple guassian copula
z <- MASS::mvrnorm(n_trial,mu=rep(0, length(cont_mus)),Sigma= cont_cor)

u <- pnorm(z)

# Iterate over outcomes and use inverse CDF copula trick to get correlated draws
# from gamma distribution
cont_sim <- purrr::map(1:5, ~ {
  
  mu <- preds[,.]
  sd <- cont_sds[[.]]
  u_o <- u[,.]
  
  gam_p <- purrr::map(mu, ~ gam_par(., sd))
  
  out <- purrr::map(seq_along(u_o), ~ {
    
    qgamma(u_o[[.]], gam_p[[.]]$shape, gam_p[[.]]$rate)
    
  }) %>% 
    unlist
  
  out
  
}) %>% do.call(cbind, .) %>%
  `colnames<-`(names(cont_mus)) 


# Output is a matrix with the binary events and continuous outcomes. 
cbind(bin_events, cont_sim)
}

