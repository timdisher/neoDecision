test_that("mv probit works", {

  data("ex_dat")
  
  run_bayes <- FALSE
  
  dat <- as.data.frame(ex_dat) %>% 
    dplyr::select(trt, mort, sev_ivh, sepsis, nec)
  y <- dat %>% dplyr::select(-trt) %>% as.matrix
  D <- ncol(Y)
  N <- nrow(Y)
  x <- cbind(1,dat %>% dplyr::select(trt) %>% as.matrix())
  K <- ncol(x) # number of predictors
  
  
if(run_bayes == TRUE){
  fit <- stan(file = here::here("inst","models","mvProbit.stan"), 
              data = c("K", "D", "N", "y", "x"),
              init_r = 1,
              chains = 4,
              cores = 4)
  sfit <- summary(fit)
  }
  tst <- mvProbit( cbind(mort, sev_ivh, sepsis, nec) ~ trt, 
            data = dat,
            iterlim = 1, nGHK = 50 )
  
  summary(tst)
  
  })


