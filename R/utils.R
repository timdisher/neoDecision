#' Find value of mu to give desired probability
#' 
#' Find the value of mu from a normal distribution with sd of 1 that would 
#' give the desired probability when event == 1 when x > 0
#' 
#' @param p Desired probability
#' @return the value of mu to use and a summary of how close it was to the one desired
#' @export 
find_mu <- function(p){
  
  ex <- tibble::tibble(try = seq(from = -3, to = 3, length.out = 1000),
                       ex = pnorm(0, mean = try, sd = 1, lower.tail = FALSE),
                       diff = abs(ex - p)) %>%
    dplyr::filter(diff == min(diff))
  
 
  print(glue::glue("Requested: {p} | Expected {round(ex$ex,3)}"))
  
  return(round(ex$try, 3))
  
}

#' Find gamma parameters for desired mean and standard deviation
#' 
#' @param mu desired mean
#' @param sd desired standard deviation
#' 
#' @return the required shape and rate (default arguments for R's gamma distribution
#' functions). 
gam_par <- function(mu, sd){
  
  a <- mu^2 / sd^2
  rate <- mu / sd^2
  
  list(shape = a, rate = rate)
  
}