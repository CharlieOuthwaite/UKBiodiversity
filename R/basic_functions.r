##%######################################################%##
#                                                          #
####                  small functions                   ####
#                                                          #
##%######################################################%##


# function to calculate the geometric mean
calc_geo <- function(x){
  exp(mean(log(x), na.rm = TRUE))
}


# function to rescale posterior geomean values
rescale <- function(x){

  multipier <- 100/x[1]
  rescaled <- x*multipier
  return(rescaled)
}


# estimating quantiles
quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}



