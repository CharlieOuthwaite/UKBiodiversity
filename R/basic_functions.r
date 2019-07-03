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