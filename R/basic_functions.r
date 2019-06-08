##%######################################################%##
#                                                          #
####                  small functions                   ####
#                                                          #
##%######################################################%##


# function to calculate the geometric mean
calc_geo <- function(x){
  exp(mean(log(x), na.rm = TRUE))
}





