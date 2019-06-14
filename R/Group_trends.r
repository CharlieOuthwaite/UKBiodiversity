##%######################################################%##
#                                                          #
####      Function to calculate group level change      ####
#                                                          #
##%######################################################%##


# use the 1000 geomean estimates to determine the group level change


rm(list = ls())

# where are the outputs
datadir <- "C:/Users/charl/Dropbox/PhD WORK/1. BIG PAPER/Package_testing/Major_groups/geomeans"

# list the geomean iterations outputs
files <- list.files(datadir, pattern = "_indicator_posterior_vals")


# create a results table
results_tab <- NULL

iters_tab <- NULL

# loop through each group
for(file in files){

  # get the group name
  group <- sub("_indicator_posterior_vals.csv", "", file)

  # read in the dataset for this group
  iters_data <- read.csv(paste(datadir, "/", file, sep = ""))

  iters_data <- iters_data[, 2:47]

  # get the mean and 95% CIs for the change in this group
  overall_change <- ((iters_data[,46] - iters_data[, 1])/iters_data[, 1]) * 100
  mean <- mean(overall_change)
  UCI_95 <- quantile(overall_change, probs = 0.975)
  LCI_95 <-  quantile(overall_change, probs = 0.025)

  # round to 3 decimal places
  mean <- round(mean, 3)
  UCI_95 <- round(UCI_95, 3)
  LCI_95 <- round(LCI_95, 3)

  overall_change <- as.data.frame(overall_change)
  overall_change$group <- group

  iters_tab <- rbind(iters_tab, overall_change)

  # combine results
  result <- c(group, mean, LCI_95, UCI_95)

  # add to results table
  results_tab <- rbind(results_tab, result)

}

colnames(results_tab) <- c("Group", "Mean change", "LCI", "UCI")

# save results
write.csv(results_tab, paste0(datadir, "/Group_level_change.csv"), row.names = F)

write.csv(iters_tab, paste0(datadir, "/Long_term_change_iters.csv"), row.names = F)

