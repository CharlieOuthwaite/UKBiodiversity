#' Calculate group level trends
#'
#' This takes the posterior values of the group level indicators generated within the
#' \code{generage_fig1} function and calculates the group level average change and
#' associated 95% credible intervals.  These are presented within the text of the
#' accompanying paper.
#'
#' @param datadir A filepath specifying where the posteior indicator values are saved.
#' If outputs have not been moved, this will be in a directory "/MajorGroups/geomeans".
#'
#' @keywords trends, species, distribution, occupancy
#' @references Outhwaite et al (in prep) Complexity of biodiversity change revealed through long-term trends of invertebrates, bryophytes and lichens.
#' @references Outhwaite, C. L., Powney, G. D., August, T. A., Chandler, R. E., Rorke, S., Pescott, O., … Isaac, N. J. B. (2019). Annual estimates of
#'  occupancy for bryophytes, lichens and invertebrates in the UK (1970-2015).
#'  NERC Environmental Information Data Centre. https://doi.org/10.5285/0ec7e549-57d4-4e2d-b2d3-2199e1578d84
#' @examples
#' \dontrun{
#'
#' # Run group_trends function to estimate in text values
#' # datadir should be the filepath of where the posterior indicator values are saved.
#' #' group_trends(postdir = paste0(getwd(), "/MajorGroups/geomeans"))
#'
#' }
#' @export




group_trends <- function(datadir){

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

} # end of group level files

colnames(results_tab) <- c("Group", "Mean_change", "LCI", "UCI")
results_tab <- as.data.frame(results_tab)

results_tab$Mean_change <- as.numeric(as.character(results_tab$Mean_change))
results_tab$LCI <- as.numeric(as.character(results_tab$LCI))
results_tab$UCI <- as.numeric(as.character(results_tab$UCI))

# save results
write.csv(results_tab, paste0(datadir, "/Group_level_change.csv"), row.names = F)

write.csv(iters_tab, paste0(datadir, "/Long_term_change_iters.csv"), row.names = F)

# return the main results table
return(results_tab)
}
