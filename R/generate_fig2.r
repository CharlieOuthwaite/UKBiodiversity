#' Generate Figure 2
#'
#' This takes the posterior values of the group level indicators generated within the
#' \code{generage_fig1} function and generates the boxplots presented in figure 2 of the associated paper.
#'
#' @param datadir A filepath specifying where the posteior indicator values are saved.
#' If outputs have not been moved, this will be in a directory "/MajorGroups/geomeans".
#' @param save_plot Logical. If `TRUE` plot will be saved in `datadir`
#'
#' @keywords trends, species, distribution, occupancy
#' @references Outhwaite et al (in prep) Complexity of biodiversity change revealed through long-term trends of invertebrates, bryophytes and lichens.
#' @references Outhwaite, C. L., Powney, G. D., August, T. A., Chandler, R. E., Rorke, S., Pescott, O., … Isaac, N. J. B. (2019). Annual estimates of
#'  occupancy for bryophytes, lichens and invertebrates in the UK (1970-2015).
#'  NERC Environmental Information Data Centre. https://doi.org/10.5285/0ec7e549-57d4-4e2d-b2d3-2199e1578d84
#' @examples
#' \dontrun{
#'
#' # Run generate_fig2 function to produce boxplot.
#' # datadir should be the filepath of where the posterior indicator values are saved.
#' generate_fig2(postdir = paste0(getwd(), "/MajorGroups/geomeans"))
#'
#' }
#' @export
#' @import ggplot2


generate_fig2 <- function(datadir, save_plot = TRUE){

# list the geomean iterations outputs
datasets <- list.files(datadir, pattern = "indicator_posterior_vals")

# create a results table
results_tab <- NULL

iters_tab <- NULL

# loop through each group
for(dataset in datasets){

  # get the group name
  group <- sub("_indicator_posterior_vals.csv", "", dataset)

  # read in the dataset for this group
  iters_data <- read.csv(paste0(datadir, "/", dataset))

  # remove "X" from column names
  colnames(iters_data) <- sub("X", "", colnames(iters_data))

  # separate out before and after 2000
  iters_data_before <- iters_data[, as.character(1970:1992)]
  iters_data_after <- iters_data[, as.character(1993:2015)]

  # get the mean and 95% CIs for the change in this group
  overall_change_before <- ((iters_data_before[,"1992"] - iters_data_before[, 1]))
  overall_change_after <- ((iters_data_after[,"2015"] - iters_data_after[, 1]))

  mean_before <- mean(overall_change_before)
  UCI_95_before <- quantile(overall_change_before, probs = 0.975)
  LCI_95_before <-  quantile(overall_change_before, probs = 0.025)

  mean_after <- mean(overall_change_after)
  UCI_95_after <- quantile(overall_change_after, probs = 0.975)
  LCI_95_after <-  quantile(overall_change_after, probs = 0.025)

  overall_change_before <- as.data.frame(overall_change_before)
  overall_change_before$group <- group
  overall_change_before$trend <- "before"
  overall_change_after <- as.data.frame(overall_change_after)
  overall_change_after$group <- group
  overall_change_after$trend <- "after"

  colnames(overall_change_before)[1] <- "value"
  colnames(overall_change_after)[1] <- "value"

  # combine results
  result <- c(group, mean_before, LCI_95_before, UCI_95_before, mean_after, LCI_95_after, UCI_95_after)

  # add to results tables
  results_tab <- rbind(results_tab, result)

  iters_tab <- rbind(iters_tab, overall_change_before, overall_change_after)

}

# edit column names
colnames(results_tab) <- c("Group", "before", "B_lower", "B_upper", "after", "A_lower", "A_upper")

# save tables
write.csv(results_tab, paste0(datadir, "/GROUP_before_after_1992_change_CIs_absolute.csv"), row.names = F)

write.csv(iters_tab, paste0(datadir, "/Group_before_after_1992_change_iters_absolute.csv"), row.names = F)

### now generate the box plot ###

# read in short-term and long-term trend iterations
plot_data <- read.csv(paste0(datadir, "/", "Group_before_after_1992_change_iters_absolute.csv"))

# Remove the data for group "ALL"
plot_data <- plot_data[!plot_data$group == "ALL", ]

# edit labels
plot_data$group <- sub("FRESHWATER_SPECIES", "Freshwater", plot_data$group)
plot_data$group <- sub("LOWER_PLANTS", "Bryophytes \n& Lichens", plot_data$group)
plot_data$group <- sub("TERRESTRIAL_INSECTS", "Insects", plot_data$group)
plot_data$group <- sub("TERRESTRIAL_NONINSECT_INVERTS", "Inverts", plot_data$group)

plot_data$trend <- sub("before", "Pre-1992", plot_data$trend)
plot_data$trend <- sub("after", "Post-1992", plot_data$trend)

plot_data$trend <- as.factor(plot_data$trend)

# reorder factor levels
plot_data$trend <- factor(plot_data$trend, levels(plot_data$trend)[c(2,1)] )

plot_data$group <- as.factor(plot_data$group)

# reorder factor levels
plot_data$group <- factor(plot_data$group, levels(plot_data$group)[c(2, 3, 4, 1)] )


# generate the plot
p1 <- ggplot(plot_data, aes(x = trend, y = value, fill = group)) +
  stat_summary(fun.data = quantiles_95, geom="boxplot", lwd = 0.1) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 10),
        text = element_text(size = 10),
        panel.border = element_rect(size = 0.2),
        axis.ticks = element_line(size = 0.2),
        strip.background = element_rect(size = 0.2)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("Absolute change in mean occupancy") +
  facet_grid(~ group, scales = 'free_x') +
  scale_y_continuous(limits = c(-0.05, 0.06), breaks = seq(-0.05, 0.06, 0.01),
                     expand = c(0,0))

if(save_plot == TRUE){
# save the plot
ggsave(filename = paste0(datadir, "/Figure_2.pdf"), plot = p1, height = 6, width = 6)
}

return(p1)

}
