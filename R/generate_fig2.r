##%######################################################%##
#                                                          #
####           Function to generate Figure 2            ####
#                                                          #
##%######################################################%##


rm(list = ls())

# load libraries
library(ggplot2)

# where are the outputs
datadir <- "C:/Users/charl/Dropbox/PhD WORK/1. BIG PAPER/Package_testing/Major_groups/geomeans"

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

  # remove unnecessary column
  iters_data <- iters_data[, 2:ncol(iters_data)]

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

  # add to results table
  results_tab <- rbind(results_tab, result)

  iters_tab <- rbind(iters_tab, overall_change_before, overall_change_after)

}


colnames(results_tab) <- c("Group", "before", "B_lower", "B_upper", "after", "A_lower", "A_upper")

# save tables
write.csv(results_tab, paste0(datadir, "/GROUP_before_after_1992_change_CIs_absolute.csv"), row.names = F)

write.csv(iters_tab, paste0(datadir, "/Group_before_after_1992_change_iters_absolute.csv"), row.names = F)



##%######################################################%##
#                                                          #
####    Box plots of long-term and short-term trends    ####
#                                                          #
##%######################################################%##

# read in long-term and short-term trends
rm(list = ls())

# load libraries
library(ggplot2)
library(reshape2)

# where are the outputs
datadir <- "C:/Users/charl/Dropbox/PhD WORK/1. BIG PAPER/Package_testing/Major_groups/geomeans"

# read in short-term and long-term trend iterations
plot_data <- read.csv(paste0(datadir, "/", "Group_before_after_1992_change_iters_absolute.csv"))

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


quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

#### used this one in latest draft 26/07/2018

ggplot(plot_data, aes(x = trend, y = value, fill = group)) +
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

ggsave(filename = paste0(datadir, "/Figure_2.pdf"), height = 6, width = 6)
