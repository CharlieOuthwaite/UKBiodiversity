##%######################################################%##
#                                                          #
####       function to generate figure 4 of paper       ####
#                                                          #
##%######################################################%##



# where are the posteriors?
postdir <- "C:/Users/charl/Dropbox/PhD WORK/1. BIG PAPER/Package_testing/Taxa"

# where to save the outputs
dir.create(paste0(postdir, "/geomeans"))
outdir <- paste0(postdir, "/geomeans")

# list the
files <- list.files(postdir, pattern = "posterior_samples")

# loop through each group and generate the indicator values
for(file in files){

  # extract the group name
  group <- sub("_posterior_samples_national.rdata", "", file)

  # load in the combined posterior generated from the combine_posteriors function
  load(paste0(postdir, "/", file))

  # convert 0 and 1 to 0.0001 and 0.9999 - solve the issue with logging zero and 1
  temp_post <- group_post[,1:(ncol(group_post)-2)]
  temp_post[temp_post == 0] <- 0.0001
  temp_post[temp_post == 1] <- 0.9999
  temp_post <- cbind(temp_post, group_post[,c("spp","iter")])

  j_post <- temp_post

  # somewhere to save the means
  all_means <- NULL

  # add the number of species
  n_sp <- length(unique(j_post$spp))

  # loop through each iteration and take the geometric mean
  for(i in 1:1000){

    # subset the all ant data so that only have the i samples
    j_post_iter <- j_post[j_post$iter == i, ]

    geo_means <- apply(j_post_iter[1:46], 2, calc_geo)

    all_means <- rbind(all_means, geo_means)
  } # end of loop through iterations


  # save the posterior geometric means
  write.csv(all_means, file = paste(outdir, "/", group, "_indicator_posterior_vals.csv", sep = ""))


  all_means_rescaled <- t(apply(all_means, 1, rescale))


  # calculate mean and 90% CIs
  final_rescaled <- data.frame(avg_occ = apply(all_means_rescaled, 2, mean, na.rm = TRUE),
                               upper_CI = apply(all_means_rescaled, 2, quantile, probs = 0.95, na.rm = TRUE),
                               lower_CI = apply(all_means_rescaled, 2, quantile, probs = 0.05, na.rm = TRUE))

  # add in the year
  final_rescaled$year <- as.numeric(rownames(final_rescaled))

  # Save the rescaled indicator values
  write.csv(final_rescaled, file = paste0(outdir, "/", group, "_rescaled_indicator_vals.csv"), row.names = FALSE)


}

# Read in and organise all rescaled indicator files

# list the files of rescaled indicator values.  One per group.
files <- list.files(outdir, pattern = "_rescaled_indicator_vals")

# somewhere to save the info
all_plot_data <- NULL

for(file in files){

  # read in first group plot data
  plot_data <- read.csv(paste0(outdir, "/", file))

  # add a group name column
  plot_data$group <- sub("_rescaled.*", "", file)

  # combine into one data table
  all_plot_data <- rbind(all_plot_data, plot_data)
}

# change column names
colnames(all_plot_data) <- c("mean", "UCI", "LCI", "year", "group")


# change order of the lines
#all_plot_data$group <- as.factor(all_plot_data$group)
#all_plot_data$group <- factor(all_plot_data$group, levels(all_plot_data$group)[c(2,3,4,1)])


library(viridis)
library(gridExtra)

# read in major orders file (this will be data within the package)
major_groups <- read.csv("C:/Users/charl/Dropbox/PhD WORK/1. BIG PAPER/UKBiodiversity/Data/Major_groups.csv")

all_plot_data$major_group <- NA

# look up the major groups from the major_groups file
for(grp in unique(all_plot_data$group)){

  major_grp <- as.character(unique(major_groups[major_groups$Group == grp, "Major_group"]))

  all_plot_data[all_plot_data$group == grp, "major_group"] <- major_grp

}

plot_group <- function(major_group){


  ggplot(all_plot_data[all_plot_data$major_group == major_group,], aes_string(x = "year", y = "mean", col = "group", fill = "group")) +
    theme_bw() +
    #scale_colour_viridis(discrete = TRUE, end = 0.9)+
    geom_ribbon(data = all_plot_data[all_plot_data$major_group == major_group,],
                aes_string(ymin = "LCI", ymax = "UCI", linetype = NA),
                alpha = 0.2) +
    geom_line(size = 0.5) +
    geom_hline(yintercept = 100) +
    #geom_point(size = 2) +
    ylab("Occupancy") +
    xlab("Year") +
    #ggtitle(major_order) +
    scale_y_continuous(limits = c(0, 200)) +
    scale_x_continuous(limits = c(1970, 2015)) +
    theme(plot.title = element_text(size = 12), text = element_text(size = 12),
          aspect.ratio = 1,
          legend.title = element_blank())+
    #plot.margin = unit(c(0,0,0,0), "cm")
    guides(colour = guide_legend(ncol = 1))
}


p = list()

for(i in 1:length(unique(all_plot_data$major_group))){
  major_group <- unique(all_plot_data$major_group)[i]

  p[[i]] = plot_group(major_group)
}

p[[1]] <-   ggplot(all_plot_data[all_plot_data$major_group == "TERRESTRIAL_INSECTS",], aes_string(x = "year", y = "mean", col = "group", fill = "group")) +
  theme_bw() +
  #scale_colour_viridis(discrete = TRUE, end = 0.9)+
  geom_ribbon(data = all_plot_data[all_plot_data$major_group == "TERRESTRIAL_INSECTS",],
              aes_string(ymin = "LCI", ymax = "UCI", linetype = NA),
              alpha = 0.2) +
  geom_line(size = 0.5) +
  geom_hline(yintercept = 100) +
  #geom_point(size = 2) +
  ylab("Occupancy") +
  xlab("Year") +
  #ggtitle("TERRESTRIAL_INSECTS") +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(1970, 2015)) +
  theme(plot.title = element_text(size = 12), text = element_text(size = 12), aspect.ratio = 1,
        legend.title = element_blank()) +
  #plot.margin = unit(c(0,0,0,0), "cm")
  guides(colour = guide_legend(ncol = 2))


library(cowplot)
plot_grid(p[[2]], p[[1]], p[[4]], p[[3]], align = "hv", ncol = 2,
          labels = c("Freshwater Species",
                     "Insects",
                     "Inverts",
                     "Bryophytes & Lichens"),
          hjust = 0, label_size = 12, label_x = 0.1)

ggsave(filename = paste0(outdir, "/Figure_4.pdf"), height = 10, width = 16)
