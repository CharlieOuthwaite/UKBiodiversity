##%######################################################%##
#                                                          #
####           Function to generate figure 1            ####
#                                                          #
##%######################################################%##

# where are the posteriors?
postdir <- "C:/Users/charl/Dropbox/PhD WORK/1. BIG PAPER/Package_testing/Major_groups"

# where to save the outputs
dir.create(paste0(postdir, "/geomeans"))
outdir <- paste0(postdir, "/geomeans")

# list the
files <- list.files(postdir, pattern = ".rdata")

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
write.csv(all_means, file = paste(outdir, "/", group, "_indicator_posterior_vals.csv", sep = ""), row.names = FALSE)


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
  plot_data$group <- sub("_re.*", "", file)

  # combine into one data table
  all_plot_data <- rbind(all_plot_data, plot_data)
}

# change column names
colnames(all_plot_data) <- c("mean", "UCI", "LCI", "year", "group")

# change name labels
all_plot_data$group <- sub("FRESHWATER_SPECIES", "Freshwater, n = 318", all_plot_data$group)
all_plot_data$group <- sub("LOWER_PLANTS", "Bryophytes & lichens, n = 1269", all_plot_data$group)
all_plot_data$group <- sub("TERRESTRIAL_INSECTS", "Insects, n = 3168", all_plot_data$group)
all_plot_data$group <- sub("TERRESTRIAL_NONINSECT_INVERTS", "Inverts, n = 538", all_plot_data$group)



# change order of the lines
all_plot_data$group <- as.factor(all_plot_data$group)
all_plot_data$group <- factor(all_plot_data$group, levels(all_plot_data$group)[c(2,3,4,1)])

# recreate figure 1
ggplot(all_plot_data, aes_string(x = "year", y = "mean", col = 'group', fill = "group")) +
  theme_bw() +
  geom_ribbon(aes_string(ymin = "LCI", ymax = "UCI", linetype = NA),
              alpha = 0.3) +
  geom_line(size = 0.5) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  ylab("Average Occupancy\n") +
  xlab("\nYear") +
  scale_y_continuous(limits = c(40, 150), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1970, 2015), expand = c(0, 0)) +
  ggtitle("Figure 1") +
  theme(text = element_text(size = 10), aspect.ratio = 1, legend.title = element_blank(),
        legend.position = c(0.2,0.85), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())


ggsave(filename = paste0(outdir, "/Figure_1.pdf"), height = 6, width = 6)














