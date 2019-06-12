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
files <- list.files(postdir)

# loop through each group and generate the indicator values

for(file in files){

  group <- sub("_posterior_samples_national.rdata", "", file)

  # load in the combined posterior generated from the combine_posteriors function
  load(paste0(postdir, "/", file))

  # convert 0 and 1 to 0.0001 and 0.9999 - solve the issue with logging zero and 1
  temp_post <- group_post[,1:(ncol(group_post)-2)]
  temp_post[temp_post == 0] <- 0.0001
  temp_post[temp_post == 1] <- 0.9999
  temp_post <- cbind(temp_post, group_post[,c("spp","iter")])

  j_post <- temp_post

  all_means <- NULL

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


  #write.csv(final, file = paste(output_dir, "/", m_group, "_indicator_values.csv", sep = ""))


all_means_rescaled <- t(apply(all_means, 1, rescale))


# calculate mean and 90% CIs
final_rescaled <- data.frame(avg_occ = apply(all_means_rescaled, 2, mean, na.rm = TRUE),
                    upper_CI = apply(all_means_rescaled, 2, quantile, probs = 0.95, na.rm = TRUE),
                    lower_CI = apply(all_means_rescaled, 2, quantile, probs = 0.05, na.rm = TRUE))

final_rescaled$year <- as.numeric(rownames(final_rescaled))


write.csv(final_rescaled, file = paste0(outdir, "/", group, "_rescaled_indicator_vals.csv"), row.names = FALSE)


}



ggplot(data = final_rescaled, aes(x = year, y = avg_occ)) +
  theme_bw() +
  geom_ribbon(data = final_rescaled,
              aes_string(ymin = 'lower_CI', ymax = 'upper_CI', linetype = NA),
              alpha = 0.2) +
  #geom_point(size = 1) +
  geom_line(size = 1) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(1970, 2015)) +
  xlab("Year") +
  ylab("Average occupancy") +
  ggtitle(paste(group, ", ", n_sp, " species", sep = ""))+
  theme(plot.title = element_text(size = 10))


