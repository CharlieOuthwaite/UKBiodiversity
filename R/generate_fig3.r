##%######################################################%##
#                                                          #
####           Function to generate Figure 3            ####
#                                                          #
##%######################################################%##


# where are the posteriors?
postdir <- "C:/Users/charl/Dropbox/PhD WORK/1. BIG PAPER/Package_testing/Major_groups"

# where to save the outputs
dir.create(paste0(postdir, "/geomeans"))
outdir <- paste0(postdir, "/geomeans")

# list the
files <- list.files(postdir)


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
  all_quants <- NULL

  # add the number of species
  n_sp <- length(unique(j_post$spp))

  # loop through each iteration and take the geometric mean
  for(i in 1:1000){

    # subset the all ant data so that only have the i samples
    j_post_iter <- j_post[j_post$iter == i, ]

    quant_0.25 <- apply(j_post_iter[1:47], 2, quantile, probs = 0.25)

    quant_0.5 <- apply(j_post_iter[1:47], 2, quantile, probs = 0.5)

    quant_0.75 <- apply(j_post_iter[1:47], 2, quantile, probs = 0.75)

    result <- rbind(quant_0.25, quant_0.5, quant_0.75)


    all_quants <- rbind(all_quants, result)


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
