##%######################################################%##
#                                                          #
####           Function to generate figure 1            ####
#                                                          #
##%######################################################%##


#datadir <- "C:/Users/charl/Dropbox/PhD WORK/1. BIG PAPER/Repository downloads"

# load the major groups list
#data(major_groups)
major_groups <- read.csv("C:/Users/charl/Dropbox/PhD WORK/1. BIG PAPER/UKBiodiversity/Data/Major_groups.csv")


## loop through each species list and make an indicator

for(group in unique(major_groups$Major_group)){

  species_list <- major_groups[major_groups$Major_group == group, "Species"]

  temp_post <- NULL

  # load each species posterior and cut out the bits needed.

  allfiles <- list.files(paste0(datadir, "/POSTERIOR_SAMPLES/"))

  for(i in 1:length(species_list)){


    sp <- allfiles[grep(species_list[i], allfiles)]

    sp_post <- read.csv(paste0(datadir, "/POSTERIOR_SAMPLES/", sp))



    # just GB or UK

    if(sum(grep("UK", sp_post$Region)) > 0) {


      sp_post <- sp_post[sp_post$Region == "UK", ]

      sp_post <- sp_post[, c(5:50, 2, 4)]

      colnames(sp_post) <- c(1970:2015, "spp", "iter")


    }else{

      sp_post <- sp_post[sp_post$Region == "GB", ]

      sp_post <- sp_post[, c(5:50, 2, 4)]

      colnames(sp_post) <- c(1970:2015, "spp", "iter")


    }





    # add into temp_post table
    temp_post <- rbind(temp_post, sp_post)

  } # end of loop through species


  j_post <- temp_post[2:nrow(temp_post), ]


  # convert 0 and 1 to 0.0001 and 0.9999 - solve the issue with logging zero and 1
  temp_post <- j_post[,1:(ncol(j_post)-2)]
  temp_post[temp_post == 0] <- 0.0001
  temp_post[temp_post == 1] <- 0.9999
  temp_post <- cbind(temp_post, j_post[,c("spp","iter")])

  j_post <- temp_post

  all_means <- NULL

  n_sp <- length(unique(j_post$spp))

  for(i in 1:1000){

    # subset the all ant data so that only have the i samples
    j_post_iter <- j_post[j_post$iter == i, ]

    geo_means <- apply(j_post_iter[1:47], 2, calc_geo)

    all_means <- rbind(all_means, geo_means)
  } # end of loop through iterations

  write.csv(all_means, file = paste(output_dir, "/", m_group, "_indicator_posterior_vals.csv", sep = ""))

  # calculate mean and 90% CIs
  final <- data.frame(avg_occ = apply(all_means, 2, mean, na.rm = TRUE),
                      upper_CI = apply(all_means, 2, quantile, probs = 0.95, na.rm = TRUE),
                      lower_CI = apply(all_means, 2, quantile, probs = 0.05, na.rm = TRUE))


  write.csv(final, file = paste(output_dir, "/", m_group, "_indicator_values.csv", sep = ""))

  ### organise the data for plotting ###
  final$year <- as.numeric(rownames(final))


  ggplot(data = final, aes(x = year, y = avg_occ)) +
    theme_bw() +
    geom_ribbon(data = final,
                aes_string(ymin = 'lower_CI', ymax = 'upper_CI', linetype = NA),
                alpha = 0.2) +
    #geom_point(size = 1) +
    geom_line(size = 1) +
    scale_y_continuous(limits = c(0, 0.6)) +
    scale_x_continuous(limits = c(1970, 2015)) +
    xlab("Year") +
    ylab("Average occupancy") +
    ggtitle(paste(m_group, ", ", n_sp, " species", sep = ""))+
    theme(plot.title = element_text(size = 10))

  ggsave(paste(m_group,"_weighted_mean_occ_plot.png", sep=""), plot = last_plot(), path=output_dir,width=4,height=4)



  rescale <- function(x){

    multipier <- 100/x[1]
    rescaled <- x*multipier
    return(rescaled)
  }

  final_rescaled <- final
  final_rescaled[, 1:3] <- apply(final_rescaled[, 1:3], 2, rescale)

  write.csv(final_rescaled, file = paste(output_dir, "/", m_group, "_indicator_values_RESCALED.csv", sep = ""))


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
    ggtitle(paste(m_group, ", ", n_sp, " species", sep = ""))+
    theme(plot.title = element_text(size = 10))


  ggsave(paste(m_group,"_weighted_mean_occ_plot_RESCALED.png", sep=""), plot = last_plot(), path=output_dir,width=4,height=4)

}

