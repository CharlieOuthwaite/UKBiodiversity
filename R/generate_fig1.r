##%######################################################%##
#                                                          #
####           Function to generate figure 1            ####
#                                                          #
##%######################################################%##




# function to calculate the geometric mean
calc_geo <- function(x){
  exp(mean(log(x), na.rm = TRUE))
}

species_lists <- list.files(output_dir, pattern = "Good_sp")

# species_list <- read.csv("W:/PYWELL_SHARED/Pywell Projects/BRC/Charlie/1.c. New Model Rerun/6. Indicators and other analyses/Good_sp_list_PAPER.csv")

## loop through each species list and make an indicator

for(sp_list in species_lists){

  species_list <- read.csv(paste(output_dir, "/", sp_list, sep = ""))

  temp_post <- matrix(NA, ncol = 49)
  colnames(temp_post) <- c(1970:2016, "spp", "iter")

  m_group <- sub("_G.*", "", sp_list)
  #m_group <- "ALL"

  # get the posterior for species within each taxa
  for(group in unique(species_list$GROUP)){

    # which species do we want
    sp <- species_list[species_list$GROUP == group, 'SPECIES']

    # read in the posterior samples for that group
    if(group %in% c("Bryophytes", "Lichens", "VascPlants", "Dragonflies", "Moths")){

      load(paste(dir, "/", group, "_jasmin_psi_sampled_posterior_1000.rdata", sep = ""))

      if(length(grep("species", names(j_post))) == 1){
        names(j_post) <- sub("species", "spp", names(j_post))
        names(j_post) <- sub("iteration", "iter", names(j_post))
      }

    }else{
      load(paste(dir, "/", group, "_cirrus_psi_sampled_posterior_1000.rdata", sep = ""))

    } # end of load posterior

    # just want UK or GB level
    if(sum(grep("UK", names(j_post))) > 0){

      j_post <- j_post[, c(grep("UK", names(j_post)), grep("spp", names(j_post)), grep("iter", names(j_post)))]


    }else{

      j_post <- j_post[, c(grep("GB", names(j_post)), grep("spp", names(j_post)), grep("iter", names(j_post)))]

    }


    # subset to required species
    sp_post <- j_post[j_post$spp %in% sp, ]



    if(group == "RoveBeetles"){

      NA_tab <- matrix(nrow = 79000, ncol = 10, data = NA)
      sp_post <- cbind(NA_tab, sp_post)

      colnames(sp_post) <- c(1970:2016, "spp", "iter")

    }else{

      if(ncol(sp_post) != 49){

        col <- 49 - ncol(sp_post)
        NA_tab <- matrix(nrow = nrow(sp_post), ncol = col, data = NA)

        sp_post <- cbind(sp_post[, 1:(ncol(sp_post)-2)], NA_tab, sp_post[, (ncol(sp_post)-1):ncol(sp_post)])


      }


      colnames(sp_post) <- c(1970:2016, "spp", "iter")

    }


    # add into temp_post table
    temp_post <- rbind(temp_post, sp_post)

  } # end of loop through groups
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

