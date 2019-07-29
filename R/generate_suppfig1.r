#' Generating Supplementary Figure 1
#'
#' This generates version of figure 1 but with subsets of species based on the number of records available for the occupancy estimation.
#'
#' @param postdir A filepath specifying where the posterior combinations are saved.
#' @param sp_trends A dataframe, downloaded from the repository which details the species
#' level trends in occupancy and the years for which data were avaialble for each species.
#' @param status Logical. If `TRUE` the progress through each group and number of records subset will be printed to the console.
#' @param save_plot Logical.  If `TRUE`, the plot will be saved as a PDF file
#' within the `outdir`. Default is `TRUE`.
#'
#' @keywords trends, species, distribution, occupancy
#' @references Outhwaite et al (in prep) Complexity of biodiversity change revealed through long-term trends of invertebrates, bryophytes and lichens.
#' @references Outhwaite, C. L., Powney, G. D., August, T. A., Chandler, R. E., Rorke, S., Pescott, O., … Isaac, N. J. B. (2019). Annual estimates of
#'  occupancy for bryophytes, lichens and invertebrates in the UK (1970-2015).
#'  NERC Environmental Information Data Centre. https://doi.org/10.5285/0ec7e549-57d4-4e2d-b2d3-2199e1578d84
#' @examples
#' \dontrun{
#'
#' # Run generate_fig1 function
#' # postdir should be the filepath of where the 4 major group level posteriors
#' # combinationss are saved.
#' generate_fig1supp(postdir = paste0(getwd(), "/MajorGroups"),
#' sp_trends = read.csv(paste0(datadir, "/Species_Trends.csv")),
#' outdir = paste0(getwd(), "/MajorGroups"))
#'
#' }
#' @export
#' @import ggplot2
#' @import cowplot

generate_fig1supp  <- function(postdir, sp_trends, status = TRUE, save_plot = TRUE){

  dir.create(paste0(postdir, "/supplementary"))
  outdir <- paste0(postdir, "/supplementary")


  # list the posterior combination files
  files <- list.files(postdir, pattern = ".rdata")

  # check there's only 4 sets of posteriors
  if(length(files) != 5) stop("There are more than 5 datafiles in the directory.")

  # remove the all species files
  files <- files[!grepl("ALL", files)]

  # remove dodgy characters
  sp_trends$Species <- sub("/", "_", sp_trends$Species)
  sp_trends$Species <- gsub("\\?", "", sp_trends$Species)

  # specify the subsets based on minimum number of records per species
  vals <- c(50, 75, 100, 150, 200)

  # for each value, subset the species and generate fig 1.
  for(val in vals){

  # which species to include
    species <- as.character(sp_trends[sp_trends$N_Records >= val, "Species"])

  # loop through each group and generate the indicator values
  for(file in files){

    # extract the group name
    group <- sub("_posterior_samples_national.rdata", "", file)

    # load in the combined posterior generated from the combine_posteriors function
    load(paste0(postdir, "/", file))

    # subset to those species with minimum recs = val
    group_post <- group_post[group_post$spp %in% species, ]

    # how many species in the subset
    nsp <- length(unique(group_post$spp))

    if(status == TRUE) print(paste0(group, ", nrec minimum = ", val))

    # convert 0 and 1 to 0.0001 and 0.9999 - solve the issue with logging 0 and 1
    temp_post <- group_post[,1:(ncol(group_post)-2)]
    temp_post[temp_post == 0] <- 0.0001
    temp_post[temp_post == 1] <- 0.9999
    temp_post <- cbind(temp_post, group_post[,c("spp","iter")])

    j_post <- temp_post

    # somewhere to save the means
    all_means <- NULL

    # loop through each iteration and take the geometric mean
    for(i in 1:1000){

      # subset the all ant data so that only have the i samples
      j_post_iter <- j_post[j_post$iter == i, ]

      geo_means <- apply(j_post_iter[1:46], 2, calc_geo)

      all_means <- rbind(all_means, geo_means)
    } # end of loop through iterations


    # save the posterior geometric means
    write.csv(all_means, file = paste0(outdir, "/", group, "_indicator_posterior_vals_", val,  ".csv"), row.names = FALSE)

    # rescale the valiues to start at 100 in 1970
    all_means_rescaled <- t(apply(all_means, 1, rescale))


    # calculate mean and 95% CIs
    final_rescaled <- data.frame(avg_occ = apply(all_means_rescaled, 2, mean, na.rm = TRUE),
                                 upper_CI = apply(all_means_rescaled, 2, quantile, probs = 0.95, na.rm = TRUE),
                                 lower_CI = apply(all_means_rescaled, 2, quantile, probs = 0.05, na.rm = TRUE))

    # add in the year
    final_rescaled$year <- as.numeric(rownames(final_rescaled))

    # Save the rescaled indicator values
    write.csv(final_rescaled, file = paste0(outdir, "/", group, "_rescaled_indicator_", val,  ".csv"), row.names = FALSE)


  } # end of loop through files

  } # loop through each val


  #### Read in and organise all rescaled indicator files ####


  # list the files of rescaled indicator values.  One per group.
  files <- list.files(outdir, pattern = paste0("rescaled_indicator_"))


  # somewhere to save the info
  all_plot_data <- NULL

  # combine all files to get a matrix of plot data
  for(file in files){

    # read in first group plot data
    plot_data <- read.csv(paste0(outdir, "/", file))

    # add a group name column
    plot_data$group <- sub("_re.*", "", file)

    nrecs <- sub(".*_", "", file)
    nrecs <- sub(".csv", "", nrecs)

    plot_data$nrecs <- nrecs

    # combine into one data table
    all_plot_data <- rbind(all_plot_data, plot_data)
  }

  # change column names
  colnames(all_plot_data) <- c("mean", "UCI", "LCI", "year", "group", "nrecs")

  # change name labels
  all_plot_data$group <- sub("FRESHWATER_SPECIES", "Freshwater", all_plot_data$group)
  all_plot_data$group <- sub("LOWER_PLANTS", "Bryophytes & lichens", all_plot_data$group)
  all_plot_data$group <- sub("TERRESTRIAL_INSECTS", "Insects", all_plot_data$group)
  all_plot_data$group <- sub("TERRESTRIAL_NONINSECT_INVERTS", "Inverts", all_plot_data$group)

  # change order of the lines
  all_plot_data$group <- as.factor(all_plot_data$group)
  all_plot_data$group <- factor(all_plot_data$group, levels(all_plot_data$group)[c(2,3,4,1)])

  # change order of nrecs
  all_plot_data$nrecs <- as.factor(all_plot_data$nrecs)
  all_plot_data$nrecs <- factor(all_plot_data$nrecs, levels(all_plot_data$nrecs)[c(4,5,1,2,3)])



p1 <- ggplot(all_plot_data, aes_string(x = "year", y = "mean", col = 'group', fill = "group")) +
    theme_bw() +
    geom_ribbon(aes_string(ymin = "LCI", ymax = "UCI", linetype = NA),
                alpha = 0.3) +
    geom_line(size = 0.2) +
    geom_hline(yintercept = 100, linetype = "dashed", size = 0.2) +
    ylab("Average Occupancy\n") +
    xlab("\nYear") +
    scale_y_continuous(limits = c(40, 150), expand = c(0, 0)) +
    scale_x_continuous(limits = c(1970, 2015), expand = c(0, 0)) +
    theme(text = element_text(size = 10), aspect.ratio = 1, legend.title = element_blank(),
          legend.position = "bottom", panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          strip.background = element_rect(size = 0.2),
          panel.border = element_rect(size = 0.2),
          axis.ticks = element_line(size = 0.2)) +
    facet_wrap(~nrecs, nrow = 2)

if(save_plot == TRUE){
  # save the plot
  ggsave(filename = paste0(outdir, "/Supp_Fig1.pdf"), plot = p1, height = 10, width = 16)

  }

return(p1)
  }








