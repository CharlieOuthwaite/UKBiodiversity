#' Generating Figure 4
#'
#' This takes the taxa level posterior combinations generated using the
#' \code{combine_posteriors} function and generates figure 4 presented within the paper.
#'
#' @param postdir A filepath specifying where the posterior combinations are saved.
#' @param save_plot Logical.  If `TRUE`, the plot will be saved as a PDF file
#' within the `postdir`. Default is `TRUE`.
#' @parm intervals A number between 0 and 100 indicating the percentiles of the credible intervals to be plotted and reported. Defaults to 90%

#'
#' @keywords trends, species, distribution, occupancy
#' @references Outhwaite et al (in prep) Complexity of biodiversity change revealed through long-term trends of invertebrates, bryophytes and lichens.
#' @references Outhwaite, C. L., Powney, G. D., August, T. A., Chandler, R. E., Rorke, S., Pescott, O., … Isaac, N. J. B. (2019). Annual estimates of
#'  occupancy for bryophytes, lichens and invertebrates in the UK (1970-2015).
#'  NERC Environmental Information Data Centre. https://doi.org/10.5285/0ec7e549-57d4-4e2d-b2d3-2199e1578d84
#' @examples
#' \dontrun{
#'
#' # Run generate_fig4 function
#' # postdir should be the filepath of where the 4 major group level posteriors
#' # combinationss are saved.
#' generate_fig4(postdir = paste0(getwd(), "/Taxa"))
#'
#' }
#' @export
#' @import ggplot2
#' @importFrom cowplot plot_grid

generate_fig4 <- function(postdir, save_plot = TRUE, interval=95){

# convert inverval (a number between 0 and 100) into quantiles
if(interval > 100 | interval < 0) stop("Interval must be between 0 and 100") 
q <- 0.5 + (c(-1,1)*interval/200)
  
# where to save the outputs
outdir <- paste0(postdir, "/geomeans")
if(!dir.exists(outdir)) dir.create(outdir) else print("Warning: overwriting existing files")

# list the group level files
files <- list.files(postdir, pattern = "posterior_samples")

# loop through each group and generate the indicator values
for(file in files){

  # extract the group name
  group <- sub("_posterior_samples_national.rdata", "", file)

  # load in the combined posterior generated from the combine_posteriors function
  load(paste0(postdir, "/", file))

  # convert 0 and 1 to 0.0001 and 0.9999 - solve the issue with logging 0 and 1
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

    # subset the data so that only have the i samples
    j_post_iter <- j_post[j_post$iter == i, ]

    # calculate the geometric mean
    geo_means <- apply(j_post_iter[1:46], 2, calc_geo)

    # save output
    all_means <- rbind(all_means, geo_means)

  } # end of loop through iterations


  # save the posterior geometric means
  write.csv(all_means, file = paste(outdir, "/", group, "_indicator_posterior_vals.csv", sep = ""), row.names = FALSE)

  # rescale the values to start at 100 in 1970
  all_means_rescaled <- t(apply(all_means, 1, rescale))

  # calculate mean and 95% CIs
  final_rescaled <- data.frame(avg_occ = apply(all_means_rescaled, 2, mean, na.rm = TRUE),
                               upper_CI = apply(all_means_rescaled, 2, quantile, probs = q[2], na.rm = TRUE),
                               lower_CI = apply(all_means_rescaled, 2, quantile, probs = q[1], na.rm = TRUE))

  # add in the year
  final_rescaled$year <- as.numeric(rownames(final_rescaled))

  # Save the rescaled indicator values
  write.csv(final_rescaled, file = paste0(outdir, "/", group, "_rescaled_indicator_vals.csv"), row.names = FALSE)


} # end of loop through group files

### Read in and organise all rescaled indicator files ###

# list the files of rescaled indicator values.  One per group.
files <- list.files(outdir, pattern = "_rescaled_indicator_vals")

# somewhere to save the info
all_plot_data <- NULL

# combine all files for plot data
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


# get data on major groupings
major_groups <- as.data.frame(major_groups)

# space to save major group
all_plot_data$major_group <- NA

# look up the major groups from the major_groups file
for(grp in unique(all_plot_data$group)){

  # look up the major group for that taxa
  major_grp <- as.character(unique(major_groups[major_groups$Group == grp, "Major_group"]))

  # add into plot_data table
  all_plot_data[all_plot_data$group == grp, "major_group"] <- major_grp

} # end of loop through groups


## a function to create a plot per group
plot_group <- function(major_group){


  ggplot(all_plot_data[all_plot_data$major_group == major_group,], aes_string(x = "year", y = "mean", col = "group", fill = "group")) +
    theme_bw() +
    geom_ribbon(data = all_plot_data[all_plot_data$major_group == major_group,],
                aes_string(ymin = "LCI", ymax = "UCI", linetype = NA),
                alpha = 0.2) +
    geom_line(size = 0.5) +
    geom_hline(yintercept = 100) +
    ylab("Occupancy") +
    xlab("Year") +
    scale_y_continuous(limits = c(0, 200)) +
    scale_x_continuous(limits = c(1970, 2015)) +
    theme(plot.title = element_text(size = 12), text = element_text(size = 12),
          aspect.ratio = 1,
          legend.title = element_blank())+
    guides(colour = guide_legend(ncol = 1))
}

# where to store plots
p = list()

# run function across each group
for(i in 1:length(unique(all_plot_data$major_group))){
  major_group <- unique(all_plot_data$major_group)[i]

  p[[i]] = plot_group(major_group)
}

# a separate plot for insects that has a larger legend
p[[1]] <-   ggplot(all_plot_data[all_plot_data$major_group == "TERRESTRIAL_INSECTS",], aes_string(x = "year", y = "mean", col = "group", fill = "group")) +
  theme_bw() +
  geom_ribbon(data = all_plot_data[all_plot_data$major_group == "TERRESTRIAL_INSECTS",],
              aes_string(ymin = "LCI", ymax = "UCI", linetype = NA),
              alpha = 0.2) +
  geom_line(size = 0.5) +
  geom_hline(yintercept = 100) +
  ylab("Occupancy") +
  xlab("Year") +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(1970, 2015)) +
  theme(plot.title = element_text(size = 12), text = element_text(size = 12), aspect.ratio = 1,
        legend.title = element_blank()) +
  guides(colour = guide_legend(ncol = 2))

# organise plots using cowplot function
plot_grid(p[[2]], p[[1]], p[[4]], p[[3]], align = "hv", ncol = 2,
          labels = c("Freshwater Species",
                     "Insects",
                     "Inverts",
                     "Bryophytes & Lichens"),
          hjust = 0, label_size = 12, label_x = 0.1)

if(save_plot == TRUE){
# save the plot
ggsave(filename = paste0(outdir, "/Figure_4.pdf"), height = 10, width = 16)

}


return(plot_grid(p[[2]], p[[1]], p[[4]], p[[3]], align = "hv", ncol = 2,
                 labels = c("Freshwater Species",
                            "Insects",
                            "Inverts",
                            "Bryophytes & Lichens"),
                 hjust = 0, label_size = 12, label_x = 0.1))

}
