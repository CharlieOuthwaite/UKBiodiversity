#' Generating Figure 1
#'
#' This takes the major group level posterior combinations generated using the
#' \code{combine_posteriors} function and generates figure 1 presented within the paper.
#' It also generates the posteriors of average change across species for each group that
#' are also used in the estimation of major group level trends presented within the text
#' of the associated paper and for the generation of figure 3.
#'
#' @param postdir A filepath specifying where the posterior combinations are saved.
#' @param status Logical.  If `TRUE`, group name will be printed to the console
#' as means and indicator values are being estimated. Default is `TRUE`.
#' @param save_plot Logical. If `TRUE` plot will be saved as a PDF file as well
#' as being returned to the console.
#' @param interval A number between 0 and 100 indicating the percentiles of the credible intervals to be plotted and reported.
#' Defaults to 95%
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
#' # combinations are saved.
#' generate_fig1(postdir = paste0(getwd(), "/MajorGroups"),
#' status = TRUE,
#' save_plot = TRUE,
#' interval = 90)
#'
#' }
#' @export
#' @import ggplot2
#' @import reshape2

generate_fig1  <- function(postdir, status = TRUE, save_plot = TRUE, interval=95){

# where to save the outputs
outdir <- paste0(postdir, "/geomeans")
if(!dir.exists(outdir)) dir.create(outdir) else print("Warning: overwriting existing files")


# list the posterior combination files
files <- list.files(postdir, pattern = ".rdata")

# check there's only 4 sets of posteriors
if(length(files) != 5) stop("There are more than 5 datafiles in the directory.")

# convert inverval (a number between 0 and 100) into quantiles
if(interval > 100 | interval < 0) stop("Interval must be between 0 and 100")
q <- 0.5 + (c(-1,1)*interval/200)

# loop through each group and generate the indicator values
for(file in files){

  # extract the group name
  group <- sub("_posterior_samples_national.rdata", "", file)

  if(status == TRUE) print(group)

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

  # calculate geometric mean occupancy
  j_post <- melt(j_post, id=c("spp","iter"))
  j_post <- acast(j_post, spp~iter~variable)
  all_means <- apply(j_post, c(2,3), calc_geo)

# save the posterior geometric means
write.csv(all_means, file = paste(outdir, "/", group, "_indicator_posterior_vals.csv", sep = ""), row.names = FALSE)

# rescale the valiues to start at 100 in 1970
all_means_rescaled <- t(apply(all_means, 1, rescale))


# calculate mean and CIs
final_rescaled <- data.frame(avg_occ = apply(all_means_rescaled, 2, mean, na.rm = TRUE),
                    upper_CI = apply(all_means_rescaled, 2, quantile, probs = q[2], na.rm = TRUE),
                    lower_CI = apply(all_means_rescaled, 2, quantile, probs = q[1], na.rm = TRUE))

# add in the year
final_rescaled$year <- as.numeric(rownames(final_rescaled))

# Save the rescaled indicator values
write.csv(final_rescaled, file = paste0(outdir, "/", group, "_rescaled_indicator_vals.csv"), row.names = FALSE)

} # end of loop through files

#### Read in and organise all rescaled indicator files ####

# list the files of rescaled indicator values.  One per group.
files <- list.files(outdir, pattern = "_rescaled_indicator_vals")

# remove ALL species file
files <- files[!grepl("ALL", files)]

# somewhere to save the info
all_plot_data <- NULL

# combine all files to get a matrix of plot data
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
all_plot_data$group <- sub("TERRESTRIAL_INSECTS", "Insects, n = 3089", all_plot_data$group)
all_plot_data$group <- sub("TERRESTRIAL_NONINSECT_INVERTS", "Inverts, n = 538", all_plot_data$group)

# change order of the lines
all_plot_data$group <- as.factor(all_plot_data$group)
all_plot_data$group <- factor(all_plot_data$group, levels(all_plot_data$group)[c(2,3,4,1)])

# recreate figure 1
p1 <- ggplot(all_plot_data, aes_string(x = "year", y = "mean", col = 'group', fill = "group")) +
  theme_bw() +
  geom_ribbon(aes_string(ymin = "LCI", ymax = "UCI", linetype = NA),
              alpha = 0.3) +
  geom_line(size = 0.5) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  ylab("Average Occupancy\n") +
  xlab("\nYear") +
  scale_y_continuous(limits = c(40, 150), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1970, 2015), expand = c(0, 0)) +
  theme(text = element_text(size = 10), aspect.ratio = 1, legend.title = element_blank(),
        legend.position = c(0.2,0.85), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

if(save_plot == TRUE){
# save the plot
ggsave(filename = paste0(outdir, "/Figure_1.pdf"), plot = p1, height = 6, width = 6)
}

return(p1)

}








