#' Generate Figure 3
#'
#' This takes the posterior values of the group level indicators generated within the
#' \code{generage_fig1} function, calculates the quantiles and plots the group level quantiles presented in figure 3.
#'
#' @param postdir A filepath specifying where the posteior samples that are generated
#' from the combine_posteriors function are saved. are saved. If outputs have not been
#' moved, this will be in a directory "/MajorGroups".
#' @param save_plot Logical.  If `TRUE`, the plot will be saved as a PDF file
#' within the `postdir`. Default is `TRUE`.
#' @param interval A number between 0 and 100 indicating the percentiles of the credible intervals to be plotted and reported.
#' Defaults to 95%.
#'
#' @keywords trends, species, distribution, occupancy
#' @references Outhwaite et al (in prep) Complexity of biodiversity change revealed through long-term trends of invertebrates, bryophytes and lichens.
#' @references Outhwaite, C. L., Powney, G. D., August, T. A., Chandler, R. E., Rorke, S., Pescott, O., … Isaac, N. J. B. (2019). Annual estimates of
#'  occupancy for bryophytes, lichens and invertebrates in the UK (1970-2015).
#'  NERC Environmental Information Data Centre. https://doi.org/10.5285/0ec7e549-57d4-4e2d-b2d3-2199e1578d84
#' @examples
#' \dontrun{
#'
#' # Run generate_fig2 function to produce boxplot.
#' # datadir should be the filepath of where the posterior indicator values are saved.
#' # generate_fig2(postdir = paste0(getwd(), "/MajorGroups/geomeans"),
#' save_plot = TRUE,
#' interval = 90)
#'
#' }
#' @export
#' @import ggplot2


generate_fig3 <- function(postdir, save_plot = TRUE, interval=95){

# where to save the outputs
outdir <- paste0(postdir, "/quantiles")
if(!dir.exists(outdir)) dir.create(outdir) else print("Warning: overwriting existing files")

# list the
files <- list.files(postdir, pattern = ".rdata")

# remove ALL species file
files <- files[!grepl("ALL", files)]


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

  # convert inverval (a number between 0 and 100) into quantiles
  if(interval > 100 | interval < 0) stop("Interval must be between 0 and 100")
  q <- 0.5 + (c(-1,1)*interval/200)

  # loop through each iteration and take the geometric mean
  for(i in 1:1000){

    # subset the all ant data so that only have the i samples
    j_post_iter <- j_post[j_post$iter == i, ]

    quant_0.25 <- apply(j_post_iter[1:46], 2, quantile, probs = 0.25, na.rm = T)

    quant_0.75 <- apply(j_post_iter[1:46], 2, quantile, probs = 0.75, na.rm = T)

    result <- rbind(quant_0.25, quant_0.75)


    all_quants <- rbind(all_quants, result)


    } # end of loop through iterations


  # save the posterior geometric means
  write.csv(all_quants, file = paste0(outdir, "/", group, "_quantiles_posterior_vals.csv"), row.names = FALSE)

  # rescale to start at 100 in 1970.
  all_quants_rescaled <- t(apply(all_quants, 1, rescale))


  # calculate mean and 95% CIs
  quants_rescaled <- data.frame(avg_0.25 = apply(all_quants_rescaled[rownames(all_quants_rescaled) == "quant_0.25",], 2, mean, na.rm = TRUE),
                      upper_CI_0.25 = apply(all_quants_rescaled[rownames(all_quants_rescaled) == "quant_0.25",], 2, quantile, probs = q[2], na.rm = TRUE),
                      lower_CI_0.25 = apply(all_quants_rescaled[rownames(all_quants_rescaled) == "quant_0.25",], 2, quantile, probs = q[1], na.rm = TRUE),
                      avg_occ_0.75 = apply(all_quants_rescaled[rownames(all_quants_rescaled) == "quant_0.75",], 2, mean, na.rm = TRUE),
                      upper_CI_0.75 = apply(all_quants_rescaled[rownames(all_quants_rescaled) == "quant_0.75",], 2, quantile, probs = q[2], na.rm = TRUE),
                      lower_CI_0.75 = apply(all_quants_rescaled[rownames(all_quants_rescaled) == "quant_0.75",], 2, quantile, probs = q[1], na.rm = TRUE))


  # add in the year
  quants_rescaled$year <- as.numeric(rownames(quants_rescaled))

  # Save the rescaled indicator values
  write.csv(quants_rescaled, file = paste0(outdir, "/", group, "_rescaled_quantile_vals.csv"), row.names = FALSE)


}


# Read in and organise all rescaled indicator files

# list the files of rescaled indicator values.  One per group.
files <- list.files(outdir, pattern = "_rescaled_quantile_vals")

# combine into one matrix
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
colnames(all_plot_data) <- c("mean_0.25", "UCI_0.25", "LCI_0.25", "mean_0.75", "UCI_0.75", "LCI_0.75","year", "group")

# change name labels
all_plot_data$group <- sub("FRESHWATER_SPECIES", "Freshwater species", all_plot_data$group)
all_plot_data$group <- sub("LOWER_PLANTS", "Bryophytes & lichens", all_plot_data$group)
all_plot_data$group <- sub("TERRESTRIAL_INSECTS", "Insects", all_plot_data$group)
all_plot_data$group <- sub("TERRESTRIAL_NONINSECT_INVERTS", "Invertebrates", all_plot_data$group)



# change order of the lines
all_plot_data$group <- as.factor(all_plot_data$group)
all_plot_data$group <- factor(all_plot_data$group, levels(all_plot_data$group)[c(2,3,4,1)])


p1 <- ggplot() +
  geom_line(data = all_plot_data, aes(x = year, y = mean_0.25), colour = c("#7D26CD"), size = 0.4) +
  geom_ribbon(data = all_plot_data, aes_string(x = 'year', ymin = 'LCI_0.25', ymax = 'UCI_0.25', linetype = NA),
              alpha = 0.4, fill = c("#7D26CD")) +
  geom_line(data = all_plot_data, aes(x = year, y = mean_0.75), colour = c("#008B8B"), size = 0.4) +
  geom_ribbon(data = all_plot_data, aes_string(x = 'year', ymin = 'LCI_0.75', ymax = 'UCI_0.75', linetype = NA),
              alpha = 0.4, fill = c("#008B8B")) +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.2) +
  scale_y_continuous(limits = c(20, 160)) +
  scale_x_continuous(limits = c(1970, 2015)) +
  xlab("Year") +
  ylab("Index of occupancy (1970 = 100)") +
  facet_wrap(facets = ~ group, nrow = 2, ncol = 2) +
  theme_bw() +
  theme(aspect.ratio = 1, strip.text = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(size = 8),
        panel.border = element_rect(size = 0.2),
        axis.ticks = element_line(size = 0.2),
        strip.background = element_rect(size = 0.2))

if(save_plot == TRUE){
# save the plot as a pdf
ggsave(filename = paste0(outdir, "/Figure_3.pdf"), plot = p1, height = 6, width = 6)
}

return(p1)
}


