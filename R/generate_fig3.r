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
#' @parm interval A number between 0 and 100 indicating the percentiles of the credible intervals to be plotted and reported. Defaults to 95%
#' @param dataprep Logical. Default is `TRUE`, which calls \code{dataprep_fig3} to prepare the data. 
#' Having run \code{generage_fig3} once with \code{dataprep = TRUE}, the figure can be redrawn quickly with \code{dataprep = FALSE}, e.g. using different values for `interval`.
#'
#' @keywords trends, species, distribution, occupancy
#' @references Outhwaite et al (in prep) Complexity of biodiversity change revealed through long-term trends of invertebrates, bryophytes and lichens.
#' @references Outhwaite, C. L., Powney, G. D., August, T. A., Chandler, R. E., Rorke, S., Pescott, O., … Isaac, N. J. B. (2019). Annual estimates of
#'  occupancy for bryophytes, lichens and invertebrates in the UK (1970-2015).
#'  NERC Environmental Information Data Centre. https://doi.org/10.5285/0ec7e549-57d4-4e2d-b2d3-2199e1578d84
#' @examples
#' \dontrun{
#'
#' # Run generate_fig3 function to produce quantile plots
#' # datadir should be the filepath of where the posterior indicator values are saved.
#' # generate_fig2(postdir = paste0(getwd(), "/MajorGroups/geomeans"),
#' save_plot = TRUE,
#' interval = 95)
#'
#' }
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom stats quantile
#' @importFrom utils read.csv

generate_fig3 <- function(postdir, save_plot = TRUE, interval=95, dataprep = TRUE){

qdir <- paste0(postdir, "/quantiles")
  
if(dataprep == TRUE) {
  dataprep_fig3(postdir=postdir)
} else 
  if(!dir.exists(qdir)) {
    print("Quantile folder not found - preparing data")
    dataprep_fig3(postdir=postdir)
}

# convert inverval (a number between 0 and 100) into quantiles
if(interval > 100 | interval < 0) stop("Interval must be between 0 and 100") 
q <- 0.5 + (c(-1,1)*interval/200)  


#### read in the posterior distribution for the quantiles 
files <- list.files(qdir, pattern = "_quantiles_posterior_vals")

for(file in files){
  # extract the name
  group <- sub("_quantiles_posterior_vals.csv", "", file)
  
  # read in the posterior geometric means
  all_quants <- read.csv(file = file.path(qdir, file), as.is=TRUE)

  # All_quants contains a timeseries for each of 1000 iterations for each of 3 quantiles
  # now rescale every timeseries to start at 100 in 1970
  
  # convert to long format
  all_quants <- melt(all_quants, id= c("iter", "year"))
  
#system.time({
  #all_quants_temp <- acast(all_quants, iter~variable~year)
  #dim(all_quants_temp) # 1000 3 46
  
  # rescale every timeseries, i.e. every iter:variable combination
  #all_quants_rescaled <- apply(all_quants_temp, c(1,2), rescale)
  #dim(all_quants_rescaled) # 46 1000 3
  # it's the right shape, but has it done what we wanted?
  #all_quants_rescaled[,1,1] # yes, this is what I expect
  #all_quants_rescaled[,2,1] # yes, this is what I expect
  #all_quants_rescaled[,1,2] # yes, this is what I expect
  
  #all_quants_rescaled <- melt(all_quants_rescaled)
  #names(all_quants_rescaled)[1:3] <- c('year','iter', 'variable')
#}) # <0.1 seconds for the Freshwater
  
  # as above, with pipes
#system.time({
  all_quants_rescaled <- all_quants %>%
    group_by(iter, variable) %>%
    mutate(value = value*100/first(value)) %>% # equivalent of rescale function
    ungroup()
#}) # 0.03 seconds for Freshwater

  # check we get what we want
  #subset(all_quants_rescaled, variable=='mid' & iter==1) # looks ok
  #subset(all_quants_rescaled, variable=='lower' & iter==901) # looks ok
  

# calculate mean and 95% CIs for each quantile
quants_rescaled <- all_quants_rescaled %>% 
                      group_by(variable, year) %>%
                      summarise(mean = mean(value),
                                lowerCI = quantile(value, q[1]),
                                upperCI = quantile(value, q[2])) %>%
                      ungroup()

# now coverce into the correct format
names(quants_rescaled)[1] <- "quartile"
quants_rescaled <- melt(quants_rescaled, id=1:2)
quants_rescaled <- dcast(quants_rescaled, year ~ variable + quartile, value=value)
  
# Save the rescaled indicator values
# include the interval value on the output file
write.csv(quants_rescaled, file = paste0(qdir, "/", group, "_rescaled_quantile_vals.csv"), row.names = FALSE)
}


# Read in and organise all rescaled indicator files

# list the files of rescaled indicator values.  One per group.
files <- list.files(qdir, pattern = "_rescaled_quantile_vals")

# combine into one matrix
# somewhere to save the info
all_plot_data <- NULL

for(file in files){
  # read in first group plot data
  plot_data <- read.csv(paste0(qdir, "/", file))

  # add a group name column
  plot_data$group <- sub("_re.*", "", file)

  # combine into one data table
  all_plot_data <- rbind(all_plot_data, plot_data)
}

# change column names
#colnames(all_plot_data) <- c("year", "mean_0.25", "UCI_0.25", "LCI_0.25", "mean_0.5", "UCI_0.5", "LCI_0.5","mean_0.75", "UCI_0.75", "LCI_0.75", "group")
names(all_plot_data) <- gsub(names(all_plot_data), pa="upperCI", repl="UCI")
names(all_plot_data) <- gsub(names(all_plot_data), pa="lowerCI", repl="LCI")
names(all_plot_data) <- gsub(names(all_plot_data), pa="_lowerQ", repl="_0.25")
names(all_plot_data) <- gsub(names(all_plot_data), pa="_upperQ", repl="_0.75")
names(all_plot_data) <- gsub(names(all_plot_data), pa="_mid", repl="_0.5")

# change name labels
all_plot_data$group <- sub("FRESHWATER_SPECIES", "Freshwater species", all_plot_data$group)
all_plot_data$group <- sub("LOWER_PLANTS", "Bryophytes & lichens", all_plot_data$group)
all_plot_data$group <- sub("TERRESTRIAL_INSECTS", "Insects", all_plot_data$group)
all_plot_data$group <- sub("TERRESTRIAL_NONINSECT_INVERTS", "Inverts", all_plot_data$group)

# change order of the lines
all_plot_data$group <- as.factor(all_plot_data$group)
all_plot_data$group <- factor(all_plot_data$group, levels(all_plot_data$group)[c(2,3,4,1)])


p1 <- ggplot() +
  geom_line(data = all_plot_data, aes(x = year, y = mean_0.25), colour = c("#7D26CD"), size = 0.4) +
  geom_ribbon(data = all_plot_data, aes_string(x = 'year', ymin = 'LCI_0.25', ymax = 'UCI_0.25', linetype = NA),
              alpha = 0.4, fill = c("#7D26CD")) +
  geom_line(data = all_plot_data, aes(x = year, y = mean_0.5), colour = c("#EE7600"), size = 0.4) +
  geom_ribbon(data = all_plot_data, aes_string(x = 'year', ymin = 'LCI_0.5', ymax = 'UCI_0.5', linetype = NA),
              alpha = 0.4, fill = c("#EE7600")) +
  geom_line(data = all_plot_data, aes(x = year, y = mean_0.75), colour = c("#008B8B"), size = 0.4) +
  geom_ribbon(data = all_plot_data, aes_string(x = 'year', ymin = 'LCI_0.75', ymax = 'UCI_0.75', linetype = NA),
              alpha = 0.4, fill = c("#008B8B")) +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.2) +
  scale_y_continuous(limits = c(20, 160)) +
  scale_x_continuous(limits = c(1970, 2015)) +
  xlab("Year") +
  ylab("Scaled Occupancy") +
  facet_wrap(facets = ~ group, nrow = 2, ncol = 2) +
  theme_bw() +
  ggtitle(paste0("Quantile plot with ", interval, "% CI")) +
  theme(aspect.ratio = 1, strip.text = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(size = 8),
        panel.border = element_rect(size = 0.2),
        axis.ticks = element_line(size = 0.2),
        strip.background = element_rect(size = 0.2))

if(save_plot == TRUE){
# save the plot as a pdf
ggsave(filename = paste0(qdir, "/Figure3_CI",interval,".pdf"), plot = p1, height = 6, width = 6)
}

return(p1)
}