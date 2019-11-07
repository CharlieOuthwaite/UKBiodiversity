#' Generate Figure 5
#'
#' This takes the posterior estimates for each species and the species trend estimates
#' downloaded from the repository, calculates the average occupancy across the time frame
#' of each species for whihch data were available and generates figure 5 of the
#' associated paper.
#' \code{generage_fig5} function estimates species level average occupancy and generates figure 5.
#'
#' @param postdir A filepath specifying where the species level posteriors have been saved.
#' If outputs have not been moved, this will be in a directory "/POSTERIOR_SAMPLES".
#' @param outdir A filepath specifying where the outputs of the function are to be saved.
#' @param sp_trends A dataframe, downloaded from the repository which details the species
#' level trends in occupancy and the years for which data were avaialble for each species.
#' @param status Logical. If TRUE the species name being worked on will be printed to the console.
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
#' # Run generate_fig5 function to produce plot of average occupancy against
#' # trend in occupancy.
#' # datadir should be the filepath of where the posterior samples are saved.
#' generate_fig5(postdir = "/POSTERIOR_SAMPLES", outdir = getwd(),
#' sp_trends = read.csv(paste0(datadir, "/Species_Trends.csv")))
#'
#'
#' }
#' @export
#' @import ggplot2
#' @import viridis


generate_fig5 <- function(postdir, outdir, sp_trends, status = TRUE, save_plot = TRUE){


if(nrow(sp_trends) != 5293) stop("Complete Species_Trends csv file not found, should have 5293 rows.")


# remove rove beetles from table as not covered in this analysis
sp_trends <- sp_trends[!sp_trends$Group == "RoveBeetles", ]

#### first need to calculate average occupancy for each species


# use group posteriors to calculate average occupancy

post.files <- list.files(postdir)

post.files2 <- sub("\\(", "", post.files)
post.files2 <- sub("\\)", "", post.files2)
post.files2 <- gsub("\\?", "", post.files2)

# table to save results
mean.tab <- matrix(NA, ncol = 3, nrow = 5214)

mean.tab <- as.data.frame(mean.tab)

sp_trends$Species <- gsub("Pardosa saltans/lugubris", "Pardosa saltans_lugubris", sp_trends$Species)

# add on column for major group
major_groups <- as.data.frame(major_groups)
sp_trends$major_group <- major_groups[match(sp_trends$Species, major_groups$Species), 'Major_group']


# remove dodgy characters
sp_trends$Species <- sub("\\(", "", sp_trends$Species)
sp_trends$Species <- sub("\\)", "", sp_trends$Species)
sp_trends$Species <- gsub("\\?", "", sp_trends$Species)


mean.tab[, 1:2] <- sp_trends[, 1:2]
colnames(mean.tab) <- c("Group", "Species", "avg.occ")

# loop through each group in good sp table
for(sp in unique(sp_trends$Species)){

  # i status = T, print progress
  if(status == TRUE) print(sp)

  # select the correct posterior file
  post <- post.files[grep(paste0(sp, ".csv"), post.files2)]

  # read in the file
  sp.post <- read.csv(paste0(postdir, "/", post))


  # select the largest region values (GB or UK)

  if(sum(grep("UK", sp.post$Region)) > 0) {

    # subset to the required rows
    sp.post <- sp.post[sp.post$Region == "UK", ]

    # reorganise required columns
    sp.post <- sp.post[, c(5:50, 2, 4)]

    # rename columns
    colnames(sp.post) <- c(1970:2015, "spp", "iter")


  }else{

    # subset to the required rows
    sp.post <- sp.post[sp.post$Region == "GB", ]

    # reorganise required columns
    sp.post <- sp.post[, c(5:50, 2, 4)]

    # rename columns
    colnames(sp.post) <- c(1970:2015, "spp", "iter")

  }


    # extract the first and last year for which there was raw data (see method of paper)
    startyr <- sp_trends[sp_trends$Species == sp, 'First_Year']
    endyr <- sp_trends[sp_trends$Species == sp, 'Last_Year']

    # select posterior years with data
    sp.post <- sp.post[, grep(startyr, colnames(sp.post)):grep(endyr, colnames(sp.post))]

    # calculate the average occupancy across iterations
    sp.means <- colMeans(sp.post)

    # overall mean across years
    overall.mean <- mean(sp.means, na.rm = T)

    # save result in table
    mean.tab[mean.tab$Species == sp, 3] <- overall.mean


  } # end of loop through species



# save table of average occupancies
write.csv(mean.tab, file = paste0(outdir, "/Average_occ.csv"), row.names = F)

### sort data for plot ###

plot.data <- mean.tab

# add on column for growth rates
plot.data$gr.rate <- sp_trends[match(plot.data$Species, sp_trends$Species), 'Mean_growth_rate']

# save this table for calculating coeffs
write.csv(plot.data, file = paste0(outdir, "/Average_occ_Growth_rates.csv"), row.names = F)


# add on column for major group
major_groups <- as.data.frame(major_groups)

major_groups$Species <- sub("\\(", "", major_groups$Species)
major_groups$Species <- sub("\\)", "", major_groups$Species)
major_groups$Species <- gsub("\\?", "", major_groups$Species)



plot.data$major_group <- major_groups[match(plot.data$Species, major_groups$Species), 'Major_group']
plot.data$major_group <- sp_trends[match(plot.data$Species, sp_trends$Species), 'major_group']


# change name labels
plot.data$major_group <- sub("FRESHWATER_SPECIES", "Freshwater species", plot.data$major_group)
plot.data$major_group <- sub("LOWER_PLANTS", "Bryophytes & lichens", plot.data$major_group)
plot.data$major_group <- sub("TERRESTRIAL_INSECTS", "Insects", plot.data$major_group)
plot.data$major_group <- sub("TERRESTRIAL_NONINSECT_INVERTS", "Inverts", plot.data$major_group)

# reorganise factor levels for major groups
plot.data$major_group <- factor(plot.data$major_group, levels = c("Freshwater species", "Insects", "Inverts","Bryophytes & lichens"))

# data for a central line in plot
line_data <- data.frame(x = c(0,1), y = c(0,-1), w = c(0,1), z = c(1,0))

# generate figure
p1 <- ggplot(data = plot.data, aes(x = avg.occ, y = gr.rate)) +
  stat_bin_hex(aes(fill = ..count..), binwidth = c(0.025, 0.5)) +
  scale_fill_viridis(limits = c(1, 50),  name = "n species", trans = "log", breaks = c(1,3,7,20)) +
  geom_hline(yintercept = 0, linetype = 'dashed', lwd = 0.5) +
  xlab("Average occupancy") +
  ylab("Growth Rate") +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        plot.margin = unit(c(1,1,1,1), "cm"),
        aspect.ratio = 1,
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text = element_text(size = 8),
        strip.background = element_rect(size = 0.2),
        panel.border = element_rect(size = 0.2),
        plot.title = element_text(size = 10),
        text = element_text(size = 10),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.key = element_rect(size = 0.3)) +
  scale_x_sqrt(expand = c(0,0), limits=c(0, 1)) +
  scale_y_continuous(limits = c(-20, 25)) +
  facet_wrap(facets = ~ major_group, nrow = 2, ncol = 2)

if(save_plot == TRUE){
# save the plot
ggsave(filename = paste0(outdir, "/Figure_5.pdf"), plot = p1, height = 6, width = 6)
}

return(p1)

}
