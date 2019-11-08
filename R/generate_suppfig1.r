#' Generate Supplementary Figure 1
#'
#' This takes the posterior values of the group level indicators generated within the
#' \code{generage_fig1} function, calculates the occupancy estimates and plots plots of
#' occupancy over time for each taxonomic grouping.
#'
#' @param postdir A filepath specifying where the posterior combinations are saved.
#' @param save_plot Logical.  If `TRUE`, the plot will be saved as a PDF file
#' within the `postdir`. Default is `TRUE`.
#' @param interval A number between 0 and 100 indicating the percentiles of the credible intervals to be plotted and reported.
#' Defaults to 95\%
#'
#' @keywords trends, species, distribution, occupancy
#' @references Outhwaite et al (in prep) Complexity of biodiversity change revealed through long-term trends of invertebrates, bryophytes and lichens.
#' @references Outhwaite, C. L., Powney, G. D., August, T. A., Chandler, R. E., Rorke, S., Pescott, O., … Isaac, N. J. B. (2019). Annual estimates of
#'  occupancy for bryophytes, lichens and invertebrates in the UK (1970-2015).
#'  NERC Environmental Information Data Centre. https://doi.org/10.5285/0ec7e549-57d4-4e2d-b2d3-2199e1578d84
#' @examples
#' \dontrun{
#'
#' # Run generate_suppfig1 function
#' # postdir should be the filepath of where the taxa level posteriors
#' # combinations are saved.
#' generate_suppfig1(postdir = paste0(getwd(), "/Taxa"))
#'
#' }
#' @export
#' @import ggplot2
#' @import gridExtra
#' @import grid

generate_suppfig1 <- function(postdir, save_plot = TRUE, interval=95){

# convert inverval (a number between 0 and 100) into quantiles
if(interval > 100 | interval < 0) stop("Interval must be between 0 and 100")
q <- 0.5 + (c(-1,1)*interval/200)

# where to save the outputs
outdir <- paste0(postdir, "/geomeans")
if(!dir.exists(outdir)) dir.create(outdir) else print("Warning: overwriting existing files")

# list the group level files
files <- list.files(paste0(postdir,"/geomeans/"), pattern = "rescaled_indicator_vals")



# file <- files[1]



plot_list = list()

for(i in 1:length(files)){


  plot.data <- read.csv(paste0(postdir, "/geomeans/", files[i]))

  group <- sub("_rescaled_indicator_vals.csv", "", files[i])

    p <- ggplot(plot.data, aes_string(x = "year", y = "avg_occ")) +
    theme_bw() +
    geom_ribbon(data = plot.data,
                aes_string(ymin = "lower_CI", ymax = "upper_CI", linetype = NA),
                alpha = 0.2, fill = "blue") +
    geom_line(size = 0.5, col = "blue") +
    geom_hline(yintercept = 100, lty = "dashed") +
    ylab("Index of occupancy (1970 = 100)") +
    xlab("Year") +
    scale_y_continuous(limits = c(0, 300)) +
    scale_x_continuous(limits = c(1970, 2015)) +
    ggtitle(group) +
    theme(plot.title = element_text(size = 10), text = element_text(size = 10),
          aspect.ratio = 1)

    plot_list[[i]] = p

}


pdf(file = paste0(outdir, "/Supp_Fig1.pdf"), paper = "a4", height=11.69, width=8.27)



grid.arrange(plot_list[[1]],
             plot_list[[2]],
             plot_list[[3]],
             plot_list[[4]],
             plot_list[[5]],
             plot_list[[6]],ncol = 2,
             plot_list[[7]],
             plot_list[[8]],
             top = textGrob("Supplementary Figure 1: \nAverage occupancy over time Values are scaled to 100 in 1970.\nColoured lines show the average response as the geometric mean occupancy \nand the shaded area represents the 95% credible intervals\n of the posterior distribution of the geometric mean",gp=gpar(fontsize=10,font=1)))


grid.arrange(plot_list[[9]],
             plot_list[[10]],
             plot_list[[11]],
             plot_list[[12]],
             plot_list[[13]],
             plot_list[[14]],
             plot_list[[15]],
             plot_list[[16]],ncol = 2)

grid.arrange(plot_list[[17]],
             plot_list[[18]],
             plot_list[[19]],
             plot_list[[20]],
             plot_list[[21]],
             plot_list[[22]],
             plot_list[[23]],
             plot_list[[24]],ncol = 2)

grid.arrange(plot_list[[25]],
             plot_list[[26]],
             plot_list[[27]],
             plot_list[[28]],
             plot_list[[26]],
             plot_list[[30]],
             plot_list[[31]], ncol = 2)


dev.off()



}





