% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_suppfig1.r
\name{generate_suppfig1}
\alias{generate_suppfig1}
\title{Generate Supplementary Figure 1}
\usage{
generate_suppfig1(postdir, save_plot = TRUE, interval = 95)
}
\arguments{
\item{postdir}{A filepath specifying where the posterior combinations are saved.}

\item{save_plot}{Logical.  If `TRUE`, the plot will be saved as a PDF file
within the `postdir`. Default is `TRUE`.}

\item{interval}{A number between 0 and 100 indicating the percentiles of the credible intervals to be plotted and reported.
Defaults to 95\%}
}
\description{
This takes the posterior values of the group level indicators generated within the
\code{generage_fig1} function, calculates the occupancy estimates and plots plots of
occupancy over time for each taxonomic grouping.
}
\examples{
\dontrun{

# Run generate_suppfig1 function
# postdir should be the filepath of where the taxa level posteriors
# combinations are saved.
generate_suppfig1(postdir = paste0(getwd(), "/Taxa"))

}
}
\references{
Outhwaite et al (in prep) Complexity of biodiversity change revealed through long-term trends of invertebrates, bryophytes and lichens.

Outhwaite, C. L., Powney, G. D., August, T. A., Chandler, R. E., Rorke, S., Pescott, O., … Isaac, N. J. B. (2019). Annual estimates of
 occupancy for bryophytes, lichens and invertebrates in the UK (1970-2015).
 NERC Environmental Information Data Centre. https://doi.org/10.5285/0ec7e549-57d4-4e2d-b2d3-2199e1578d84
}
\keyword{distribution,}
\keyword{occupancy}
\keyword{species,}
\keyword{trends,}
