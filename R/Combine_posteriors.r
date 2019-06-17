#' Combining species level posteriors for group level analyses
#'
#' This takes the individual posterior sample files for each species from a directory
#' where these have been downloaded from the associated EIDC reposotory entry and combines
#' them for the group level analyses presented within the associated paper.
#'
#' @param group_level A character string of either \code{"taxa"} or \code{"major_group"} depending on analysis level of interest.
#' @param datadir A filepath specifying where the repository downloads are saved.  POSTERIOR_SAMPLES subfolder must be within this directory.
#' @param outdir A filepath specifying where the combined posterior outputs are to be saved.
#' @param status Logical. If \code{TRUE} then the status of the function will be printed to the consolde.
#' This specifies where in each group the function is up to in number of species out of the total number of species.
#' e.g. 1 of 29 for the first ant species. Default is \code{TRUE}
#'
#' @keywords trends, species, distribution, occupancy,
#' @references Outhwaite et al (in prep) Complexity of biodiversity change revealed through long-term trends of invertebrates, bryophytes and lichens.
#' @references Outhwaite, C. L., Powney, G. D., August, T. A., Chandler, R. E., Rorke, S., Pescott, O., … Isaac, N. J. B. (2019). Annual estimates of
#'  occupancy for bryophytes, lichens and invertebrates in the UK (1970-2015).
#'  NERC Environmental Information Data Centre. https://doi.org/10.5285/0ec7e549-57d4-4e2d-b2d3-2199e1578d84
#' @examples
#' \dontrun{
#'
#' # Run combine_posteriors function for taxa level analyses.
#' combine_posteriors(group_level = "taxa",
#'datadir = paste0(getwd(), "/Repository downloads"),
#'outdir = paste0(getwd(), "/Outputs"),
#'status = TRUE)
#'
#'#' # Run combine_posteriors function for major group level analyses.
#' combine_posteriors(group_level = "major_group",
#'datadir = paste0(getwd(), "/Repository downloads"),
#'outdir = paste0(getwd(), "/Outputs"),
#'status = TRUE)
#'
#' }
#' @export
#' @importFrom
#' @importFrom


combine_posteriors <- function(group_level, datadir, outdir, status = TRUE){

  # check for correct group level specification
  if(!group_level == "taxa" | !group_level == "taxa") stop("group_level must be taxa or major_group")

#datadir <- "C:/Users/charl/Dropbox/PhD WORK/1. BIG PAPER/Repository downloads"

# specify outdir in function
#outdir <- "C:/Users/charl/Dropbox/PhD WORK/1. BIG PAPER/Package_testing"

# load the major groups list
data(major_groups)
#major_groups <- read.csv("C:/Users/charl/Dropbox/PhD WORK/1. BIG PAPER/UKBiodiversity/Data/Major_groups.csv")


# remove brackets from list and outputs
major_groups$Species <- sub("\\(", "", major_groups$Species)
major_groups$Species <- sub("\\)", "", major_groups$Species)
major_groups$Species <- gsub("\\?", "", major_groups$Species)

#list all the file names of the species level posterior samples
allfiles <- list.files(paste0(datadir, "/POSTERIOR_SAMPLES/"))

# check that files available in the datadir
if(length(allfiles) == 0) stop("Repository POSTERIOR_SAMPLES folder not in datadir")

# create a copy but remove brackets from file names as it causes trouble.
allfiles2 <- sub("\\(", "", allfiles)
allfiles2 <- sub("\\)", "", allfiles2)
allfiles2 <- gsub("\\?", "", allfiles2)


## function to combine all the species level posteriors into group or major group level sets ##

# set group level within function to "taxa" or "major_group"
#group_level <- "major_group"

group_level <- "taxa"

if(group_level == "major_group"){

  groups <- unique(major_groups$Major_group)

  dir.create(paste0(outdir, "/MajorGroups"))
  outdir <- paste0(outdir, "/MajorGroups")
}


if(group_level == "taxa"){

  groups <- unique(major_groups$Group)
  dir.create(paste0(outdir, "/Taxa"))
  outdir <- paste0(outdir, "/Taxa")
}


  for(group in groups){

  if(group_level == "taxa"){

    species_list <- as.character(major_groups[major_groups$Group == group, "Species"])

  }


  if(group_level == "major_group"){

    species_list <- as.character(major_groups[major_groups$Major_group == group, "Species"])

  }

  group_post <- NULL

  # loop through each species, get the posterior values and combine into one matrix
  for(i in 1:length(species_list)){

    if(status == TRUE) print(paste(i, "of", length(species_list)))

    # get the species specific filename
    sp <- allfiles[grep(paste0(species_list[i], ".csv"), allfiles2, fixed = TRUE)]

    # read in the species csv posterior samples file
    sp_post <- read.csv(paste0(datadir, "/POSTERIOR_SAMPLES/", sp))



    # select the largest region values (GB or UK)

    if(sum(grep("UK", sp_post$Region)) > 0) {

      # subset to the required rows
      sp_post <- sp_post[sp_post$Region == "UK", ]

      # reorganise required columns
      sp_post <- sp_post[, c(5:50, 2, 4)]

      # rename columns
      colnames(sp_post) <- c(1970:2015, "spp", "iter")


    }else{

      # subset to the required rows
      sp_post <- sp_post[sp_post$Region == "GB", ]

      # reorganise required columns
      sp_post <- sp_post[, c(5:50, 2, 4)]

      # rename columns
      colnames(sp_post) <- c(1970:2015, "spp", "iter")

    }


    # add into temp_post table
    group_post <- rbind(group_post, sp_post)

  } # end of loop through species


  # save posterior combination in outdir
  save(group_post, file = paste0(outdir, "/", group, "_posterior_samples_national.rdata"))

}


}
