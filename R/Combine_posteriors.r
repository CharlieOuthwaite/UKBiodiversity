#' Combining species level posteriors for group level analyses
#'
#' This takes the individual posterior sample files for each species from a directory
#' where these have been downloaded from the associated EIDC reposotory and combines
#' them for the group level analyses presented within the associated paper.
#'
#' @param group_level A character string of either \code{"taxa"} or \code{"major_group"} depending on analysis level of interest.
#' Selecting \code{"major_group"} will also produce a combined posterior file for all species so that change across all species can be calculated later.
#' @param datadir A filepath specifying where the repository downloads are saved.  POSTERIOR_SAMPLES subfolder must be within this directory.
#' @param outdir A filepath specifying where the combined posterior outputs are to be saved.
#' @param status Logical. If \code{TRUE} then the status of the function will be printed to the console.
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

combine_posteriors <- function(group_level, datadir, outdir, status = TRUE){

  # check for correct group level specification
  if(!group_level == "taxa" & !group_level == "major_group") stop("group_level must be taxa or major_group")


# load the major groups list
major_groups <- as.data.frame(major_groups)

# Remove Rove beetles from the major_groups table.
# These are not analyses here since data starts in 1980 rather than 1970.
major_groups <- major_groups[!major_groups$Group == "RoveBeetles", ]

# remove brackets from list and outputs
major_groups$Species <- sub("\\(", "", major_groups$Species)
major_groups$Species <- sub("\\)", "", major_groups$Species)
major_groups$Species <- gsub("\\?", "", major_groups$Species)

#list all the file names of the species level posterior samples
allfiles <- list.files(datadir)

# check that files available in the datadir
if(!length(allfiles) == 5293) stop("Repository POSTERIOR_SAMPLES folder not in datadir")

# create a copy but remove brackets from file names as it causes trouble.
allfiles2 <- sub("\\(", "", allfiles)
allfiles2 <- sub("\\)", "", allfiles2)
allfiles2 <- gsub("\\?", "", allfiles2)


# great an output folder
if(group_level == "major_group"){

  groups <- unique(major_groups$Major_group)

  if(!dir.exists(paste0(outdir, "/MajorGroups"))) dir.create(paste0(outdir, "/MajorGroups"))
  outdir <- paste0(outdir, "/MajorGroups")
}


if(group_level == "taxa"){

  groups <- unique(major_groups$Group)

  if(!dir.exists(paste0(outdir, "/Taxa"))) dir.create(paste0(outdir, "/Taxa"))
  outdir <- paste0(outdir, "/Taxa")
}

# loop through each group, combining outputs into one matrix
for(group in groups){

  # get the list of species in the group

  if(group_level == "taxa"){

    species_list <- as.character(major_groups[major_groups$Group == group, "Species"])

  }


  if(group_level == "major_group"){

    species_list <- as.character(major_groups[major_groups$Major_group == group, "Species"])

  }

  # somewhere to save the combined posteriors
  group_post <- NULL

  # loop through each species, get the posterior values and combine into one matrix
  for(i in 1:length(species_list)){

    # print progress if status = T specified
    if(status == TRUE) print(paste(i, "of", length(species_list)))

    # get the species specific filename
    sp <- allfiles[grep(paste0(species_list[i], ".csv"), allfiles2, fixed = TRUE)]

    # read in the species csv posterior samples file
    sp_post <- read.csv(paste0(datadir, "/", sp))

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

} # end of loop through groups


if(group_level == "major_group"){

  ### combine posterior sets of all groups to get an "ALL" set for all species ###
  # this is used later to estimate overall change in average occupancy.
  files <- list.files(outdir, pattern = "_posterior_samples_national.rdata")

  # incase being rerun, remove "ALL" combination if in directory
  files <- files[!grepl("ALL", files)]

  if(length(files) != 4) stop("There are more than required files in the directory.")

  # where to save
  all_post <- NULL

  # loop through each file and combine
  for(file in files){

    # read in file
    load(paste0(outdir, "/", file))

    # combine
    all_post <- rbind(all_post, group_post)

  }

  group_post <- all_post

  # save as rdata file
  save(group_post, file = paste0(outdir, "/ALL_posterior_samples_national.rdata"))
}

}
