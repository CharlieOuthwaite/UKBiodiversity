##%######################################################%##
#                                                          #
####   COmbining species level posteriors for groups    ####
#                                                          #
##%######################################################%##

# level: can be "group" or "major group"


datadir <- "C:/Users/charl/Dropbox/PhD WORK/1. BIG PAPER/Repository downloads"

# load the major groups list
#data(major_groups)
major_groups <- read.csv("C:/Users/charl/Dropbox/PhD WORK/1. BIG PAPER/UKBiodiversity/Data/Major_groups.csv")


# remove brackets from list and outputs
major_groups$Species <- sub("\\(", "", major_groups$Species)
major_groups$Species <- sub("\\)", "", major_groups$Species)

allfiles <- list.files(paste0(datadir, "/POSTERIOR_SAMPLES/"))

allfiles2 <- sub("\\(", "", allfiles)
allfiles2 <- sub("\\)", "", allfiles2)


## loop through each species list and make an indicator



for(group in unique(major_groups$Major_group)){

  species_list <- as.character(major_groups[major_groups$Major_group == group, "Species"])

  temp_post <- NULL


  for(i in 1:length(species_list)){


    sp <- allfiles[grep(species_list[i], allfiles2)]

    sp_post <- read.csv(paste0(datadir, "/POSTERIOR_SAMPLES/", sp))



    # just GB or UK

    if(sum(grep("UK", sp_post$Region)) > 0) {


      sp_post <- sp_post[sp_post$Region == "UK", ]

      sp_post <- sp_post[, c(5:50, 2, 4)]

      colnames(sp_post) <- c(1970:2015, "spp", "iter")


    }else{

      sp_post <- sp_post[sp_post$Region == "GB", ]

      sp_post <- sp_post[, c(5:50, 2, 4)]

      colnames(sp_post) <- c(1970:2015, "spp", "iter")


    }





    # add into temp_post table
    temp_post <- rbind(temp_post, sp_post)

  } # end of loop through species

