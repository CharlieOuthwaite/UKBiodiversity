#' Dataprep Figure 3
#'
#' This takes the posterior values of the group level indicators generated within the
#' \code{generage_fig1} function, calculates the quantiles and plots the group level quantiles presented in figure 3.
#' \code{dataprep_fig3} is called within  \code{generage_fig3} by setting \code{dataprep = TRUE}.
#'
#' @param postdir A filepath specifying where the posteior samples that are generated
#' from the combine_posteriors function are saved. are saved. If outputs have not been
#' moved, this will be in a directory "/MajorGroups".
#' 
#' @examples
#' \dontrun{
#'
#' # Run generate_fig3 function to produce quantile plots
#' # datadir should be the filepath of where the posterior indicator values are saved.
#' # generate_fig3(postdir = paste0(getwd(), "/MajorGroups/geomeans"))
#'
#' }
#' @import tidyr
#' @import dplyr
#' @import reshape2 
#' @importFrom stats quantile
#' @importFrom utils read.csv
#' @importFrom utils write.csv
#' @export

dataprep_fig3 <- function(postdir){
  
  # where to save the outputs
  outdir <- paste0(postdir, "/quantiles")
  
  # check whether there is already a directory of quantiles 
  if(!dir.exists(outdir)) dir.create(outdir) else print("Warning: overwriting existing quantile files")
  
  # list the raw data files
  files <- list.files(postdir, pattern = ".rdata")
  
  # remove the "ALL" species file
  files <- files[!grepl("ALL", files)]
 
  for(file in files){
    # extract the group name
    group <- sub(patt="_posterior_samples_national.rdata", "", file)
    print(group)
    
    # load in the combined posterior generated from the combine_posteriors function
    load(file.path(postdir, file))
    
    # convert 0 and 1 to 0.0001 and 0.9999 - solve the issue with logging zero and 1
    # NJBI I think this is redundant since we're calculating quantiles on untransformed data
    temp_post <- group_post#[,1:(ncol(group_post)-2)]
    #temp_post[temp_post < 0.0001] <- 0.0001
    #temp_post[temp_post > 0.9999] <- 0.9999
    #temp_post <- cbind(temp_post, group_post[,c("spp","iter")])
    
    # add the number of species
    n_sp <- length(unique(temp_post$spp))
    
    # calculate the quantiles of occupancy for each year:iteration combination
    j_post <- melt(temp_post, id = c("spp", "iter"))
    all_quants <- j_post %>% 
      group_by(iter, variable) %>%
      summarise(mid = median(value, na.rm=T), # there are NAs in the Rove beetles
            lowerQ = quantile(value, 0.25, na.rm=T),
            upperQ = quantile(value, 0.75, na.rm=T)) %>%
      ungroup()

    # format the years appropriately
    names(all_quants)[2] <- 'year'
    all_quants$year <- as.numeric(as.character(all_quants$year))
    
    # save the posterior geometric means
  write.csv(all_quants, file = paste0(outdir, "/", group, "_quantiles_posterior_vals.csv"), row.names = FALSE)
  }
}