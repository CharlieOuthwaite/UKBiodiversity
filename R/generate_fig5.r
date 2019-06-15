##%######################################################%##
#                                                          #
####           Function to generate Figure 5            ####
#                                                          #
##%######################################################%##

datadir <- "C:/Users/charl/Dropbox/PhD WORK/1. BIG PAPER/Repository downloads"


# where to save results
outdir <- "C:/Users/charl/Dropbox/PhD WORK/1. BIG PAPER/Package_testing/Taxa/geomeans"

sp_trends <- read.csv(paste0(datadir, "/Species_Trends.csv"))


#### first need to calculate average occupancy for each species


# use group posteriors to calculate average occupancy

postdir <- paste0(datadir, "/POSTERIOR_SAMPLES")

post.files <- list.files(postdir)

post.files2 <- sub("\\(", "", post.files)
post.files2 <- sub("\\)", "", post.files2)
post.files2 <- gsub("\\?", "", post.files2)

# table to save results
mean.tab <- matrix(NA, ncol = 3, nrow = 5293)

mean.tab <- as.data.frame(mean.tab)

mean.tab[, 1:2] <- sp_trends[, 1:2]

colnames(mean.tab) <- c("Group", "Species", "avg.occ")

sp_trends$Species <- sub("\\(", "", sp_trends$Species)
sp_trends$Species <- sub("\\)", "", sp_trends$Species)
sp_trends$Species <- gsub("\\?", "", sp_trends$Species)

sp_trends$Species <- gsub("Pardosa saltanslugubris", "Pardosa saltans_lugubris", sp_trends$Species)


# loop through each group in good sp table
for(sp in unique(sp_trends$Species)){

  post <- post.files[grep(paste0(sp, ".csv"), post.files2)]

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


    startyr <- sp_trends[sp_trends$Species == sp, 'First_Year']
    endyr <- sp_trends[sp_trends$Species == sp, 'Last_Year']


    # just years with data
    sp.post <- sp.post[, grep(startyr, colnames(sp.post)):grep(endyr, colnames(sp.post))]

    # calc the average across a set of years - make this selectable, end at 2015
    sp.means <- colMeans(sp.post)

    # overall mean across years
    overall.mean <- mean(sp.means, na.rm = T)


    mean.tab[mean.tab$Species == sp, 3] <- overall.mean


  }



# save table of average occupancies
write.csv(mean.tab, file = paste0(outdir, "/Average_occ.csv"), row.names = F)

### sort data for ggplot ###

plot.data <- mean.tab

# add on column for growth rates
plot.data$gr.rate <- sp_trends[match(plot.data$Species, sp_trends$Species), 'Mean_growth_rate']



line_data <- data.frame(x = c(0,1), y = c(0,-1), w = c(0,1), z = c(1,0))


library(viridis)

# Nick recommended plotting growth rate rather than absolute change
ggplot(data = plot.data, aes(x = avg.occ, y = gr.rate)) +
  stat_bin_hex(aes(fill = ..count..), binwidth = c(0.025, 0.5)) +
  scale_fill_viridis(limits = c(1, 50),  name = "n species", trans = "log", breaks = c(1,3,7,20)) +
  geom_hline(yintercept = 0, linetype = 'dashed', lwd = 0.5) +
  xlab("Average occupancy") +
  ylab("Growth Rate") +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.margin = unit(c(1,1,1,1), "cm"),
        aspect.ratio = 1,
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_x_sqrt(expand = c(0,0), limits=c(0, 1)) +
  scale_y_continuous(limits = c(-20, 25))


ggsave(filename = paste0(outdir, "/Figure_5.pdf"), height = 6, width = 6)


