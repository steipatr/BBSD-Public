########
#adjust working directory
getwd()
setwd("./GitHub/BBSD")
getwd()

#load functions
source("20180819_BBSD_R_Functions.R")

#load libraries
library("feather")
library("Cairo")
library("caret")

#load data
file_location <- "C:/Users/Patrick/Documents/GitHub/BBSD/ShaleGas/Data/"
data_date <- "2018-07-12"
outcomes_name <- "Oilprice"
experiments_name <- "Experiments"

path_outcomes <- paste(file_location, data_date,outcomes_name,".feather", sep="")
path_experiments <- paste(file_location, "2018-08-20",experiments_name,".feather", sep="")

df_outcomes <- read_feather(path_outcomes)
df_expts <- read_feather(path_experiments)

#get cluster memberships
cl_methods <- c("CID")
k=6
df_cl2 <- get.clusters.df(df_outcomes,cl_methods,k)
head(df_cl2)

#get centroids of each cluster
centroids <- get.centroids(df_outcomes,df_cl2$CID)
centroids

#build data columns for viz
df_cl2$Centroids <- 0
df_cl2$alpha <- 0.8
df_cl2$width <- 1
i=1
for (c in centroids$centroid){
  df_cl2$Centroids[c] <- i
  df_cl2$alpha[c] <- 1
  df_cl2$width[c] <- 10
  i <- i+1
}

#plot
#reformat wide into long
expt_id <- as.character(1:nrow(df_ts))
df_ts_id <- cbind(expt_id=expt_id[0:250], df_outcomes[0:250,])
df_ts_long <- melt(df_ts_id,variable.name="time_step")

centroids_plot <- ggplot(data=df_ts_long, aes(x=time_step, y=value,width=as.factor(rep(df_cl2$width[0:250],ncol(df_ts))),alpha=as.factor(rep(df_cl2$alpha[0:250],ncol(df_ts))),colour=as.factor(rep(df_cl2$Centroids[0:250],ncol(df_ts))))) + 
  geom_line(aes(group=expt_id)) +
  theme(legend.position="none") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
  xlab("time step") + ylab("value") + 
  scale_color_manual(values=c("grey","red","blue","yellow","green","purple","pink")) +
  ylim(0,20000)

centroids_plot

loc <- paste(getwd(),"/Report/Final_Report/Figures/Separability/",Sys.Date(),"ShaleGasCentroids.png",sep="")
ggsave(plot = centroids_plot, loc, h = 5, w = 8, type = "cairo-png")
