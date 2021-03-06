#adjust working directory
getwd()
setwd("../")
wd <- getwd()

#load functions
source("../20180819_BBSD_R_Functions.R")

#load libraries
library("feather")

#load data
data_date <- "2018-08-29"
outcome <- "x"
experiments <- "Experiments"

path_outcomes <- paste(wd,"/Data/", data_date,outcome,".feather", sep="")
path_experiments <- paste(wd,"/Data/", data_date,experiments,".feather", sep="")

df_expts <- read_feather(path_experiments)
df_outcomes <- read_feather(path_outcomes)

#get clustering solutions for a given k value 
#choose from ("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD","LCM")
cl_methods <- c("ACF","CID","DWT")

df_cl3 <- get.clusters.df(df_outcomes,cl_methods,3)

#plot clustered time series for one method
clustered_plot <- plot.timeseries.clustered(df_outcomes,df_cl3$"ACF")
plot(clustered_plot)

#plot and save clustered time series for all methods
for (mthd in colnames(df_cl3)){
  print(paste("Plotting",mthd))
  this_plot <- plot.timeseries.clustered(df_outcomes,df_cl3[,mthd])
  this_loc  <- paste(wd,"/Figures/",Sys.Date(),"-Budworms-",mthd,".png",sep="")
  ggsave(plot = this_plot, this_loc, h = 8, w = 8, type = "cairo-png")
  print(paste(mthd,"done."))
}
