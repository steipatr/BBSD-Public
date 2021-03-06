#adjust working directory
getwd()
setwd("./GitHub/BBSD")
getwd()

#load functions
source("20180625_BBSD_R_Functions.R")

#load libraries
library("feather")
library("Cairo")
library("caret")

#load data
file_location <- "C:/Users/Patrick/Documents/GitHub/BBSD/Brusselator/Data/"
data_date <- "2018-07-09"
outcomes_name <- "y"
experiments_name <- "Experiments"

path_outcomes <- paste(file_location, data_date,outcomes_name,".feather", sep="")
path_experiments <- paste(file_location, data_date,experiments_name,".feather", sep="")

df_outcomes <- read_feather(path_outcomes)

#compare cluster count possibilities across all methods
#("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD","LCM")
cl_methods <- c("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD")

df_cl6 <- get.clusters.df(df_outcomes,cl_methods,6)
head(df_cl6)

for (mthd in colnames(df_cl6)){
  plt <- plot.timeseries.clustered(df_outcomes,df_cl6[,mthd])
  loc <- paste(getwd(),"/Report/Report/Figures/Brusselator/ClusteringSolutions/6/Orig/",Sys.Date(),"-",mthd,"6-Orig.png",sep="")
  ggsave(plot = plt, loc, h = 4, w = 8, type = "cairo-png")
  print(paste(mthd,"done."))
}

#match cluster indices on CID)
df_cl6m <- match.clusters(df_cl6,"CID",logging=TRUE)
head(df_cl6m)

#replot
for (mthd in colnames(df_cl6m)){
  plt <- plot.timeseries.clustered(df_outcomes,df_cl6m[,mthd])
  loc <- paste(getwd(),"/Report/Report/Figures/Brusselator/ClusteringSolutions/6/Matched/",Sys.Date(),"-",mthd,"6-Matched.png",sep="")
  ggsave(plot = plt, loc, h = 4, w = 8, type = "cairo-png")
  print(paste(mthd,"done."))
}
