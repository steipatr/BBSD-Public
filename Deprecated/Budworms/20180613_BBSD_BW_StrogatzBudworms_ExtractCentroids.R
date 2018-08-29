#adjust working directory
getwd()
setwd("./GitHub/BBSD")
getwd()

#load functions
source("20180613_BBSD_R_Functions.R")

#load libraries
library("feather")
library("Cairo")
library("caret")
library("reshape2")

#load data
file_location <- "C:/Users/Patrick/Documents/GitHub/BBSD/Budworms/Data/"
data_date <- "2018-06-09"
outcomes_name <- "x"
experiments_name <- "Experiments"

path_outcomes <- paste(file_location, data_date,outcomes_name,".feather", sep="")
path_experiments <- paste(file_location, data_date,experiments_name,".feather", sep="")

df_outcomes <- read_feather(path_outcomes)

#build df of silhouette values for different cluster counts
#("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD","LCM")
#clmethods <- c("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK")
clmethods <- c("DWT","CID")

df_cl <- get.clusters.df(df_outcomes,clmethods,3)
head(df_cl)

df_cl_m <- match.clusters(df_cl,"DWT",logging=TRUE)
head(df_cl_m)

df_DWTcentr <- get.centroids(df_outcomes,df_cl$"DWT")
head(df_DWTcentr)

df_CIDcentr <- get.centroids(df_outcomes,df_cl$"CID")
head(df_CIDcentr)



for (mthd in colnames(df_cl_m)){
  this_plot <- plot.timeseries.clustered(df_outcomes,df_cl_m[,mthd],mthd)
  loc <- paste(getwd(),"/Budworms/Plots/2018-06-13/SeedComparison/",Sys.Date(),"-",mthd,".png",sep="")
  png(loc,height=6,width=12,units="cm",res=300,type="cairo")
  print(this_plot)
  dev.off()
  #print(paste(mthd,"done."))
}
