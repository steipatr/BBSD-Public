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
path_experiments <- paste(file_location, data_date,experiments_name,".feather", sep="")

df_outcomes <- read_feather(path_outcomes)
df_red <- df_outcomes[]
dim(df_red)
head(df_red)

#compare cluster count possibilities across all methods
#("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD","LCM")
cl_methods <- c("CID")

#match on CID
k=6
df_cl <- get.clusters.df(df_red,cl_methods,k)

#export clustering solution and reduced outcomes
clustersolutions_name <- "CID"
path_clusters <- paste(file_location, data_date,clustersolutions_name,".feather", sep="")
write_feather(df_cl,path_clusters)

outcomes_name <- "OilpriceSmall"
path_clusters <- paste(file_location, data_date,outcomes_name,".feather", sep="")
write_feather(df_red,path_clusters)
