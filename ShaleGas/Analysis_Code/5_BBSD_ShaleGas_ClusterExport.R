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
outcome <- "Oilprice"
experiments <- "Experiments"

path_outcomes <- paste(wd,"/Data/", data_date,outcome,".feather", sep="")
path_experiments <- paste(wd,"/Data/", data_date,experiments,".feather", sep="")

df_expts <- read_feather(path_experiments)
df_outcomes <- read_feather(path_outcomes)

#get clustering solutions for a given k value 
#choose from ("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD","LCM")
cl_methods <- c("CID")

df_cl6 <- get.clusters.df(df_outcomes,cl_methods,6)

#export clustering solutions for rule induction in Python
clustersolutions_name <- "CID"
path_clusters <- paste(wd,"/Data/", data_date,clustersolutions_name,".feather", sep="")
write_feather(df_cl6,path_clusters)
