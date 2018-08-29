#adjust working directory
getwd()
setwd("./GitHub/BBSD")
getwd()

#load functions
source("20180612_BBSD_R_Functions.R")

#load libraries
library("feather")
library("Cairo")
library("caret")

#load data
file_location <- "C:/Users/Patrick/Documents/GitHub/BBSD/Budworms/Data/"
data_date <- "2018-06-09"
outcomes_name <- "x"
experiments_name <- "Experiments"

path_outcomes <- paste(file_location, data_date,outcomes_name,".feather", sep="")
path_experiments <- paste(file_location, data_date,experiments_name,".feather", sep="")

df_outcomes <- read_feather(path_outcomes)

#
clmethods <- c("CID","LCM")

df_clusters <- get.clusters.df(df_outcomes,clmethods,3)

head(df_clusters)

df_clm <- match.clusters(df_clusters,"CID",logging=TRUE)
head(df_clm)

plt <- plot.timeseries.clustered(df_outcomes,df_clm[,"LCM"],"LCM auto")
print(plt)
