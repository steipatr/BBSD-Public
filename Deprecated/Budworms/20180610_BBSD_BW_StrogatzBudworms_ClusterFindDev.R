#adjust working directory
getwd()
setwd("./GitHub/BBSD")
getwd()

#load functions
source("20180610_BBSD_R_Functions.R")

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

#create list of desired clustering methods
#choose from:
#("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD","LCM")
clmethods <- c("CID","SBD")

#compare options k using silhouette
df_silhouettes <- compare.silhouettes(df_outcomes,clmethods,kmin=2,kmax=7,logging=TRUE)
head(df_silhouettes)
