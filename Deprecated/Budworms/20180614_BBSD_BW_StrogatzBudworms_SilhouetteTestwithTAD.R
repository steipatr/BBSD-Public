#adjust working directory
getwd()
setwd("./GitHub/BBSD")
getwd()

#load functions
source("20180614_BBSD_R_Functions.R")

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
clmethods <- c("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","TAD","LCM")

df_sil <- compare.silhouettes(df_outcomes,clmethods,2,5,logging=TRUE)
print(df_sil)

plt <- plot.silhouettes(df_sil)
plot(plt)

this <- c("DTW","DTW")
this2 <- c("SBD",rep("DTW",2))
this
this2
