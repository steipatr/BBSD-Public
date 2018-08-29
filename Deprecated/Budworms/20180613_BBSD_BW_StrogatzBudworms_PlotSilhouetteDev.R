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
clmethods <- c(rep("CID",3))

df_sils <- compare.silhouettes(df_outcomes,clmethods,2,6,logging=TRUE)
head(df_sils)


#melt into long for plotting
df_sils_long <- melt(df_sils,id.vars = c("k"),variable.name="method")

ggplot(df_sils_long, aes(x = k, y = value,color=method, group = method)) + geom_point() + geom_line()

