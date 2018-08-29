#adjust working directory
getwd()
setwd("./GitHub/BBSD")
getwd()

#load functions
source("20180608_BBSD_R_Functions.R")

#load libraries
library("feather")

#load data
file_location <- "C:/Users/Patrick/Documents/GitHub/BBSD/Budworms/Data/"
data_date <- "2018-05-31"
outcomes_name <- "x"
experiments_name <- "Experiments"

path_outcomes <- paste(file_location, data_date,outcomes_name,".feather", sep="")
path_experiments <- paste(file_location, data_date,experiments_name,".feather", sep="")

df_outcomes <- read_feather(path_outcomes)

#plot unclustered outcomes
tsplot <- plot.timeseries(df_outcomes)
#tsplot

#dtwclust methods
DTWcl <- get.clusters(df_outcomes,"DTW",3)
SBDcl <- get.clusters(df_outcomes,"SBD",3)
GAKcl <- get.clusters(df_outcomes,"GAK",3)
TADcl <- get.clusters(df_outcomes,"TAD",3)


#TSclust methods
CORTcl <- get.clusters(df_outcomes,"CORT",3)
ACFcl <- get.clusters(df_outcomes,"ACF",3)
PERcl <- get.clusters(df_outcomes,"PER",3)
DWTcl <- get.clusters(df_outcomes,"DWT",3)
PICcl <- get.clusters(df_outcomes,"PIC",3)

#TODO
#seqHMM method
#LCMcl <- get.clusters(df_outcomes,"LCM",3)


#combine clusters vectors into dataframe
#df_clusters <- data.frame(DTWcl,SBDcl,GAKcl,TADcl,CORTcl,ACFcl,PERcl,DWTcl,PICcl)
#colnames(df_clusters) <- c("DTW","SBD","GAK","TAD","CORT","ACF","PER","DWT","PIC")
df_clusters <- data.frame(DTWcl,SBDcl,GAKcl,TADcl,CORTcl,ACFcl,PERcl,DWTcl,PICcl)
colnames(df_clusters) <- c("DTW","SBD","GAK","TAD","CORT","ACF","PER","DWT","PIC")
print(df_clusters)


#get correct clusters from experiment values
df_expts <- read_feather(path_experiments)
df_work <- df_expts

df_work$cluster<-NA
df_work$cluster[df_work$'r max'<0.556]<-3
df_work$cluster[df_work$'r max'>0.556 & df_work$'r max' - df_work$'drift'>0.384]<-1
df_work$cluster[df_work$'r max'>0.556 & df_work$'r max' - df_work$'drift'<0.384]<-2

df_clusters$truecl <- df_work$cluster #add true clusters to clusters df

head(df_clusters)
