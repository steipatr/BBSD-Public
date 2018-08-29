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
file_location <- "C:/Users/Patrick/Documents/GitHub/BBSD/Budworms/Data/"
data_date <- "2018-06-25"
outcomes_name <- "x"
experiments_name <- "Experiments"

path_outcomes <- paste(file_location, data_date,outcomes_name,".feather", sep="")
path_experiments <- paste(file_location, data_date,experiments_name,".feather", sep="")

df_outcomes <- read_feather(path_outcomes)

#compare cluster count possibilities across all methods
#("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD","LCM")
cl_methods <- c("LCM","ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD")

#run LCM first because of high chance of failing to converge
df_cl4 <- get.clusters.df(df_outcomes,cl_methods,4)
head(df_cl4)

for (mthd in colnames(df_cl4)){
  plt <- plot.timeseries.clustered(df_outcomes,df_cl4[,mthd])
  loc <- paste(getwd(),"/Report/Report/Figures/BudwormsClusteringSolutions/4/Orig/",Sys.Date(),"-",mthd,"4-Orig.png",sep="")
  png(loc,height=6,width=12,units="cm",res=300,type="cairo")
  print(plt)
  dev.off()
  print(paste(mthd,"done."))
}

#match cluster indices on LCM)
df_cl4m <- match.clusters(df_cl4,"LCM",logging=TRUE)
head(df_cl4m)

#replot
for (mthd in colnames(df_cl4m)){
  plt <- plot.timeseries.clustered(df_outcomes,df_cl4m[,mthd])
  loc <- paste(getwd(),"/Report/Report/Figures/BudwormsClusteringSolutions/4/Matched/",Sys.Date(),"-",mthd,"4-Matched.png",sep="")
  png(loc,height=6,width=12,units="cm",res=300,type="cairo")
  print(plt)
  dev.off()
  print(paste(mthd,"done."))
}
