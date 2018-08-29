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
cl_methods <- c("LCM","ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD")

#run LCM first because of high chance of failing to converge
df_cl2 <- get.clusters.df(df_outcomes,cl_methods,2)
head(df_cl2)

#add true column from input data
df_expts <- read_feather(path_experiments)

head(df_expts)

#create true cluster column
df_expts$cl<-NA
df_expts$cl[df_expts$'b0' < (1+(df_expts$'a')^2)]<-1 #fixed point
df_expts$cl[df_expts$'b0' > (1+(df_expts$'a')^2)]<-2 #Hopf bifurcation/oscillating

#add column to df_cl3c
df_cl2$True <- df_expts$cl
head(df_cl2)

#match cluster indices on True
df_cl2m <- match.clusters(df_cl2,"True",logging=TRUE)
head(df_cl2m)


tile <- plot.confusionmatrix(df_cl2m[,"True"],df_cl2m[,"SBD"],external=TRUE)
tile

#plot confusion matrix for every combo of actual and clustering methods
for (mthd in colnames(df_cl2m)){
  plt <- plot.confusionmatrix(df_cl2m[,"True"],df_cl2m[,mthd],external=TRUE)
  #this_plot <- this_plot + ggtitle(paste("True","vs.",mthd))
  loc <- paste(getwd(),"/Report/Report/Figures/Brusselator/ConfusionMatrices/",Sys.Date(),"-True_",mthd,".png",sep="")
  ggsave(plot = plt, loc, h = 2, w = 3, type = "cairo-png")
  print(paste(mthd,"done."))
}
