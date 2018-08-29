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
df_cl3 <- get.clusters.df(df_outcomes,cl_methods,3)
head(df_cl3)

#add true column from input data
df_expts <- read_feather(path_experiments)

#for given k value, set r_max and drift limits
r_max_cusp <- 0.559525 #for k=10
drift_cusp <- 0.383971 #for k=10

#create true cluster column
df_expts$cluster<-NA
df_expts$cluster[df_expts$'r0'<r_max_cusp]<-1 #always at refuge level
df_expts$cluster[df_expts$'r0'>r_max_cusp & df_expts$'r0' - df_expts$'rstep'<drift_cusp]<-2 #drop from outbreak to refuge
df_expts$cluster[df_expts$'r0'>r_max_cusp & df_expts$'r0' - df_expts$'rstep'>drift_cusp]<-3 #stay at outbreak

#add column to df_cl3c
df_cl3$True <- df_expts$cluster

#match cluster indices on true (for k=3))
df_cl3m <- match.clusters(df_cl3,"True",logging=TRUE)
head(df_cl3m)


tile <- plot.confusionmatrix(df_cl3m[,"True"],df_cl3m[,"SBD"],external=TRUE)
tile

#plot confusion matrix for every combo of actual and clustering methods
for (mthd in colnames(df_cl3m)){
  this_plot <- plot.confusionmatrix(df_cl3m[,"True"],df_cl3m[,mthd],external=TRUE)
  #this_plot <- this_plot + ggtitle(paste("True","vs.",mthd))
  loc <- paste(getwd(),"/Report/Report/Figures/BudwormsConfusionMatrices/",Sys.Date(),"-True_",mthd,".png",sep="")
  png(loc,height=5,width=8,units="cm",res=300,type="cairo")
  print(this_plot)
  dev.off()
  print(paste(mthd,"done."))
}
