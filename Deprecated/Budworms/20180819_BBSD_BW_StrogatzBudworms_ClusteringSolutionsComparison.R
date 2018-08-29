#adjust working directory
getwd()
setwd("./GitHub/BBSD")
getwd()

#load functions
source("20180726_BBSD_R_Functions.R")

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
cl_methods <- c("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD")

df_cl2 <- get.clusters.df(df_outcomes,cl_methods,2)
#head(df_cl2,30)
df_cl3 <- get.clusters.df(df_outcomes,cl_methods,3)
#head(df_cl3,30)
df_cl4 <- get.clusters.df(df_outcomes,cl_methods,4)
#head(df_cl4,30)

#run HMM separately because of high risk of errors
df_hmm2 <- get.clusters.df(df_outcomes,("LCM"),2)
df_hmm3 <- get.clusters.df(df_outcomes,("LCM"),3)
df_hmm4 <- get.clusters.df(df_outcomes,("LCM"),4)

#assemble dataframes for sorting
df_cl2c <- merge(df_cl2, df_hmm2,by='row.names')
df_cl2c$Row.names <- NULL
df_cl3c <- merge(df_cl3, df_hmm3,by='row.names')
df_cl3c$Row.names <- NULL
df_cl4c <- merge(df_cl4, df_hmm4,by='row.names')
df_cl4c$Row.names <- NULL

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
df_cl3c$True <- df_expts$cluster
head(df_cl3c)

#match cluster indices on true (for k=3) and SBD (for k=2 or 4)
df_cl2t <- match.clusters(df_cl2c,"SBD",logging=TRUE)
df_cl3t <- match.clusters(df_cl3c,"True",logging=TRUE)
df_cl4t <- match.clusters(df_cl4c,"SBD",logging=TRUE)

#plot every clustering solution

for (k in 2:4){
  if (k==2){df_clusters_matched <- df_cl2t}
  if (k==3){df_clusters_matched <- df_cl3t}
  if (k==4){df_clusters_matched <- df_cl4t}
  
  for (mthd in colnames(df_clusters_matched)){
    this_plot <- plot.timeseries.clustered(df_outcomes,df_clusters_matched[,mthd],mthd)
    loc <- paste(getwd(),"/Report/Final_Report/Figures/Budworms/ClusteringSolutions/",k,"/Final/",Sys.Date(),"-",mthd,k,"-Matched.png",sep="")
    ggsave(plot=this_plot,loc,h=3,w=5,type="cairo-png")
    print(paste(mthd,"done."))
  }
}
