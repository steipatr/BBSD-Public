#adjust working directory
getwd()
setwd("./GitHub/BBSD")
getwd()

#load functions
source("20180608_BBSD_R_Functions.R")

#load libraries
library("feather")
library("Cairo")

#load data
file_location <- "C:/Users/Patrick/Documents/GitHub/BBSD/Budworms/Data/"
data_date <- "2018-06-09"
outcomes_name <- "x"
experiments_name <- "Experiments"

path_outcomes <- paste(file_location, data_date,outcomes_name,".feather", sep="")
path_experiments <- paste(file_location, data_date,experiments_name,".feather", sep="")

df_outcomes <- read_feather(path_outcomes)

#plot unclustered outcomes
#plot.timeseries(df_outcomes)

#create list of desired clustering methods
#choose from ("DTW","SBD","GAK","TAD","CORT","ACF","PER","DWT","PIC")
clmethods <- c("DTW","SBD","GAK","TAD","CORT","ACF","PER","DWT","PIC")

#get df of those clusters
df_clusters <- get.clusters.df(df_outcomes,clmethods,3)
#head(df_clusters)

#get correct clusters from experiment values
df_expts <- read_feather(path_experiments)
df_work <- df_expts

#for given k value, set r_max and drift limits
r_max_cusp <- 0.559525 #for k=10
drift_cusp <- 0.383971 #for k=10

df_work$cluster<-NA
df_work$cluster[df_work$'r0'<r_max_cusp]<-1 #always at refuge level
df_work$cluster[df_work$'r0'>r_max_cusp & df_work$'r0' - df_work$'rstep'<drift_cusp]<-2 #drop from outbreak to refuge
df_work$cluster[df_work$'r0'>r_max_cusp & df_work$'r0' - df_work$'rstep'>drift_cusp]<-3 #stay at outbreak

df_clusters$TrueCl <- df_work$cluster #add true clusters to clusters df

#re-order clusters to match true_cl
df_clusters_matched <- match.clusters(df_clusters,"TrueCl",logging=FALSE)
#head(df_clusters_matched)

#plot and export clustering solutions
for (mthd in colnames(df_clusters_matched)){
  this_plot <- plot.timeseries.clustered(df_outcomes,df_clusters_matched[,mthd],mthd)
  loc <- paste(getwd(),"/Budworms/Plots/2018-06-10/",Sys.Date(),"-",mthd,".png",sep="")
  png(loc,height=6,width=12,units="cm",res=300,type="cairo")
  print(this_plot)
  dev.off()
  #print(paste(mthd,"done."))
}
print(paste("Done at",Sys.time()))
      