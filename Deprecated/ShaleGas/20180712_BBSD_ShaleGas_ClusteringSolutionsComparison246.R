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
file_location <- "C:/Users/Patrick/Documents/GitHub/BBSD/ShaleGas/Data/"
data_date <- "2018-07-12"
outcomes_name <- "Oilprice"
experiments_name <- "Experiments"

path_outcomes <- paste(file_location, data_date,outcomes_name,".feather", sep="")
path_experiments <- paste(file_location, data_date,experiments_name,".feather", sep="")

df_outcomes <- read_feather(path_outcomes)
df_red <- df_outcomes[sample(nrow(df_outcomes), 20), ]

#compare cluster count possibilities across all methods
#("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD","LCM")
cl_methods <- c("DWT")

df_cl2 <- get.clusters.df(df_red,cl_methods,2)

df_cl4 <- get.clusters.df(df_red,cl_methods,4)

df_cl6 <- get.clusters.df(df_red,cl_methods,6)

#match cluster indices on CID
#df_cl2t <- match.clusters(df_cl2c,"CID",logging=TRUE)
#df_cl4t <- match.clusters(df_cl4c,"CID",logging=TRUE)
#df_cl6t <- match.clusters(df_cl6c,"CID",logging=TRUE)

#plot every clustering solution

for (k in c(2,4,6)){
  if (k==2){df_clusters_matched <- df_cl2}
  if (k==4){df_clusters_matched <- df_cl4}
  if (k==6){df_clusters_matched <- df_cl6}
  
  for (mthd in colnames(df_clusters_matched)){
    this_plot <- plot.timeseries.clustered(df_outcomes,df_clusters_matched[,mthd],mthd)
    loc <- paste(getwd(),"/Report/Report/Figures/ShaleGas/ClusteringSolutions/",k,"/",Sys.Date(),"-",mthd,k,".png",sep="")
    ggsave(plot = this_plot, loc, h = 5, w = 8, type = "cairo-png")
  }
}


#merge plots


loc <- paste(getwd(),"/Report/Report/Figures/",Sys.Date(),"-BudwormsSilhouettes.png",sep="")
png(loc,height=11,width=16,units="cm",res=300,type="cairo")
print(plt)
dev.off()
