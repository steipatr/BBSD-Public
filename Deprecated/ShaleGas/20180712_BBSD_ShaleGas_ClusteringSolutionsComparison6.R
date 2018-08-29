#adjust working directory
getwd()
setwd("./GitHub/BBSD")
getwd()

#load functions
source("20180626_BBSD_R_Functions.R")

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
df_red <- df_outcomes[sample(nrow(df_outcomes), 500), ]

#compare cluster count possibilities across all methods
#("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD","LCM")
cl_methods <- c("CID","CORT","DWT","DTW","SBD")

#test methods sequentially, some seem to fail

k=6 #clusters
for (m in cl_methods){
  cl <- get.clusters(df_red,m,k)
  this_plot <- plot.timeseries.clustered(df_red,cl)
  loc <- paste(getwd(),"/Report/Report/Figures/ShaleGas/ClusteringSolutions/",k,"/",Sys.Date(),"-",m,k,"-100Sequential.png",sep="")
  ggsave(plot = this_plot, loc, h = 5, w = 8, type = "cairo-png")
}


#df_cl <- get.clusters.df(df_red,cl_methods,10)

#match on CID
df_clm <- match.clusters(df_cl,"CID",logging=TRUE)

#plot every clustering solution


for (mthd in colnames(df_clm)){
  this_plot <- plot.timeseries.clustered(df_red,df_clm[,mthd],mthd)
  loc <- paste(getwd(),"/Report/Report/Figures/ShaleGas/ClusteringSolutions/",k,"/",Sys.Date(),"-",mthd,k,"-500.png",sep="")
  ggsave(plot = this_plot, loc, h = 5, w = 8, type = "cairo-png")
}



#merge plots


loc <- paste(getwd(),"/Report/Report/Figures/",Sys.Date(),"-BudwormsSilhouettes.png",sep="")
png(loc,height=11,width=16,units="cm",res=300,type="cairo")
print(plt)
dev.off()
