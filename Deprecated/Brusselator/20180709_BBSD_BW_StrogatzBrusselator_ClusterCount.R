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
#cl_methods <- c(rep("DTW",5),rep("SBD",5),rep("GAK",5))
cl_methods <- c("LCM","ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD")

df_sils <- compare.silhouettes(df_outcomes,cl_methods,2,7,logging=TRUE)

#plot silhouette trajectories
df_sils_long <- melt(df_sils,id.vars = c("k"),variable.name="method")
sil_plt <- ggplot(df_sils_long, aes(x = k, y = value,color=method, group = method)) + geom_point() + geom_line() +
  xlab("k") + ylab("silhouette width") + guides(color=guide_legend(ncol=2))
plot(sil_plt)


#export plot
saveloc <- paste(getwd(),"/Report/Report/Figures/Brusselator/",Sys.Date(),"-BrusselatorSilhouettes27.png",sep="")
ggsave(plot = sil_plt, saveloc, h = 5, w = 8, type = "cairo-png")


# #plot time series
# ts_plot <- plot.timeseries(df_outcomes)
# plot(ts_plot)
# 
# 
# #export plot
# loc <- paste(getwd(),"/Report/Report/Figures/",Sys.Date(),"-Budworms","Exploration.png",sep="")
# png(loc,height=6,width=12,units="cm",res=300,type="cairo")
# print(this_plot)
# dev.off()
# #print(paste(mthd,"done."))