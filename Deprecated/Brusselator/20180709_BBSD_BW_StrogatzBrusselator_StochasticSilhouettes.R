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
cl_methods <- c(rep("DTW",5),rep("SBD",5),rep("GAK",5))

df_sils <- compare.silhouettes(df_outcomes,cl_methods,2,5,logging=TRUE)

#plot silhouette trajectories
df_sils_long <- melt(df_sils,id.vars = c("k"),variable.name="method")
plt <- ggplot(df_sils_long, aes(x = k, y = value,color=method, group = method)) + geom_point() + geom_line() +
  xlab("k") + ylab("silhouette width")
plot(plt)


loc <- paste(getwd(),"/Report/Report/Figures/",Sys.Date(),"-StochasticSilhouettes2.png",sep="")
png(loc,height=11,width=16,units="cm",res=300,type="cairo")
print(plt)
dev.off()


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