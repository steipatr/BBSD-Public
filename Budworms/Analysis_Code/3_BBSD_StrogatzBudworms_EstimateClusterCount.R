#adjust working directory
getwd()
setwd("../")
wd <- getwd()

#load functions
source("../20180819_BBSD_R_Functions.R")

#load libraries
library("feather")
require("reshape2")
require("ggplot2")

#load data
data_date <- "2018-08-29"
outcome <- "x"
experiments <- "Experiments"

path_outcomes <- paste(wd,"/Data/", data_date,outcome,".feather", sep="")
path_experiments <- paste(wd,"/Data/", data_date,experiments,".feather", sep="")

df_outcomes <- read_feather(path_outcomes)

#compare cluster count possibilities across methods using silhouettes
#choose from ("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD","LCM")
cl_methods <- c("CID","CORT","DWT")
df_sils <- compare.silhouettes(df_outcomes,cl_methods,2,6,logging=TRUE)

#plot silhouette trajectories
df_sils_long <- melt(df_sils,id.vars = c("k"),variable.name="method")
sils_plot <- ggplot(df_sils_long, aes(x = k, y = value,color=method, group = method)) + geom_point() + geom_line() +
  xlab("k") + ylab("silhouette width")
plot(sils_plot)

#export plot
loc <- paste(wd,"/Figures/",Sys.Date(),"-Budworms","Silhouettes.png",sep="")
ggsave(plot = sils_plot, loc, h = 8, w = 8, type = "cairo-png")
