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
df_red <- df_outcomes[800:1000, ]

#compare cluster count possibilities across all methods
#("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD","LCM")
cl_methods <- c("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK")

#dev for cluster-specific coloring
k=6 #clusters
cl_dev <- get.clusters(df_red,"CID",k)
#df_expts$cl[df_expts$'b0' < (1+(df_expts$'a')^2)]<-1 #fixed point
plot_dev <- plot.timeseries.clustered(df_red[cl_dev == 2],cl_dev)
plot(plot_dev)


#################################
#plotting
require("ggplot2")
require("reshape2")

df_ts <- df_red
clusters <- cl_dev

#reformat wide into long
expt_id <- as.character(1:nrow(df_ts))
df_ts_id <- cbind(expt_id=expt_id, df_ts)
df_ts_long <- melt(df_ts_id,variable.name="time_step")

#build ggplot object
clustered_line_plot1 <- ggplot(data=df_ts_long, aes(x=time_step, y=value,colour=as.factor(rep(clusters,ncol(df_ts))))) + 
  geom_line(aes(group=expt_id)) +
  labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
  xlab("time step") + ylab("value") +
  scale_color_manual(values=c("red", "grey", "grey","grey","grey","grey"))

clustered_line_plot2 <- ggplot(data=df_ts_long, aes(x=time_step, y=value,colour=as.factor(rep(clusters,ncol(df_ts))))) + 
  geom_line(aes(group=expt_id)) +
  labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
  xlab("time step") + ylab("value") +
  scale_color_manual(values=c("grey", "red", "grey","grey","grey","grey"))

clustered_line_plot3 <- ggplot(data=df_ts_long, aes(x=time_step, y=value,colour=as.factor(rep(clusters,ncol(df_ts))))) + 
  geom_line(aes(group=expt_id)) +
  labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
  xlab("time step") + ylab("value") +
  scale_color_manual(values=c("grey", "grey", "red","grey","grey","grey"))

clustered_line_plot4 <- ggplot(data=df_ts_long, aes(x=time_step, y=value,colour=as.factor(rep(clusters,ncol(df_ts))))) + 
  geom_line(aes(group=expt_id)) +
  labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
  xlab("time step") + ylab("value") +
  scale_color_manual(values=c("grey", "grey", "grey","red","grey","grey"))

clustered_line_plot5 <- ggplot(data=df_ts_long, aes(x=time_step, y=value,colour=as.factor(rep(clusters,ncol(df_ts))))) + 
  geom_line(aes(group=expt_id)) +
  labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
  xlab("time step") + ylab("value") +
  scale_color_manual(values=c("grey", "grey", "grey","grey","red","grey"))

clustered_line_plot6 <- ggplot(data=df_ts_long, aes(x=time_step, y=value,colour=as.factor(rep(clusters,ncol(df_ts))))) + 
  geom_line(aes(group=expt_id)) +
  labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
  xlab("time step") + ylab("value") +
  scale_color_manual(values=c("grey", "grey", "grey","grey","grey","red"))

require("gridExtra")
allclusters <- grid.arrange(clustered_line_plot1,clustered_line_plot2,clustered_line_plot3,clustered_line_plot4,clustered_line_plot5,clustered_line_plot6)

loc <- paste(getwd(),"/Report/Report/Figures/ShaleGas/ClusteringSolutions/",k,"/",Sys.Date(),"-CID-100Grid.png",sep="")
ggsave(plot = allclusters, loc, h = 12, w = 12, type = "cairo-png")

###################################################################################
##for all methods
for (m in cl_methods){
  cl <- get.clusters(df_red,m,k)
  this_plot <- plot.timeseries.clustered(df_red,cl)
  loc <- paste(getwd(),"/Report/Report/Figures/ShaleGas/ClusteringSolutions/",k,"/",Sys.Date(),"-",m,k,"-100Sequential.png",sep="")
  ggsave(plot = this_plot, loc, h = 5, w = 8, type = "cairo-png")
}



