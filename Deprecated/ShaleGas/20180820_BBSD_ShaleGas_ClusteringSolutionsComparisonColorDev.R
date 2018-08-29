#adjust working directory
getwd()
setwd("./GitHub/BBSD")
getwd()

#load functions
source("20180819_BBSD_R_Functions.R")

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

#dev for cluster-specific coloring
k=6 #clusters
cl_methods <- c("CID","CORT","DWT","DTW","SBD")
df_cl <- get.clusters.df(df_red,cl_methods,k)
df_clm <- match.clusters(df_cl,"CID",logging=TRUE)

#plot every clustering solution
for (mthd in colnames(df_clm)){
  this_plot <- plot.timeseries.clustered(df_red,df_clm[,mthd],mthd)
  loc <- paste(getwd(),"/Report/Final_Report/Figures/ShaleGas/ClusteringSolutions/",k,"/",Sys.Date(),"-",mthd,k,"-200-Matched.png",sep="")
  ggsave(plot = this_plot, loc, h = 5, w = 8, type = "cairo-png")
}

#################################
#plotting
require("ggplot2")
require("reshape2")

df_ts <- df_red

#build colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols <- gg_color_hue(k)

for (mthd in colnames(df_clm)){
  print(mthd)
  clusters <- df_clm[,mthd]
  
  #reformat wide into long
  expt_id <- as.character(1:nrow(df_ts))
  df_ts_id <- cbind(expt_id=expt_id, df_ts)
  df_ts_long <- melt(df_ts_id,variable.name="time_step")
  
  #build ggplot object
  clustered_line_plot1 <- ggplot(data=df_ts_long, aes(x=time_step, y=value,colour=as.factor(rep(clusters,ncol(df_ts))))) + 
    geom_line(aes(group=expt_id)) +
    labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
    xlab("time step") + ylab("value") +
    scale_color_manual(values=c(cols[1], "grey", "grey","grey","grey","grey"))
  
  clustered_line_plot2 <- ggplot(data=df_ts_long, aes(x=time_step, y=value,colour=as.factor(rep(clusters,ncol(df_ts))))) + 
    geom_line(aes(group=expt_id)) +
    labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
    xlab("time step") + ylab("value") +
    scale_color_manual(values=c("grey", cols[2], "grey","grey","grey","grey"))
  
  clustered_line_plot3 <- ggplot(data=df_ts_long, aes(x=time_step, y=value,colour=as.factor(rep(clusters,ncol(df_ts))))) + 
    geom_line(aes(group=expt_id)) +
    labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
    xlab("time step") + ylab("value") +
    scale_color_manual(values=c("grey", "grey", cols[3],"grey","grey","grey"))
  
  clustered_line_plot4 <- ggplot(data=df_ts_long, aes(x=time_step, y=value,colour=as.factor(rep(clusters,ncol(df_ts))))) + 
    geom_line(aes(group=expt_id)) +
    labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
    xlab("time step") + ylab("value") +
    scale_color_manual(values=c("grey", "grey", "grey",cols[4],"grey","grey"))
  
  clustered_line_plot5 <- ggplot(data=df_ts_long, aes(x=time_step, y=value,colour=as.factor(rep(clusters,ncol(df_ts))))) + 
    geom_line(aes(group=expt_id)) +
    labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
    xlab("time step") + ylab("value") +
    scale_color_manual(values=c("grey", "grey", "grey","grey",cols[5],"grey"))
  
  clustered_line_plot6 <- ggplot(data=df_ts_long, aes(x=time_step, y=value,colour=as.factor(rep(clusters,ncol(df_ts))))) + 
    geom_line(aes(group=expt_id)) +
    labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
    xlab("time step") + ylab("value") +
    scale_color_manual(values=c("grey", "grey", "grey","grey","grey",cols[6]))
  
  require("gridExtra")
  allclusters <- grid.arrange(clustered_line_plot1,clustered_line_plot2,clustered_line_plot3,clustered_line_plot4,clustered_line_plot5,clustered_line_plot6)
  
  loc <- paste(getwd(),"/Report/Final_Report/Figures/ShaleGas/ClusteringSolutions/",k,"/",Sys.Date(),"-",mthd,"-200Grid.png",sep="")
  ggsave(plot = allclusters, loc, h = 12, w = 12, type = "cairo-png")
  print(mthd)
  print("done!")
}

