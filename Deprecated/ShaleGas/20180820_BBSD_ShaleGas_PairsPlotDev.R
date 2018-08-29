library(GGally)

########
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
path_experiments <- paste(file_location, "2018-08-20",experiments_name,".feather", sep="")

df_outcomes <- read_feather(path_outcomes)
df_expts <- read_feather(path_experiments)

#get cluster memberships
cl_methods <- c("CID")
k=6
df_cl2 <- get.clusters.df(df_outcomes,cl_methods,k)
head(df_cl2)

ts_plot <- plot.timeseries.clustered(df_outcomes,df_cl2$CID)
loc <- paste(getwd(),"/Report/Final_Report/Figures/ShaleGas/ClusteringSolutions/",k,"/",Sys.Date(),"-",mthd,"-2000.png",sep="")
ggsave(plot = ts_plot, loc, h = 5, w = 8, type = "cairo-png")


#######new cluster members plot
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols <- gg_color_hue(k)

df_ts <- df_outcomes
df_clm <- df_cl2

clusters <- df_clm[,mthd]
head(clusters)
clusters_mutated <- clusters

for (el in clusters){
  if (el==1){clusters_mutated[el]=2}
  if (el==2){clusters_mutated[el]=3}
  if (el==3){clusters_mutated[el]=1}
}
head(clusters_mutated)
clusters <- clusters_mutated

for (mthd in colnames(df_clm)){
  print(mthd)
  
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
  
  loc <- paste(getwd(),"/Report/Final_Report/Figures/ShaleGas/ClusteringSolutions/",k,"/",Sys.Date(),"-",mthd,"-2000Grid.png",sep="")
  ggsave(plot = allclusters, loc, h = 12, w = 12, type = "cairo-png")
  print(mthd)
  print("done!")
}


#######pairs plot
#build dataframe
df_expts <- read_feather(path_experiments)
df_expts$clusterID <- clusters
head(df_expts)

#convert cluster column to categorical, add jitter
df_expts$clusterID <- as.factor(df_expts$clusterID)
#employ.data$salary <- employ.data$salary + runif(nrow(employ.data),-0.5,0.5)
df_expts$`Switch prices or supply dominance in demand subtitution` <- df_expts$`Switch prices or supply dominance in demand subtitution` + runif(nrow(df_expts),-0.3,0.3)
df_expts$`Switch legal emission cap` <- df_expts$`Switch legal emission cap` + runif(nrow(df_expts),-0.3,0.3)

pairsplot <- ggpairs(df_expts, aes(color = clusterID, alpha = 0.4)) 
saveloc <- paste(getwd(),"/Report/Final_Report/Figures/ShaleGas/Subspaces/",Sys.Date(),"-CID6_PairsPlot-2000Large.png",sep="")
ggsave(plot = pairsplot, saveloc, h = 20, w = 24, type = "cairo-png")

#######uniform/random pairs plot for comparison
#build dataframe
df_expts <- read_feather(path_experiments)
df_expts$clusterID <- sample(1:6, 2000, replace=T)
head(df_expts)

#convert cluster column to categorical, add jitter
df_expts$clusterID <- as.factor(df_expts$clusterID)
#employ.data$salary <- employ.data$salary + runif(nrow(employ.data),-0.5,0.5)
df_expts$`Switch prices or supply dominance in demand subtitution` <- df_expts$`Switch prices or supply dominance in demand subtitution` + runif(nrow(df_expts),-0.3,0.3)
df_expts$`Switch legal emission cap` <- df_expts$`Switch legal emission cap` + runif(nrow(df_expts),-0.3,0.3)

pairsplot <- ggpairs(df_expts, aes(color = clusterID, alpha = 0.4)) 
saveloc <- paste(getwd(),"/Report/Final_Report/Figures/ShaleGas/Subspaces/",Sys.Date(),"-CID6_PairsPlot-2000Random.png",sep="")
ggsave(plot = pairsplot, saveloc, h = 10, w = 12, type = "cairo-png")


######calculate coverage and density and foreign cluster members for each box

#reimport experiments to clean jitter
df_expts <- read_feather(path_experiments)
df_expts$clusterID <- clusters
head(df_expts)

#simplify column names
colnames(df_expts) <- c("demsub","oilcosts","supshort","emcap","thruput","clusterID")
head(df_expts)

#for cluster 1
k <- 1
cluster_members <- nrow(subset(df_expts, clusterID==k))

total_in_box <- nrow(subset(df_expts, demsub==1 &
                              oilcosts>1002 & oilcosts<4945 &
                              supshort> -0.299 & supshort< -0.088 &
                              thruput>0.057 & thruput<0.2)) 

all_members_in_box <- c(rep(0,6))

for (i in 1:6){
  all_members_in_box[i] <- nrow(subset(df_expts, demsub==1 &
                                         oilcosts>1002 & oilcosts<4945 &
                                         supshort> -0.299 & supshort< -0.088 &
                                         thruput>0.057 & thruput<0.2 &
                                         clusterID==i)) 
}

writeLines(c(paste("Cluster",k),
              paste("Members:",cluster_members),
             paste("Total in box:",total_in_box),
             paste("All members in box:",all_members_in_box),
             paste("Coverage:",all_members_in_box[k]/cluster_members),
             paste("Density:",all_members_in_box[k]/total_in_box)
             ))

#for cluster 2
k <- 2
cluster_members <- nrow(subset(df_expts, clusterID==k))

total_in_box <- nrow(subset(df_expts, demsub==2 &
                              supshort> -0.27 & supshort< 0 &
                              emcap==0)) 

members_in_box <- nrow(subset(df_expts, demsub==2 &
                                supshort> -0.27 & supshort< 0 &
                                emcap==0 &
                                clusterID==k)) 

all_members_in_box <- c(rep(0,6))

for (i in 1:6){
  all_members_in_box[i] <- nrow(subset(df_expts, demsub==2 &
                                         supshort> -0.27 & supshort< 0 &
                                         emcap==0 &
                                         clusterID==i)) 
}

writeLines(c(paste("Cluster",k),
             paste("Members:",cluster_members),
             paste("Total in box:",total_in_box),
             paste("All members in box:",all_members_in_box),
             paste("Coverage:",all_members_in_box[k]/cluster_members),
             paste("Density:",all_members_in_box[k]/total_in_box)
))

#for cluster 3
k <- 3
cluster_members <- nrow(subset(df_expts, clusterID==k))

total_in_box <- nrow(subset(df_expts, demsub==2 &
                              supshort> -0.22 & supshort< 0 &
                              emcap!=0)) 

members_in_box <- nrow(subset(df_expts, demsub==2 &
                                supshort> -0.22 & supshort< 0 &
                                emcap!=0 &
                                clusterID==k)) 

all_members_in_box <- c(rep(0,6))

for (i in 1:6){
  all_members_in_box[i] <- nrow(subset(df_expts, demsub==2 &
                                         supshort> -0.22 & supshort< 0 &
                                         emcap!=0 &
                                         clusterID==i)) 
}

writeLines(c(paste("Cluster",k),
             paste("Members:",cluster_members),
             paste("Total in box:",total_in_box),
             paste("All members in box:",all_members_in_box),
             paste("Coverage:",all_members_in_box[k]/cluster_members),
             paste("Density:",all_members_in_box[k]/total_in_box)
))

#for cluster 4
k <- 4
cluster_members <- nrow(subset(df_expts, clusterID==k))

total_in_box <- nrow(subset(df_expts, demsub==1 &
                              oilcosts>3368 & oilcosts<7997 &
                              thruput>0.05 & thruput<0.11)) 

members_in_box <- nrow(subset(df_expts, demsub==1 &
                                oilcosts>3368 & oilcosts<7997 &
                                thruput>0.05 & thruput<0.11 &
                                clusterID==k)) 

all_members_in_box <- c(rep(0,6))

for (i in 1:6){
  all_members_in_box[i] <- nrow(subset(df_expts, demsub==1 &
                                         oilcosts>3368 & oilcosts<7997 &
                                         thruput>0.05 & thruput<0.11 &
                                         clusterID==i)) 
}

writeLines(c(paste("Cluster",k),
             paste("Members:",cluster_members),
             paste("Total in box:",total_in_box),
             paste("All members in box:",all_members_in_box),
             paste("Coverage:",all_members_in_box[k]/cluster_members),
             paste("Density:",all_members_in_box[k]/total_in_box)
))

#for cluster 5
k <- 5
cluster_members <- nrow(subset(df_expts, clusterID==k))

total_in_box <- nrow(subset(df_expts, demsub==1 &
                              oilcosts>2293 & oilcosts<7753 &
                              supshort> -0.299 & supshort< -0.015 &
                              thruput>0.05 & thruput<0.11)) 

members_in_box <- nrow(subset(df_expts, demsub==1 &
                                oilcosts>2293 & oilcosts<7753 &
                                supshort> -0.299 & supshort< -0.015 &
                                thruput>0.084 & thruput<0.2 &
                                clusterID==k)) 

all_members_in_box <- c(rep(0,6))

for (i in 1:6){
  all_members_in_box[i] <- nrow(subset(df_expts, demsub==1 &
                                         oilcosts>2293 & oilcosts<7753 &
                                         supshort> -0.299 & supshort< -0.015 &
                                         thruput>0.084 & thruput<0.2 &
                                         clusterID==i)) 
}

writeLines(c(paste("Cluster",k),
             paste("Members:",cluster_members),
             paste("Total in box:",total_in_box),
             paste("All members in box:",all_members_in_box),
             paste("Coverage:",all_members_in_box[k]/cluster_members),
             paste("Density:",all_members_in_box[k]/total_in_box)
))

#for cluster 6
k <- 6
cluster_members <- nrow(subset(df_expts, clusterID==k))

total_in_box <- nrow(subset(df_expts, demsub==2 &
                              oilcosts>1002 & oilcosts<5819 &
                              supshort> -0.282 & supshort< -0.028 &
                              emcap!=0)) 

members_in_box <- nrow(subset(df_expts, demsub==2 &
                                oilcosts>1002 & oilcosts<5819 &
                                supshort> -0.282 & supshort< -0.028 &
                                emcap!=0 &
                                clusterID==k)) 

all_members_in_box <- c(rep(0,6))

for (i in 1:6){
  all_members_in_box[i] <- nrow(subset(df_expts, demsub==2 &
                                         oilcosts>1002 & oilcosts<5819 &
                                         supshort> -0.282 & supshort< -0.028 &
                                         emcap!=0 &
                                         clusterID==i)) 
}

writeLines(c(paste("Cluster",k),
             paste("Members:",cluster_members),
             paste("Total in box:",total_in_box),
             paste("All members in box:",all_members_in_box),
             paste("Coverage:",all_members_in_box[k]/cluster_members),
             paste("Density:",all_members_in_box[k]/total_in_box)
))






#####determine shared box members for every pair of boxes
#clusters 1 and 2
shared12 <- nrow(subset(df_expts, demsub==1 &
                                         oilcosts>1002 & oilcosts<4945 &
                                         supshort> -0.299 & supshort< -0.088 &
                                         thruput>0.057 & thruput<0.2 &
                          demsub==1 &
                          oilcosts>1002 & oilcosts<4945 &
                          supshort> -0.299 & supshort< -0.088 &
                          thruput>0.057 & thruput<0.2)) 

shared12

shared13 <- nrow(subset(df_expts, demsub==1 &
                          oilcosts>1002 & oilcosts<4945 &
                          supshort> -0.299 & supshort< -0.088 &
                          thruput>0.057 & thruput<0.2 &
                          
                          demsub==2 &
                          supshort> -0.22 & supshort< 0 &
                          emcap!=0)) 

shared13

shared14 <- nrow(subset(df_expts, demsub==1 &
                          oilcosts>1002 & oilcosts<4945 &
                          supshort> -0.299 & supshort< -0.088 &
                          thruput>0.057 & thruput<0.2 &
                          
                          demsub==1 &
                          oilcosts>3368 & oilcosts<7997 &
                          thruput>0.05 & thruput<0.11)) 

shared14

shared15 <- nrow(subset(df_expts, demsub==1 &
                          oilcosts>1002 & oilcosts<4945 &
                          supshort> -0.299 & supshort< -0.088 &
                          thruput>0.057 & thruput<0.2 &
                          
                          demsub==1 &
                          oilcosts>2293 & oilcosts<7753 &
                          supshort> -0.299 & supshort< -0.015 &
                          thruput>0.084 & thruput<0.2 )) 

shared15

shared16 <- nrow(subset(df_expts, demsub==1 &
                          oilcosts>1002 & oilcosts<4945 &
                          supshort> -0.299 & supshort< -0.088 &
                          thruput>0.057 & thruput<0.2 &
                          
                          demsub==2 &
                          oilcosts>1002 & oilcosts<5819 &
                          supshort> -0.282 & supshort< -0.028 &
                          emcap!=0 )) 

shared16


#cluster 2
#demsub==2 & supshort> -0.27 & supshort< 0 & emcap==0

shared23 <- nrow(subset(df_expts, demsub==2 & supshort> -0.27 & supshort< 0 & emcap==0 &
                          
                          demsub==2 &
                          supshort> -0.22 & supshort< 0 &
                          emcap!=0)) 

shared23

shared24 <- nrow(subset(df_expts, demsub==2 & supshort> -0.27 & supshort< 0 & emcap==0 &
                          demsub==1 & oilcosts>3368 & oilcosts<7997 & thruput>0.05 & thruput<0.11)) 

shared24

shared25 <- nrow(subset(df_expts, demsub==2 & supshort> -0.27 & supshort< 0 & emcap==0 &
                          demsub==1 & oilcosts>2293 & oilcosts<7753 & supshort> -0.299 & supshort< -0.015 & thruput>0.084 & thruput<0.2)) 

shared25

shared26 <- nrow(subset(df_expts, demsub==2 & supshort> -0.27 & supshort< 0 & emcap==0 &
                          demsub==2 & oilcosts>1002 & oilcosts<5819 & supshort> -0.282 & supshort< -0.028 & emcap!=0)) 

shared26

#cluster 3
#demsub==2 & supshort> -0.22 & supshort< 0 & emcap!=0

shared34 <- nrow(subset(df_expts,demsub==2 & supshort> -0.22 & supshort< 0 & emcap!=0 &
                          demsub==1 & oilcosts>3368 & oilcosts<7997 & thruput>0.05 & thruput<0.11)) 

shared34

shared35 <- nrow(subset(df_expts,demsub==2 & supshort> -0.22 & supshort< 0 & emcap!=0 &
                          demsub==1 & oilcosts>2293 & oilcosts<7753 & supshort> -0.299 & supshort< -0.015 & thruput>0.084 & thruput<0.2)) 

shared35

shared36 <- nrow(subset(df_expts,demsub==2 & supshort> -0.22 & supshort< 0 & emcap!=0 &
                          demsub==2 & oilcosts>1002 & oilcosts<5819 & supshort> -0.282 & supshort< -0.028 & emcap!=0)) 

shared36

total3 <- nrow(subset(df_expts,demsub==2 & supshort> -0.22 & supshort< 0 & emcap!=0 &
                        demsub==2 & supshort> -0.22 & supshort< 0 & emcap!=0)) 

total3

#cluster 4
#demsub==1 & oilcosts>3368 & oilcosts<7997 & thruput>0.05 & thruput<0.11

shared45 <- nrow(subset(df_expts,demsub==1 & oilcosts>3368 & oilcosts<7997 & thruput>0.05 & thruput<0.11 &
                          demsub==1 & oilcosts>2293 & oilcosts<7753 & supshort> -0.299 & supshort< -0.015 & thruput>0.084 & thruput<0.2)) 

shared45

shared46 <- nrow(subset(df_expts,demsub==1 & oilcosts>3368 & oilcosts<7997 & thruput>0.05 & thruput<0.11 &
                          demsub==2 & oilcosts>1002 & oilcosts<5819 & supshort> -0.282 & supshort< -0.028 & emcap!=0)) 

shared46

#cluster 5
#demsub==1 & oilcosts>2293 & oilcosts<7753 & supshort> -0.299 & supshort< -0.015 & thruput>0.084 & thruput<0.2

shared56 <- nrow(subset(df_expts,demsub==1 & oilcosts>2293 & oilcosts<7753 & supshort> -0.299 & supshort< -0.015 & thruput>0.084 & thruput<0.2 &
                          demsub==2 & oilcosts>1002 & oilcosts<5819 & supshort> -0.282 & supshort< -0.028 & emcap!=0)) 

shared56


#cluster 6
#demsub==2 & oilcosts>1002 & oilcosts<5819 & supshort> -0.282 & supshort< -0.028 & emcap!=0

