#adjust working directory
getwd()
setwd("../")
wd <- getwd()

#load functions
source("../20180819_BBSD_R_Functions.R")

#load libraries
library("feather")
require("ggplot2")
require("GGally")

#load data
data_date <- "2018-08-29"
outcome <- "Oilprice"
experiments <- "Experiments"

path_outcomes <- paste(wd,"/Data/", data_date,outcome,".feather", sep="")
path_experiments <- paste(wd,"/Data/", data_date,experiments,".feather", sep="")

df_expts <- read_feather(path_experiments)

#get clustering solutions for a given k value 
cl_methods <- c("CID")

df_cl6 <- get.clusters.df(df_outcomes,cl_methods,6) #for random clusters: sample(1:6, 2000, replace=T)

#######pairs plot
#build dataframe of most predictive parameters (identified through manual analysis of rule induction in Python)
df_expts_red <- df_expts[c('Switch prices or supply dominance in demand subtitution', 'Initial unit costs oil','Effect of supply shortage on GDP growth','Switch legal emission cap','Average throughput time stocks')]

#transform clusters to factors for clearer visualization
df_expts_red$clusterID <- as.factor(df_cl6$CID)

#add jitter to categorical inputs
df_expts_red$`Switch prices or supply dominance in demand subtitution` <- df_expts_red$`Switch prices or supply dominance in demand subtitution` + runif(nrow(df_expts_red),-0.3,0.3)
df_expts_red$`Switch legal emission cap` <- df_expts_red$`Switch legal emission cap` + runif(nrow(df_expts_red),-0.3,0.3)

#plot and save
pairsplot <- ggpairs(df_expts_red, aes(color = clusterID, alpha = 0.4))
saveloc <- paste(getwd(),"/Figures/",Sys.Date(),"-CID-6-PairsPlot.png",sep="")
ggsave(plot = pairsplot, saveloc, h = 12, w = 12, type = "cairo-png")
