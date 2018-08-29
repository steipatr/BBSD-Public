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
outcome <- "Oilprice"
experiments <- "Experiments"

path_outcomes <- paste(wd,"/Data/", data_date,outcome,".feather", sep="")
path_experiments <- paste(wd,"/Data/", data_date,experiments,".feather", sep="")

df_outcomes <- read_feather(path_outcomes)

df_red <- df_outcomes[sample(nrow(df_outcomes), 500), ]

#compare cluster count possibilities across methods using silhouettes. 
#choose from ("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD","LCM")
cl_methods <- c("CID","DWT",rep("SBD",3))

df_sils <- compare.silhouettes(df_red,cl_methods,kmin=2,kmax=24,kstep=2,logging=TRUE)

head(df_sils)

#plot silhouette trajectories
df_sils_long <- melt(df_sils,id.vars = c("k"),variable.name="method")
sil_plt <- ggplot(df_sils_long, aes(x = k, y = value,color=method, group = method)) + geom_point() + geom_line() +
  xlab("k") + ylab("silhouette width") + guides(color=guide_legend(ncol=1))
plot(sil_plt)


#export plot
saveloc <- paste(getwd(),"/Figures/",Sys.Date(),"-OilPrice-Silhouettes.png",sep="")
ggsave(plot = sil_plt, saveloc, h = 5, w = 8, type = "cairo-png")
