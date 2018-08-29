#adjust working directory
getwd()
setwd("./GitHub/BBSD")
getwd()

#load functions
source("20180726_BBSD_R_Functions.R")

#load libraries
library("feather")
library("Cairo")
library("caret")
library("reshape2")

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
#cl_methods <- c(rep("DTW",5),rep("SBD",5),rep("GAK",5))
cl_methods <- c("CID","CORT","DWT","SBD")

df_sils <- compare.silhouettes(df_red,cl_methods,19,499,20,logging=TRUE)

head(df_sils)

#plot silhouette trajectories
df_sils_long <- melt(df_sils,id.vars = c("k"),variable.name="method")
sil_plt <- ggplot(df_sils_long, aes(x = k, y = value,color=method, group = method)) + geom_point() + geom_line() +
  xlab("k") + ylab("silhouette width") + guides(color=guide_legend(ncol=1))
plot(sil_plt)


#export plot
saveloc <- paste(getwd(),"/Report/Report/Figures/ShaleGas/",Sys.Date(),"-OilpriceSilhouettes19-499multi.png",sep="")
ggsave(plot = sil_plt, saveloc, h = 5, w = 8, type = "cairo-png")
