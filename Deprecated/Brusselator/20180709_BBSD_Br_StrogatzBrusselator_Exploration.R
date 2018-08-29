#adjust working directory
getwd()
setwd("./GitHub/BBSD")
getwd()

#load functions
source("20180625_BBSD_R_Functions.R")

#load libraries
library("feather")
library("Cairo")

#load data
file_location <- "C:/Users/Patrick/Documents/GitHub/BBSD/Brusselator/Data/"
data_date <- "2018-07-09"
outcomes_name <- "y"
experiments_name <- "Experiments"

path_outcomes <- paste(file_location, data_date,outcomes_name,".feather", sep="")
path_experiments <- paste(file_location, data_date,experiments_name,".feather", sep="")

df_outcomes <- read_feather(path_outcomes)

#plot time series
ts_plot <- plot.timeseries(df_outcomes)
plot(ts_plot)


#export plot
saveloc <- paste(getwd(),"/Report/Report/Figures/Brusselator/",Sys.Date(),"-Brusselator","Exploration.png",sep="")
ggsave(plot = ts_plot, saveloc, h = 6, w = 12, type = "cairo-png")


#print(paste(mthd,"done."))