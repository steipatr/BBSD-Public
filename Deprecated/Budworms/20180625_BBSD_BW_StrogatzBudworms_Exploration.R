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

#plot time series
ts_plot <- plot.timeseries(df_outcomes)
plot(ts_plot)


#export plot
loc <- paste(getwd(),"/Report/Report/Figures/",Sys.Date(),"-Budworms","Exploration.png",sep="")
png(loc,height=6,width=12,units="cm",res=300,type="cairo")
print(ts_plot)
dev.off()
#print(paste(mthd,"done."))