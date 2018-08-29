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

#2000 series is too much, randomly select 500
df_red <- df_outcomes[sample(nrow(df_outcomes), 500), ]

#plot time series
ts_plot <- plot.timeseries(df_red)
plot(ts_plot)


#export plot
loc <- paste(getwd(),"/Report/Report/Figures/ShaleGas/",Sys.Date(),"-Oilprice","Exploration500.png",sep="")
ggsave(plot = ts_plot, loc, h = 6, w = 12, type = "cairo-png")
