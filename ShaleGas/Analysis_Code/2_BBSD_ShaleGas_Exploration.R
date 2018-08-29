#adjust working directory
getwd()
setwd("../")
wd <- getwd()

#load functions
source("../20180819_BBSD_R_Functions.R")

#load libraries
library("feather")

#load data
data_date <- "2018-08-29"
outcome <- "Oilprice"
experiments <- "Experiments"

path_outcomes <- paste(wd,"/Data/", data_date,outcome,".feather", sep="")
path_experiments <- paste(wd,"/Data/", data_date,experiments,".feather", sep="")

df_outcomes <- read_feather(path_outcomes)

#2000 series is too much, randomly select 500
df_outcomes_red <- df_outcomes[sample(nrow(df_outcomes), 500), ]

#plot time series
ts_plot <- plot.timeseries(df_outcomes_red)
plot(ts_plot)

#export plot
loc <- paste(wd,"/Figures/",Sys.Date(),"-ShaleGas-","Exploration.png",sep="")
ggsave(plot = ts_plot, loc, h = 6, w = 8, type = "cairo-png")
