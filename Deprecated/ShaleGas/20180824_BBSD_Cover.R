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


#plot
#reformat wide into long
expt_id <- as.character(1:nrow(df_ts))
df_ts_id <- cbind(expt_id=expt_id[0:500], df_outcomes[0:500,])
df_ts_long <- melt(df_ts_id,variable.name="time_step")

cover_plot <- ggplot(data=df_ts_long, aes(x=time_step, y=value,colour=as.factor(rep(df_cl2$CID[0:500],ncol(df_ts))))) + 
  geom_line(aes(group=expt_id)) +
  scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
  xlab("time step") + ylab("value") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  scale_color_manual(values=c("grey","grey","grey","#00A6D6","grey","grey"))

cover_plot

loc <- paste(getwd(),"/Report/Final_Report/Figures/",Sys.Date(),"-Cover.png",sep="")
ggsave(plot = cover_plot, loc, h = 10, w = 16, type = "cairo-png")
