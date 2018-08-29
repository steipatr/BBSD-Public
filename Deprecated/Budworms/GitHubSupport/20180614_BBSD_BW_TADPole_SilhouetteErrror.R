require("feather")
require("dtwclust")

#load data generated in Python with feather
getwd()
df_outcomes <- read_feather(paste(getwd(),"/2018-05-31x.feather",sep=""))

#run TADPole clustering
tad_data <- tsclust(df_outcomes, type = "tadpole", k = 3L,
                    control = tadpole_control(dc = 10, window.size = 20L))

#read out clusters
tad_cl <- tad_data@cluster
head(tad_cl)

#set environment details
environment(tad_data@family@dist)$distance <- "dtw_basic"

#(try to) get silhouette width
tad_sil <- unname(cvi(tad_data,type="Sil"))
tad_sil




#plot clustered time series
require("ggplot2")
require("reshape2")

#reformat wide into long
df_outcomes_id <- cbind(expt_id=as.character(1:nrow(df_outcomes)), df_outcomes)
df_outcomes_long <- melt(df_outcomes_id,variable.name="time_step")


#build title
title <- paste("TADPole Clustering, n=",nrow(df_outcomes), ", k=",max(tad_cl),sep="" )

#build ggplot object
clustered_line_plot <- ggplot(data=df_outcomes_long, aes(x=time_step, y=value,colour=as.factor(rep(tad_cl,ncol(df_outcomes))))) + 
  geom_line(aes(group=expt_id)) +
  labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
  ggtitle(title) + xlab("time step") + ylab("population")

plot(clustered_line_plot)
