library(ggplot2)

methodcode <- c("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD","LCM")
runtime <- c(3,1,1,1,2,11,1,4,7,1,2,3,3,7)
accuracy <- c(66,83,82,79,80,78,79,63,76,90,80,70,63,72)

methods.data <- data.frame(methodcode,runtime, accuracy)


# Basic scatter plot
this_plot <- ggplot(methods.data, aes(x=runtime, y=accuracy)) + geom_point(aes(size = 4,color="red")) + geom_text_repel(aes(label=methodcode),hjust=-0.1, vjust=0,size=6,force=5) + 
  theme(legend.position="none") + labs(y = "accuracy [%]",x="runtime [s]")

plot(this_plot)
#save
loc <- paste(getwd(),"/Report/Final_Report/Figures/Brusselator/ConfusionMatrices/",Sys.Date(),"-BrusselatorScatterPerformance.png",sep="")
ggsave(plot=this_plot,loc,h=4,w=6,type="cairo-png")
