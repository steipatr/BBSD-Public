library(ggplot2)

methodcode <- c("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD","LCM")
runtime <- c(5,3,3,1,19,40,1,18,46,17,8,39,1,22)
accuracy <- c(57,89,88,86,72,51,53,87,39,88,90,72,75,88)

methods.data <- data.frame(methodcode,runtime, accuracy)


# Basic scatter plot
this_plot <- ggplot(methods.data, aes(x=runtime, y=accuracy)) + geom_point(aes(size = 4,color="red")) + geom_text_repel(aes(label=methodcode),hjust=-0.1, vjust=0,size=6,force=5) + 
  theme(legend.position="none") + labs(y = "accuracy [%]",x="runtime [s]")

plot(this_plot)
#save
loc <- paste(getwd(),"/Report/Final_Report/Figures/Budworms/ConfusionMatrices/",Sys.Date(),"-ScatterPerformance.png",sep="")
ggsave(plot=this_plot,loc,h=4,w=6,type="cairo-png")
