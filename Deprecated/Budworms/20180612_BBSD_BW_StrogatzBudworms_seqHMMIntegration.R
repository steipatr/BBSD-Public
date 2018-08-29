#adjust working directory
getwd()
setwd("./GitHub/BBSD")
getwd()

#load functions
source("20180612_BBSD_R_Functions.R")

#load libraries
library("feather")
library("Cairo")
library("caret")

#load data
file_location <- "C:/Users/Patrick/Documents/GitHub/BBSD/Budworms/Data/"
data_date <- "2018-06-09"
outcomes_name <- "x"
experiments_name <- "Experiments"

path_outcomes <- paste(file_location, data_date,outcomes_name,".feather", sep="")
path_experiments <- paste(file_location, data_date,experiments_name,".feather", sep="")

df_outcomes <- read_feather(path_outcomes)

#test discretization for LCM
#build NA matrix of correct size
bmin <- 2
bmax <- 13


matrix_lcm <- matrix(ncol=bmax-bmin+1,nrow=nrow(df_outcomes))
df_lcm <- data.frame(matrix_lcm)
colnames(df_lcm) <- bmin:bmax
head(df_lcm)

for (b in bmin:bmax){
  print(b)
  df_lcm[,b-1] <- get.seqHMM.clusters(df_outcomes,"LCM",k=3,b)
  
}

#df_lcm5 <- get.seqHMM.clusters(df_outcomes,"LCM",k=3,b=5)
#df_lcm19 <- get.seqHMM.clusters(df_outcomes,"LCM",k=3,b=19)

#df_lcm <- data.frame(df_lcm5,df_lcm19)
head(df_lcm)

df_lcm <- match.clusters(df_lcm,"2",logging=TRUE)

#plot and export clustering solutions
for (c in 1:ncol(df_lcm)){
  this_plot <- plot.timeseries.clustered(df_outcomes,df_lcm[,c],paste("LCM, bins:",c))
  loc <- paste(getwd(),"/Budworms/Plots/2018-06-12/LCM/",Sys.Date(),"-",colnames(df_lcm)[c],"bins.png",sep="")
  png(loc,height=6,width=12,units="cm",res=300,type="cairo")
  print(this_plot)
  dev.off()
  #print(paste(mthd,"done."))
}

colnames()