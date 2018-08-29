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
file_location <- "C:/Users/Patrick/Documents/GitHub/BBSD/Brusselator/Data/"
data_date <- "2018-07-09"
outcomes_name <- "y"
experiments_name <- "Experiments"

path_outcomes <- paste(file_location, data_date,outcomes_name,".feather", sep="")
path_experiments <- paste(file_location, data_date,experiments_name,".feather", sep="")

df_outcomes <- read_feather(path_outcomes)
df_expts_raw <- read_feather(path_experiments)

#compare cluster count possibilities across all methods
#("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD","LCM")
cl_methods <- c("CID")

#run LCM first because of high chance of failing to converge
df_cl2 <- get.clusters.df(df_outcomes,cl_methods,2)
head(df_cl2)

#add true column from input data
df_expts <- read_feather(path_experiments)

head(df_expts)

#create true cluster column
df_expts$cl<-NA
df_expts$cl[df_expts$'b0' < (1+(df_expts$'a')^2)]<-1 #fixed point
df_expts$cl[df_expts$'b0' > (1+(df_expts$'a')^2)]<-2 #Hopf bifurcation/oscillating

#add column to df_cl2c
df_cl2$truecl <- df_expts$cl
head(df_cl2)

#match cluster indices on true (for k=2))
df_cl2m <- match.clusters(df_cl2,"truecl",logging=TRUE)
head(df_cl2m)


#find input subspaces for a given method
#add cluster memberships to experiments df
df_expts_cl <- df_expts_raw
df_expts_cl$cloi <- df_cl2m$CID #cluster of interest
head(df_expts_cl)


#build dataframe to hold results
k <- max(df_cl2m)
print(k)
di <- ncol(df_expts_raw)
print(di)

ma_ss <- matrix(nrow=k,ncol=di+1)

df_ss <- as.data.frame(ma_ss)

colnames(df_ss) <- colnames(df_expts_cl)
head(df_ss)

df_ss$cloi <- c(1:k)
head(df_ss)

#export clustering solution with feather for PRIM in Python
#write_feather(df, path)
clustersolutions_name <- "CID_True"
path_clusters <- paste(file_location, data_date,clustersolutions_name,".feather", sep="")
write_feather(df_cl2m,path_clusters)

#subspace limits all copied from EMA Workbench in Python
#plot induced boxes with CID-found memberships
SubspacesCID_PRIM <- ggplot(df_expts_cl, aes(b0,a))+geom_point(aes(color=as.factor(cloi))) + 
  labs(color="Cluster") +
  geom_rect(aes(xmin = 1.02, xmax = 4.08, ymin = 0.56, ymax = 1.98),color = 'darkred', alpha=0) +
  geom_rect(aes(xmin = 2.41, xmax = 4.98, ymin = 0.21, ymax = 1.11),color = 'darkblue', alpha=0)

  
plot(SubspacesCID_PRIM)

saveloc <- paste(getwd(),"/Report/Report/Figures/Brusselator/Subspaces/",Sys.Date(),"-CID_PRIM.png",sep="")
ggsave(plot = SubspacesCID_PRIM, saveloc, h = 4, w = 6, type = "cairo-png")

#plot induced boxes with CID-found memberships
SubspacesCID_PRIM_True <- ggplot(df_expts_cl, aes(b0,a))+geom_point(aes(color=as.factor(df_cl2$truecl))) + 
  labs(color="Cluster") +
  geom_rect(aes(xmin = 1.02, xmax = 4.08, ymin = 0.56, ymax = 1.98),color = 'darkred', alpha=0) +
  geom_rect(aes(xmin = 2.41, xmax = 4.98, ymin = 0.21, ymax = 1.11),color = 'darkblue', alpha=0)


plot(SubspacesCID_PRIM_True)

saveloc <- paste(getwd(),"/Report/Report/Figures/Brusselator/Subspaces/",Sys.Date(),"-CID_PRIM_True.png",sep="")
ggsave(plot = SubspacesCID_PRIM_True, saveloc, h = 4, w = 6, type = "cairo-png")

brussfun <- function(x) sqrt(x-1)

SubspacesCID_PRIM_TrueCurve <- ggplot(df_expts_cl, aes(b0,a))+geom_point(aes(color=as.factor(df_cl2$truecl))) + 
  labs(color="Cluster") +
  geom_rect(aes(xmin = 1.02, xmax = 4.08, ymin = 0.56, ymax = 1.98),color = 'darkred', alpha=0) +
  geom_rect(aes(xmin = 2.41, xmax = 4.98, ymin = 0.21, ymax = 1.11),color = 'darkblue', alpha=0) +
  geom_area(stat="function", fun=brussfun,fill="turquoise",alpha=0.2) +
  coord_cartesian(ylim = c(0.2,2)) 


plot(SubspacesCID_PRIM_TrueCurve)


saveloc <- paste(getwd(),"/Report/Report/Figures/Brusselator/Subspaces/",Sys.Date(),"-CID_PRIM_TrueCurve.png",sep="")
ggsave(plot = SubspacesCID_PRIM_TrueCurve, saveloc, h = 4, w = 6, type = "cairo-png")







#old stuff----------------


saveloc <- paste(getwd(),"/Report/Report/Figures/Brusselator/Subspaces/",Sys.Date(),"-CID_PRIM.png",sep="")
ggsave(plot = SubspacesCID_PRIM, saveloc, h = 4, w = 6, type = "cairo-png")

df_poly1 <- data.frame(x=c(0.45,r_max_cusp,r_max_cusp,0.45), y=c(0.1,0.1,0.4,0.4)) #red, left
df_poly2 <- data.frame(x=c(r_max_cusp,0.7,0.7,r_max_cusp), y=c(0.1,0.1,(0.7 - drift_cusp),(r_max_cusp - drift_cusp))) #green, top right
df_poly3 <- data.frame(x=c(r_max_cusp,0.7,0.7,r_max_cusp), y=c((r_max_cusp - drift_cusp),(0.7 - drift_cusp),0.4,0.4)) #blue, bottom right
dF_polyclrs <- data.frame(c("red","blue","green"))

#improve with https://stackoverflow.com/questions/39739327/add-a-legend-for-geom-polygon
#geom_point(aes(color=as.factor(df_cl3$truecl)))
#geom_point(color="grey",size=1)
SubspacesCID_PRIM_True <- ggplot(df_expts_cl, aes(r0,rstep))+geom_point(aes(color=as.factor(df_cl3$truecl)),alpha=0.2) + 
  labs(color="Cluster") +
  geom_rect(aes(xmin = 0.450947, xmax = 0.572623, ymin = -Inf, ymax = Inf),color = 'darkred', alpha=0) +
  geom_rect(aes(xmin = 0.576196, xmax = 0.6879, ymin = 0.269450, ymax = 0.3987),color = 'darkgreen', alpha=0) +
  geom_rect(aes(xmin = 0.580144, xmax = 0.699804, ymin = 0.100926, ymax = 0.299325),color = 'darkblue', alpha=0) +
  geom_polygon(data=df_poly1,aes(x,y),fill="red",alpha=0.2) + 
  geom_polygon(data=df_poly2,aes(x,y),fill="blue",alpha=0.2) + 
  geom_polygon(data=df_poly3,aes(x,y),fill="green",alpha=0.2)

plot(SubspacesCID_PRIM_True)

saveloc <- paste(getwd(),"/Report/Report/Figures/Budworms/Subspaces/",Sys.Date(),"-CID_PRIMinTrue.png",sep="")
ggsave(plot = SubspacesCID_PRIM_True, saveloc, h = 4, w = 6, type = "cairo-png")

#scatter plot to show PRIM challenge
#SubspacesCID <- ggplot(df_expts_cl, aes(r0,rstep))+geom_point(aes(color=as.factor(cloi))) + labs(color="Cluster")
#loc <- paste(getwd(),"/Report/Report/Figures/Budworms/Subspaces/",Sys.Date(),"-CID.png",sep="")
#SubspacesTrue <- ggplot(df_expts_cl, aes(r0,rstep))+geom_point(aes(color=as.factor(df_cl3$truecl))) + labs(color="Cluster")
#loc <- paste(getwd(),"/Report/Report/Figures/Budworms/Subspaces/",Sys.Date(),"-True.png",sep="")

#export clustering solutions to feather
#write_feather(df, path)
clustersolutions_name <- "CID_True"
path_clusters <- paste(file_location, data_date,clustersolutions_name,".feather", sep="")
write_feather(df_cl3m,path_clusters)
