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

#compare cluster count possibilities across all methods
#("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC","DTW","SBD","GAK","TAD","LCM")
cl_methods <- c("LPC")

#run LCM first because of high chance of failing to converge
df_cl3 <- get.clusters.df(df_outcomes,cl_methods,3)
head(df_cl3)

#add true column from input data
df_expts_raw <- read_feather(path_experiments)
df_expts <- df_expts_raw

#for given k value, set r_max and drift limits
r_max_cusp <- 0.559525 #for k=10
drift_cusp <- 0.383971 #for k=10

#create true cluster column
df_expts$truecl<-NA
df_expts$truecl[df_expts$'r0'<r_max_cusp]<-1 #always at refuge level
df_expts$truecl[df_expts$'r0'>r_max_cusp & df_expts$'r0' - df_expts$'rstep'<drift_cusp]<-2 #drop from outbreak to refuge
df_expts$truecl[df_expts$'r0'>r_max_cusp & df_expts$'r0' - df_expts$'rstep'>drift_cusp]<-3 #stay at outbreak

#add column to df_cl3c
df_cl3$truecl <- df_expts$truecl

#match cluster indices on true (for k=3))
df_cl3m <- match.clusters(df_cl3,"truecl",logging=TRUE)
head(df_cl3m)


#find input subspaces for a given method
#add cluster memberships to experiments df
df_expts_cl <- df_expts_raw
df_expts_cl$cloi <- df_cl3m$LPC #cluster of interest
head(df_expts_cl)


#build dataframe to hold results
k <- max(df_cl3m)
print(k)
di <- ncol(df_expts_raw)
print(di)

ma_ss <- matrix(nrow=k,ncol=di+1)

df_ss <- as.data.frame(ma_ss)

colnames(df_ss) <- colnames(df_expts_cl)
head(df_ss)

df_ss$cloi <- c(1:k)
head(df_ss)

#find min and max for each cluster and column/input, concatenate into string, add to subspaces df

#TODO improve with grep or sapply?
for (cl in 1:k){
  for (d in 1:di){
    print(paste("cl",cl,"/d:",d))
    
    #bar <- subset(foo, location == "there")
    df_expts_cl_subset <- subset(df_expts_cl, cloi == cl)
    #print(head(df_expts_cl_subset))
    mini <- min(df_expts_cl_subset[,d])
    maxi <- max(df_expts_cl_subset[,d])
    print(paste("min:",round(mini,2),"//max:",round(maxi,2)))
    
    df_ss[cl,d] <- paste(round(mini,2),"-",round(maxi,2),sep="")
    
  }
}

#subspace limits all copied from EMA Workbench in Python
SubspacesLPC_PRIM <- ggplot(df_expts_cl, aes(r0,rstep))+geom_point(aes(color=as.factor(cloi))) + 
  labs(color="Cluster",x="r",y="rstep") +
  geom_rect(aes(xmin = 0.499835, xmax = 0.616625, ymin = -Inf, ymax = Inf),color = 'darkred', alpha=0) +
  geom_rect(aes(xmin = 0.566487, xmax = 0.595297, ymin = 0.132130, ymax = 0.398700),color = 'darkgreen', alpha=0) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.100926, ymax = 0.284403),color = 'darkblue', alpha=0)
  
plot(SubspacesLPC_PRIM)

saveloc <- paste(getwd(),"/Report/Final_Report/Figures/Budworms/Subspaces/",Sys.Date(),"-LPC_PRIM.png",sep="")
ggsave(plot = SubspacesLPC_PRIM, saveloc, h = 4, w = 6, type = "cairo-png")

df_poly1 <- data.frame(x=c(0.45,r_max_cusp,r_max_cusp,0.45), y=c(0.1,0.1,0.4,0.4)) #red, left
df_poly2 <- data.frame(x=c(r_max_cusp,0.7,0.7,r_max_cusp), y=c(0.1,0.1,(0.7 - drift_cusp),(r_max_cusp - drift_cusp))) #green, top right
df_poly3 <- data.frame(x=c(r_max_cusp,0.7,0.7,r_max_cusp), y=c((r_max_cusp - drift_cusp),(0.7 - drift_cusp),0.4,0.4)) #blue, bottom right
dF_polyclrs <- data.frame(c("red","blue","green"))

#improve with https://stackoverflow.com/questions/39739327/add-a-legend-for-geom-polygon
#geom_point(aes(color=as.factor(df_cl3$truecl)))
#geom_point(color="grey",size=1)
SubspacesLPC_PRIM_True <- ggplot(df_expts_cl, aes(r0,rstep))+geom_point(aes(color=as.factor(cloi)),alpha=0.2) + 
  labs(color="Cluster",x="r",y="rstep") +
  geom_rect(aes(xmin = 0.499835, xmax = 0.616625, ymin = -Inf, ymax = Inf),color = 'darkred', alpha=0) +
  geom_rect(aes(xmin = 0.566487, xmax = 0.595297, ymin = 0.132130, ymax = 0.398700),color = 'darkgreen', alpha=0) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.100926, ymax = 0.284403),color = 'darkblue', alpha=0) +
  geom_polygon(data=df_poly1,aes(x,y),fill="red",alpha=0.2) + 
  geom_polygon(data=df_poly2,aes(x,y),fill="blue",alpha=0.2) + 
  geom_polygon(data=df_poly3,aes(x,y),fill="green",alpha=0.2)

plot(SubspacesLPC_PRIM_True)

saveloc <- paste(getwd(),"/Report/Final_Report/Figures/Budworms/Subspaces/",Sys.Date(),"-LPC_PRIMinTrue.png",sep="")
ggsave(plot = SubspacesLPC_PRIM_True, saveloc, h = 4, w = 6, type = "cairo-png")

#export clustering solutions to feather
#write_feather(df, path)
clustersolutions_name <- "LPC_True"
path_clusters <- paste(file_location, data_date,clustersolutions_name,".feather", sep="")
write_feather(df_cl3m,path_clusters)
