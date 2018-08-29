library("feather")
library("reshape2")
library("ggplot2")
library("GGally")
library("dtwclust")
library("gridExtra")

#.........................................................................
#specify data files

file_location <- "C:/Users/Patrick/Documents/GitHub/BBSD/Budworms/Data/"
data_date <- "2018-05-31"
outcomes_name <- "x"
experiments_name <- "Experiments"

path_outcomes <- paste(file_location, data_date,outcomes_name,".feather", sep="")
path_experiments <- paste(file_location, data_date,experiments_name,".feather", sep="")

#.........................................................................
#load outcomes and generate plot
df_outcomes <- read_feather(path_outcomes)

PartTSClusters <- tsclust(df_outcomes, type = "partitional", k = 3L, 
                distance = "gak", centroid = "shape", 
                seed = 3247L, trace = TRUE,
                args = tsclust_args(dist = list(window.size = 20L)))

expt_id <- as.character(1:nrow(df_outcomes))
df_outcomes_id <- cbind(expt_id=expt_id, df_outcomes)

df_outcomes_id_long <- melt(df_outcomes_id,variable.name="timestep")

#build title
grid_title <- paste(basename(path_outcomes),"/expts:",nrow(df_outcomes),"/dist func:", PartTSClusters@distance, "/centroid:", PartTSClusters@centroid)

grob_lines_outcomes <- ggplot(data=df_outcomes_id_long, aes(x=timestep, y=value,colour=as.factor(rep(PartTSClusters@cluster,ncol(df_outcomes))))) + 
  geom_line(aes(group=expt_id)) +
  labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
  labs(title = paste(grid_title))

#outcome_clusters <- plot(PartTSClusters)

#.........................................................................
#load experiments and generate plot
df_expt <- read_feather(path_experiments)

#add column of clusters
df_expt_clusters <- cbind(Clusters=as.factor(PartTSClusters@cluster), df_expt)

grob_parcoord_expt <- ggparcoord(data=df_expt_clusters,columns=2:(ncol(df_expt_clusters)),groupColumn = "Clusters")

#.........................................................................
#find input subspaces
#print(df_expt_clusters[df_expt_clusters$Clusters==1,]) #find inputs that generate specific cluster

#build dataframe
subspaces <- data.frame(Clusters=1:PartTSClusters@k)
#print(subspaces)

#make dataframe true to size
for (i in 1:(ncol(df_expt))){
  placeholder <- c(rep(NA,PartTSClusters@k))
  #print(placeholder)
  subspaces <- cbind(subspaces,placeholder)
  
}
colnames(subspaces) <- c("Clusters", colnames(df_expt))
#print(subspaces)

#find min and max for each cluster and column/input, concatenate into string, add to subspaces df

#TODO improve with grep or sapply?
for (clust in 1:PartTSClusters@k){
  for (inp in 2:ncol(df_expt_clusters)){
    mini <- min(df_expt_clusters[df_expt_clusters$Clusters==clust,inp])
    maxi <- max(df_expt_clusters[df_expt_clusters$Clusters==clust,inp])
    #print(paste("Cluster",clust,"/",colnames(df_expt_clusters)[inp],":",round(mini,2),"-",round(maxi,2)))
    subspaces[clust,inp] <- paste(round(mini,2),"-",round(maxi,2),sep="")
  }
}

grob_subspaces <- tableGrob(subspaces,rows=NULL, theme=ttheme_default(base_size=10,padding = unit(c(2, 2), "mm")))



#.........................................................................
#combine plots into one and show
#grid.arrange(outcome_lines, outcome_clusters, expt_paracoords, ncol = 1)
grid.arrange(grob_lines_outcomes, grob_parcoord_expt,grob_subspaces, ncol = 1)

