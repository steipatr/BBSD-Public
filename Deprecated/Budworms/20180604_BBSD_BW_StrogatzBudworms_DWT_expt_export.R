library(TSclust)
library(feather)
library(ggplot2)
library("gridExtra")
library(reshape2)
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
head(df_outcomes)
df_outcomes_work <- df_outcomes[] #[1:10,1:50]
print(dim(df_outcomes_work) )

matrix_outcomes <- data.matrix(df_outcomes_work, rownames.force = NA)
print(dim(matrix_outcomes))

#.......................
#get distances between time series
DWT_dist <- diss( matrix_outcomes, "DWT") 

#cluster around k medoids
DWTclusters <- pam( DWT_dist, 3 )
print(DWTclusters$clustering)

#-----------------------
#visualize as geom_lines with clusters as colors

expt_id <- as.character(1:nrow(df_outcomes_work))
df_outcomes_id_work <- cbind(expt_id=expt_id, df_outcomes_work)

df_outcomes_id_work_long <- melt(df_outcomes_id_work,variable.name="timestep")

#build title
grid_title <- paste(basename(path_outcomes),"/expts:",nrow(df_outcomes),"/dist func:", DWTclusters$call$x, "/centroid:", attributes(DWTclusters)$class[1])

grob_lines_outcomes <- ggplot(data=df_outcomes_id_work_long, aes(x=timestep, y=value,colour=as.factor(rep(DWTclusters$clustering,ncol(df_outcomes))))) + 
  geom_line(aes(group=expt_id)) +
  labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
  labs(title = grid_title)

#.........................................................................
#load experiments and generate plot
df_expt <- read_feather(path_experiments)

#add column of clusters
df_expt_clusters <- cbind(Clusters=as.factor(DWTclusters$clustering), df_expt)

grob_parcoord_expt <- ggparcoord(data=df_expt_clusters,columns=2:(ncol(df_expt_clusters)),groupColumn = "Clusters")

#.........................................................................
#find input subspaces
#print(df_expt_clusters[df_expt_clusters$Clusters==1,]) #find inputs that generate specific cluster

#build dataframe
subspaces <- data.frame(Clusters=1:max(DWTclusters$clustering))
#print(subspaces)

#make dataframe true to size
for (i in 1:(ncol(df_expt))){
  placeholder <- c(rep(NA,max(DWTclusters$clustering)))
  #print(placeholder)
  subspaces <- cbind(subspaces,placeholder)
  
}
colnames(subspaces) <- c("Clusters", colnames(df_expt))
#print(subspaces)

#find min and max for each cluster and column/input, concatenate into string, add to subspaces df

#TODO improve with grep or sapply?
for (clust in 1:max(DWTclusters$clustering)){
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

#export experiments to feather for Python viz
#print(df_expt_clusters)
path_expt_export <- paste(file_location, "df_expt_clusters_test",".feather", sep="")
write_feather(df_expt_clusters, path_expt_export)
