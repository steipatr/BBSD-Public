#.........................................................................
#specify data files

file_location <- "C:/Users/Patrick/Documents/GitHub/BBSD/Budworms/Data/"
data_date <- "2018-05-31"
outcomes_name <- "x"
experiments_name <- "Experiments"

path_outcomes <- paste(file_location, data_date,outcomes_name,".feather", sep="")
#path_experiments <- paste(file_location, data_date,experiments_name,".feather", sep="")

#.........................................................................
#load outcomes and generate plot
df_outcomes <- read_feather(path_outcomes)
head(df_outcomes)
df_outcomes_work <- df_outcomes[1:100,1:400] #[1:10,1:50]
print(dim(df_outcomes_work) )

matrix_outcomes <- data.matrix(df_outcomes_work, rownames.force = NA)
print(dim(matrix_outcomes))

#.......................
#get distances between time series
dtwarp_dist <- diss( matrix_outcomes, "DTWARP") 

#cluster around k medoids
dtwcluster <- pam( dtwarp_dist, 2 )
print(dtwcluster$clustering)
