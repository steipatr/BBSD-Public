library("feather")
library("dtwclust")

#load data generated in Python with feather
df_outcomes <- read_feather(paste(getwd(),"/Budworms/Data/2018-05-31x.feather",sep=""))

data_dtwclust <- TADPole(df_outcomes, window.size=20L, k = 3L, dc=10L, error.check = TRUE)

print(data_dtwclust$cl)
