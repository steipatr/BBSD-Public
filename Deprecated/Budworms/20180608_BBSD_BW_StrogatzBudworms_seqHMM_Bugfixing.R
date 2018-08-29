library("seqHMM")
library("feather")
library("infotheo")
library("ggplot2")
library("reshape2")
#library("TraMineR")
#.........................................................................
#specify data files

file_location <- "C:/Users/Patrick/Documents/GitHub/BBSD/Budworms/Data/"
data_date <- "2018-05-31"
outcomes_name <- "x"
experiments_name <- "Experiments"

path_outcomes <- paste(file_location, data_date,outcomes_name,".feather", sep="")
path_experiments <- paste(file_location, data_date,experiments_name,".feather", sep="")

#.........................................................................
#load data
df_outcomes <- read_feather(path_outcomes)

#.........................................................................
df_hmm <- df_outcomes

bins <- ceiling(1 + 3.322 * log(nrow(df_hmm))) #Sturges rule
df_hmm_disc <- discretize(df_hmm, disc="globalequalwidth", nbins=bins)

data_hmm.seq <- seqdef(df_hmm_disc)

set.seed(2) #1 causes "Backward probabilities contain non-finite values.". 2 works for five bins.
lc_model <- build_lcm(data_hmm.seq, n_clusters = 3)
lcm_fit <- fit_model(lc_model )

logLik(lc_model)

??logLik

this <- summary(lcm_fit$model)$most_probable_cluster

levels(this) <- c("1","2","3")
numthis <- as.numeric(levels(this))[this]
print(numthis)


#viz-----------------------
df_outcomes_id <- cbind(expt_id=expt_id, df_outcomes)
df_outcomes_id_long <- melt(df_outcomes_id,variable.name="timestep")

ggplot(data=df_outcomes_id_long, aes(x=timestep, y=value,colour=rep(this,ncol(df_outcomes)))) + 
  geom_line(aes(group=expt_id)) +
  labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100))


"expt_id <- as.character(1:nrow(df_hmm_disc))
df_hmm_disc_id <- cbind(expt_id=expt_id, df_hmm_disc)
df_hmm_disc_id_long <- melt(df_hmm_disc_id,variable.name="timestep")

ggplot(data=df_hmm_disc_id_long, aes(x=timestep, y=value * (1/bins) * max(df_outcomes, na.rm=TRUE))) + geom_line(aes(group=expt_id,colour=expt_id)) + 
  theme(legend.position="none") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100))
"