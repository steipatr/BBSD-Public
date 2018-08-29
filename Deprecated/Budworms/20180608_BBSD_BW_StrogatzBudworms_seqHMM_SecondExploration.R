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
#create test data
t0 <- c(1,1,1,1,1)
t1 <- c(1.4,2.7,1.3,3.1,1.3)
t2 <- c(3.2,4.2,1.2,2.4,4.4)
t3 <- c(3.6,4.1,2.1,3.1,3.7)

df_dt <- data.frame(t0,t1,t2,t3)
print(df_dt)
print(typeof(df_dt))

#.........................................................................
#load data
df_outcomes <- read_feather(path_outcomes)

#.........................................................................
#generate plot
df_hmm <- df_outcomes
print(typeof(df_hmm))
head(df_hmm)

bins <- 1 + 3.322 * log(nrow(df_hmm)) #Sturges rule
df_hmm_disc <- discretize(as.data.frame(df_hmm), disc="globalequalwidth", nbins=bins)



head(df_hmm_disc)

expt_id <- as.character(1:nrow(df_hmm_disc))
df_hmm_disc_id <- cbind(expt_id=expt_id, df_hmm_disc)
df_hmm_disc_id_long <- melt(df_hmm_disc_id,variable.name="timestep")

ggplot(data=df_hmm_disc_id_long, aes(x=timestep, y=value * (1/bins) * max(df_outcomes, na.rm=TRUE))) + geom_line(aes(group=expt_id,colour=expt_id)) + 
  theme(legend.position="none") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100))

data_hmm.seq <- seqdef(df_hmm_disc)

set.seed(42)
model <- build_lcm(data_hmm.seq, n_clusters = 3)
fit <- fit_model(model)

this <- summary(fit$model)$most_probable_cluster
print(this)

levels(this) <- c("1","2","3")
print(this)

numthis <- as.numeric(levels(this))[this]

print(typeof(numthis))

#viz-----------------------
df_outcomes_id <- cbind(expt_id=expt_id, df_outcomes)
df_outcomes_id_long <- melt(df_outcomes_id,variable.name="timestep")

ggplot(data=df_outcomes_id_long, aes(x=timestep, y=value,colour=rep(this,ncol(df_outcomes)))) + 
  geom_line(aes(group=expt_id)) +
  labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100))
