library("seqhmm")
library("feather")
library("arules")
library("infotheo")
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

#.........................................................................
#load data
df_outcomes <- read_feather(path_outcomes)

#.........................................................................
#generate plot
df_hmm <- df_outcomes

bins <- 1 + 3.322 * log(nrow(df_hmm)) #Sturges rule
df_hmm_disc <- discretize(df_hmm, disc="globalequalwidth", nbins=bins)
#print(ceiling(2*(nrow(df_hmm)^(1/3))))
head(df_hmm_disc)

expt_id <- as.character(1:nrow(df_hmm_disc))
df_hmm_disc_id <- cbind(expt_id=expt_id, df_hmm_disc)
df_hmm_disc_id_long <- melt(df_hmm_disc_id,variable.name="timestep")

ggplot(data=df_hmm_disc_id_long, aes(x=timestep, y=value * (1/bins) * max(df_outcomes, na.rm=TRUE))) + geom_line(aes(group=expt_id,colour=expt_id)) + 
  theme(legend.position="none") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100))

??build_lcm
