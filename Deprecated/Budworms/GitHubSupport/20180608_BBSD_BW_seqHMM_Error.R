library("feather")
library("infotheo")
library("seqHMM")

#load data generated in Python with feather
df_outcomes <- read_feather(paste(getwd(),"/Budworms/Data/2018-05-31x.feather",sep=""))

df_hmm <- df_outcomes

#discretize data using infotheo
bins <- 5
df_hmm_disc <- discretize(df_hmm, disc="globalequalwidth", nbins=bins)

#build LCM using seqHMM
data_hmm.seq <- seqdef(df_hmm_disc)
set.seed(2)
"
seed example values:
seed=1 and bins=5 gives -EM algorithm failed: Backward probabilities contain non-finite values.-. 
seed=2 and bins=5 works, but not for other bin counts (e.g. 19).
seed=9087 and bins=5 gives -EM algorithm failed: Estimation of gamma coefficients failed due to non-finite cluster probabilities.-.
"

lc_model <- build_lcm(data_hmm.seq, n_clusters = 3)
lcm_fit <- fit_model(lc_model )

#get clusters
clusters <- summary(lcm_fit$model)$most_probable_cluster
print(clusters)

#model seems functional? log likelihood =/= NaN, inf or 0
logLik(lc_model)

