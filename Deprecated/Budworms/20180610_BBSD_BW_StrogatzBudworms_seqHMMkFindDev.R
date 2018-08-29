df_hmm <- df_outcomes
method <- "LCM"
k <- 3

require("seqHMM")
require("infotheo")

#get appropriate number of bins for discretization using Sturges' rule
bins <- 5 #ceiling(1 + 3.322 * log(nrow(df_hmm)))
#TODO set to five because it works, should be more, but causes errors

#discretize time series df
df_hmm_disc <- discretize(df_hmm, disc="globalequalwidth", nbins=bins)

#convert into sts object per package specs
data_hmm.seq <- seqdef(df_hmm_disc)

#build LCM model using appropriate clusters count
set.seed(42) #TODO how should repetitions be handled here?
lc_model <- build_lcm(data_hmm.seq, n_clusters = k)
#lcm_fit <- fit_model(lc_model)

lcm_fit <- NULL
attempt <- 0
while( is.null(lcm_fit) && attempt < 20){
  attempt <- attempt + 1
  set.seed(attempt)
  print(paste("lcm fitting attempt/seed:",attempt))
  try(
    lcm_fit <- fit_model(lc_model)
  )
  if (attempt == 20 && is.null(lcm_fit)) {print("Failed.")}
}

#get and reformat most probable clusters
factor_clusters <- summary(lcm_fit$model)$most_probable_cluster
levels(factor_clusters) <- c(1:k)
clusters <- as.numeric(levels(factor_clusters))[factor_clusters]