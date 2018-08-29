df_hmm <- df_outcomes
k <- 3

require("seqHMM")
require("infotheo")

#create df for logging BIC
bmax <- 6
bmin <- 2

matrix_bic <- matrix(ncol=2,nrow=bmax-bmin+1)
df_bic <- data.frame(matrix_bic)
colnames(df_bic) <- c("bins","BIC")

#seqHMM building/fitting can fail for some random seed values. Repeat up to 20 times until it works.

for (b in bmin:bmax){
  
  #discretize time series df
  df_hmm_disc <- discretize(df_hmm, disc="globalequalwidth", nbins=b)
  
  #convert into sts object per package specs
  data_hmm.seq <- seqdef(df_hmm_disc)
  
  #search for and fit hmm model
  lcm_fit <- NULL
  attempt <- 0
  while( is.null(lcm_fit) && attempt < 20){
    attempt <- attempt + 1
    #set.seed(attempt)
    
    lc_model <- build_lcm(data_hmm.seq, n_clusters = k)
    
    tryCatch({
      lcm_fit <- fit_model(lc_model,log_space = TRUE)
    }, error=function(e){})
    
    if (!is.null(lcm_fit)) {

      print(paste("lcm fitting successful:",b,"->",summary(lcm_fit$model)$BIC))
      df_bic[b-1,"bins"] <- b
      df_bic[b-1,"BIC"] <- summary(lcm_fit$model)$BIC
      #list_lcm[b-1] <- lcm_fit
      }
  }
}

#find best number of bins/states based on BIC
df_bic
corrbin <- df_bic[which.min(df_bic[,"BIC"]),"bins"]
corrbin

#re-run with best bin count

#discretize time series df
df_hmm_disc <- discretize(df_hmm, disc="globalequalwidth", nbins=corrbin)

#convert into sts object per package specs
data_hmm.seq <- seqdef(df_hmm_disc)

#search for and fit hmm model
lcm_fit <- NULL
attempt <- 0
while( is.null(lcm_fit) && attempt < 20){
  attempt <- attempt + 1
  #set.seed(attempt)
  
  lc_model <- build_lcm(data_hmm.seq, n_clusters = k)
  
  tryCatch({
    lcm_fit <- fit_model(lc_model,log_space = TRUE)
  }, error=function(e){})
  
  if (!is.null(lcm_fit)) {
    
    if (!is.null(lcm_fit)) {print("lcm clustering successful.")}

  }
}

#get and reformat most probable clusters
factor_clusters <- summary(lcm_fit$model)$most_probable_cluster
levels(factor_clusters) <- c(1:k)
clusters <- as.numeric(levels(factor_clusters))[factor_clusters]

clusters

