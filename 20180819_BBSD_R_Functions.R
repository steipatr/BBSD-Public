#-------------------------
#allowed clustering methods
TSclust_methods <- c("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC")
dtwclust_methods <- c("DTW","SBD","GAK","TAD")
seqHMM_methods <- c("LCM")


#-------------------------
#plotting functions

#main generic function
get.clusters <- function(df_ts,method,k) {
  "takes df of time series, clustering method (see list of implemented choices) and desired number of clusters 
  implemented and tested:
    TSclust: 
      CORT (proximity on observations and temporal correlation)
      ACF (autocorrelation functions-based distance)
      PER (periodogram-based distance, similar to fourier transform)
      DWT (discrete wavelet transform-based distance)
      PIC (ARIMA coefficients-based distance)
    dtwclust: 
      DTW (basic dynamic time warping, uses partition around medoids)
      SBD (shape-based distance, uses specific centroid)
      GAK (global alignment kernels, uses specific centroid)
      TAD (TADPole clustering, uses specific centroid)
      SDTW (soft dynamic time warping, uses partition around medoids)
    seqHMM: 
      LCM (single latent class model using mixture hidden Markov)
  "

  #find and load right method
  if (method %in% TSclust_methods) {
    return(get.TSclust.clusters(df_ts,method,k))
    
  } else if (method %in% dtwclust_methods) {
    return(get.dtwclust.clusters(df_ts,method,k))
    
  } else if (method %in% seqHMM_methods) {
    return(get.seqHMM.clusters(df_ts,method,k,b=NULL))
    
  } else {
    return("No method match found.")
  }
}


get.TSclust.clusters <- function(df_TS,method,k,raw=FALSE) {
  
  require("TSclust")
  
  #aliasing
  if (method == "PIC") {
    method <- "AR.PIC"
  }
  if (method == "LPC") {
    method <- "AR.LPC.CEPS"
  }
  if (method == "MAH") {
    method <- "AR.MAH"
  }
  if (method == "FRE") {
    method <- "FRECHET"
  }
  if (method == "SAX") {
    method <- "MINDIST.SAX"
  }
  if (method == "GLK") {
    method <- "SPEC.GLK"
  }
  if (method == "LLR") {
    method <- "SPEC.LLR"
  }
  if (method == "ISD") {
    method <- "SPEC.ISD"
  }
  
  #special case for PDC
  if (method == "PDC"){
    df_pdc <- df_TS
    matrix_TS <- data.matrix(df_pdc, rownames.force = NA)
    tmatrix_TS <- t(matrix_TS)
    
    m <- mapply(tmatrix_TS, FUN=as.numeric)
    numtmatrix_TS <- matrix(data=m, ncol=ncol(tmatrix_TS), nrow=nrow(tmatrix_TS))
    
    TSclust_dist <- pdcDist(X=numtmatrix_TS,m=k,t=5)
    data_TSclust <- pam(TSclust_dist,k)
    
    #include ability to get raw data (for silhouette)
    if (raw==TRUE){return(data_TSclust)}
    
    #get clusters
    clusters <- data_TSclust$clustering
    
    return(clusters)
  }
  
  
  #reformat df into matrix
  matrix_TS <- data.matrix(df_TS, rownames.force = NA)
  
  #get distances between time series
  TSclust_dist <- diss(matrix_TS, method)
  
  #partition around medoids
  data_TSclust <- pam(TSclust_dist, k )
  
  #include ability to get raw data (for silhouette)
  if (raw==TRUE){return(data_TSclust)}
  
  #get clusters
  clusters <- data_TSclust$clustering
  
  return(clusters)
}








get.dtwclust.clusters <- function(df_dtw,method,k,raw=FALSE) {
  
  require("dtwclust")
  require("reshape2")
  
  #set distance, centroid methods and type
  if (method == "DTW") {
    distance <- "dtw_basic"
    centroid <- "pam"
    type <- "partitional"
    
    data_dtwclust <- get.dtwclust.clusters.partitional(df_dtw,distance,centroid,k)
    
    if (raw==TRUE){return(data_dtwclust)}
    
    clusters <- data_dtwclust@cluster
    
  } else if (method == "SBD") {
    distance <- "sbd"
    centroid <- "shape"
    type <- "partitional"
    
    data_dtwclust <- get.dtwclust.clusters.partitional(df_dtw,distance,centroid,k)
    
    if (raw==TRUE){return(data_dtwclust)}
    
    clusters <- data_dtwclust@cluster
    
  } else if (method == "GAK") {
    distance <- "gak"
    centroid <- "shape"
    type <- "partitional"
    
    data_dtwclust <- get.dtwclust.clusters.partitional(df_dtw,distance,centroid,k)
    
    if (raw==TRUE){return(data_dtwclust)}
    
    clusters <- data_dtwclust@cluster
    
  } else { #TAD, special case

    #data_dtwclust <- TADPole(df_dtw, window.size=20L, k = k, dc=10L, error.check = TRUE)
    data_dtwclust <- tsclust(df_dtw, type = "tadpole", k = k,
                             control = tadpole_control(dc = 10, window.size = 20L))
    
    if (raw==TRUE){return(data_dtwclust)}
    
    clusters <- data_dtwclust@cluster
  }
  
  return(clusters)
}






#run partitional clustering
get.dtwclust.clusters.partitional <- function(df_dtw_part,d,c,k){
  
  #TODO how should repetitions be handled here?
  data_dtwclust_part <- tsclust(df_dtw_part, type = "partitional", k = k, 
                           distance = d, centroid = c, trace = TRUE,
                           args = tsclust_args(dist = list(window.size = 20L)))

  return(data_dtwclust_part)
}







get.seqHMM.clusters <- function(df_hmm,method,k,b=NULL,bmin=2,bmax=6) {
  
  require("seqHMM")
  require("infotheo")
  
  if (is.null(b)){
    
    #build df from matrix for storing Bayesian Information Criteria for bins finding
    matrix_bic <- matrix(ncol=2,nrow=bmax-bmin+1)
    df_bic <- data.frame(matrix_bic)
    colnames(df_bic) <- c("bins","BIC")
    
    #for every bins value in given range, build/fit model and get Bayesian Information Criteria, store in df
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
    
    #find best number of bins, re-run
    best_b <- df_bic[which.min(df_bic[,"BIC"]),"bins"]
    
    #discretize time series df
    df_hmm_disc <- discretize(df_hmm, disc="globalequalwidth", nbins=best_b)
    
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
    
    return(clusters)
    
  } else {
    
    bins <- b
    
    #discretize time series df
    df_hmm_disc <- discretize(df_hmm, disc="globalequalwidth", nbins=bins)
    
    #convert into sts object per package specs
    data_hmm.seq <- seqdef(df_hmm_disc)
    
    #seqHMM building/fitting can fail for some random seed values. Repeat up to 20 times until it works.
    lcm_fit <- NULL
    attempt <- 0
    while( is.null(lcm_fit) && attempt < 20){
      attempt <- attempt + 1
      #set.seed(attempt)
      
      lc_model <- build_lcm(data_hmm.seq, n_clusters = k)
      #print(paste("lcm fitting attempt/seed:",attempt))
      #try(
      #  lcm_fit <- fit_model(lc_model,log_space = TRUE)
      #)
      
      tryCatch({
        lcm_fit <- fit_model(lc_model,log_space = TRUE)
      }, error=function(e){})
      
      
      
      
      if (!is.null(lcm_fit)) {print("lcm fitting successful.")}
    }
    
    #get and reformat most probable clusters
    factor_clusters <- summary(lcm_fit$model)$most_probable_cluster
    levels(factor_clusters) <- c(1:k)
    clusters <- as.numeric(levels(factor_clusters))[factor_clusters]
    
    return(clusters)
    
  }

}


#get df consisting of one clustering method per column, and ts cluster identifiers as rows
get.clusters.df <- function(df_ts,methods,k){
  
  #build NA matrix of correct size
  ma_clusters <- matrix(ncol=length(methods),nrow=nrow(df_ts))
  
  print(paste("Beginning at",Sys.time()))
  
  #loop through list of methods, get clusters for each and write into matrix
  i <- 1
  for(mthd in methods){
    ma_clusters[,i] <- get.clusters(df_ts,mthd,k)
    print(paste(mthd," done at ",Sys.time(),sep=""))
    i <- i+1
  }
  
  #reformat matrix into df, add methods as column names and return
  df_clusters <- data.frame(ma_clusters)
  
  v <- methods
  d <- duplicated(v)
  for (e in 1:length(d)){
    if (d[e] == TRUE){
      v[e] <- paste(v[e],e-1,sep="")
    }
  }
  
  colnames(df_clusters) <- v
  
  return(df_clusters)
}







#rearrange cluster identifiers in one df column based on reference column
#useful to align cluster identifiers
match.clusters <- function(df_clusters,ref_col,logging=FALSE){
  
  #--------------------
  # labels from cluster A will be matched on the labels from cluster B
  #from https://www.r-bloggers.com/matching-clustering-solutions-using-the-hungarian-method/
  minWeightBipartiteMatching <- function(clusteringA, clusteringB) {
    require(clue)
    idsA <- unique(clusteringA)  # distinct cluster ids in a
    idsB <- unique(clusteringB)  # distinct cluster ids in b
    nA <- length(clusteringA)  # number of instances in a
    nB <- length(clusteringB)  # number of instances in b
    if (length(idsA) != length(idsB) || nA != nB) {
      stop("number of cluster or number of instances do not match")
    }
    
    nC <- length(idsA)
    tupel <- c(1:nA)
    
    # computing the distance matrix
    assignmentMatrix <- matrix(rep(-1, nC * nC), nrow = nC)
    for (i in 1:nC) {
      tupelClusterI <- tupel[clusteringA == i]
      solRowI <- sapply(1:nC, function(i, clusterIDsB, tupelA_I) {
        nA_I <- length(tupelA_I)  # number of elements in cluster I
        tupelB_I <- tupel[clusterIDsB == i]
        nB_I <- length(tupelB_I)
        nTupelIntersect <- length(intersect(tupelA_I, tupelB_I))
        return((nA_I - nTupelIntersect) + (nB_I - nTupelIntersect))
      }, clusteringB, tupelClusterI)
      assignmentMatrix[i, ] <- solRowI
    }
    
    # optimization
    result <- solve_LSAP(assignmentMatrix, maximum = FALSE)
    attr(result, "assignmentMatrix") <- assignmentMatrix
    return(result)
  }

  ######--------------------------------
  if (is.numeric(ref_col) == TRUE) { 
    ref_col <- colnames(df_clusters)[ref_col] 
  }
  
  methods <- colnames(df_clusters)

  for (mthd in methods){
    cl_match <- minWeightBipartiteMatching(df_clusters[,ref_col],df_clusters[,mthd])
    
    for (i in 1:length(cl_match)){
      if (logging == TRUE) {print(paste(ref_col,i,"corresponds to",mthd,cl_match[i]))}
      df_clusters[,mthd][df_clusters[,mthd]==i] <- cl_match[i] + 0.1
    }
    
    df_clusters[,mthd] <- as.integer(df_clusters[,mthd])
  }
  
  return(df_clusters)
  }





#use silhouettes to compare k choices across different methods
compare.silhouettes <- function(df_ts,methods,kmin=2,kmax=10,kstep=1,logging=FALSE){
  
  #remove non-functional elements from list of methods
  nfunc <- c("LCM")
  methods <- methods [! methods %in% nfunc]
  
  #determine number of tested clustering options
  kseq <- seq(kmin,kmax,by=kstep)
  print(kseq)
  
  #build NA matrix of correct size
  matrix_k <- matrix(ncol=(length(methods))+1,nrow=length(kseq))
  df_k <- data.frame(matrix_k)
  colnames(df_k) <- c("k",methods)
  df_k[,"k"] <- kseq
  print(head(df_k))
  
  #for each method given, find silhouette values for each k in given range
  for (i in 1:(length(methods))){
    mthd <- methods[i]
    
    if (mthd %in% TSclust_methods){
      
      #reformat df into matrix
      matrix_ts <- data.matrix(df_ts, rownames.force = NA)
      
      #loop through k's and record silhouette width, store in df
      for (k in kseq){
        data_TSclust <- get.TSclust.clusters(df_ts,mthd,k,raw=TRUE)
        silwidth <- data_TSclust$silinfo$avg.width
        if (logging==TRUE){
          print(paste(mthd,"-",k,":",silwidth,"@",Sys.time()))
          }
        #old: df_k[k-1,i+1] <- silwidth
        df_k[df_k$"k" == k,i+1] <- silwidth
        
        #df_expts$cl[df_expts$'b0' < (1+(df_expts$'a')^2)]<-1 #fixed point
      }
      
    } else if (mthd %in% dtwclust_methods){

      for (k in kseq){
        data_dtwclust <- get.dtwclust.clusters(df_ts,mthd,k,raw=TRUE)
        
        #manual method override for special case of TAD
        if (mthd == "TAD"){environment(data_dtwclust@family@dist)$distance <- "dtw_basic"}
        
        silwidth <- unname(cvi(data_dtwclust,type="Sil"))
        if (logging==TRUE){print(paste(mthd,"-",k,":",silwidth,"@",Sys.time()))}
        
        df_k[df_k$"k" == k,i+1] <- silwidth
      } 
      
      
    } else if (mthd %in% seqHMM_methods){
      
      print(paste(mthd,"not implemented yet."))
      
    } else {
      print(paste(mthd,"not recognized."))
      next()
    }
    
  }

  v <- colnames(df_k)
  nv <- v
  d <- duplicated(v) #find which are duplicates
  for (e in 1:length(d)){ #go through duplicates vector
    if (d[e] == TRUE){ #for every found duplicate...
      l <- sum(head(v,e) == v[e]) #see how many duplicates are to the left of it
      nv[e] <- paste(v[e],l,sep="") #and adjust the name accordingly
    }
  }
  colnames(df_k) <- nv
  
  return(df_k)
  
}





#find Euclidean centroids/prototypes for each cluster in a clustering solution
get.centroids <- function(df_ts,clusters){
  
  #find k
  k <- max(clusters)
  
  #create dataframe to store outputs of loop
  ma_centr <- matrix(ncol=2,nrow=k)
  df_centr <- data.frame(ma_centr)
  colnames(df_centr) <- c("cluster","centroid")
  df_centr[,"cluster"] <- 1:k
  
  #loop through cluster identifiers, find and store centroid for each
  for (i in 1:k){
    df_o <- df_ts[clusters == i,]
    p <- pam(df_o,1,metric="euclidean")
    df_centr[i,"centroid"] <- p$id.med
  }
  
  return(df_centr)
}




#-------------------------
#plotting functions

#plot unclustered ts
plot.timeseries <- function(df_ts){
  
  require("ggplot2")
  require("reshape2")

  #reformat wide into long
  expt_id <- as.character(1:nrow(df_ts))
  df_ts_id <- cbind(expt_id=expt_id, df_ts)
  df_ts_long <- melt(df_ts_id,variable.name="time_step")
  
  #build title
  title <- paste("Unclustered Time Series, n =",nrow(df_ts) )
  
  #build ggplot object
  line_plot <- ggplot(df_ts_long, aes(x=time_step, y=value)) + 
    geom_line(aes(group=expt_id,color=expt_id)) +
    scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) + theme(legend.position="none") +
    xlab("time step") + ylab("value")
  #+ggtitle(title)

  return(line_plot)
}










#plot clustered ts
plot.timeseries.clustered <- function(df_ts,clusters,title_addendum=""){
  
  require("ggplot2")
  require("reshape2")
  
  #reformat wide into long
  expt_id <- as.character(1:nrow(df_ts))
  df_ts_id <- cbind(expt_id=expt_id, df_ts)
  df_ts_long <- melt(df_ts_id,variable.name="time_step")
  
  
  #build title
  title <- paste("Clustered Time Series, n=",nrow(df_ts), ", k=",max(clusters),": ",title_addendum,sep="" )
  
  #build ggplot object
  clustered_line_plot <- ggplot(data=df_ts_long, aes(x=time_step, y=value,colour=as.factor(rep(clusters,ncol(df_ts))))) + 
    geom_line(aes(group=expt_id)) +
    labs(colour = "Clusters") + scale_x_discrete(breaks = seq(100, ncol(df_outcomes), by = 100)) +
    xlab("time step") + ylab("value")
  #ggtitle(title) +

  return(clustered_line_plot)
}









#plot clustered experiments
plot.experiments <- function(df_expt,clusters){
  
  require("ggplot2")
  require("reshape2")
  
  #add clusters to df
  df_expt_c <- cbind(Clusters=as.factor(clusters),df_expt)
  
  #build ggplot object
  clustered_parcoord_plot <- ggparcoord(data=df_expt_c,columns=2:(ncol(df_expt_c)),groupColumn = "Clusters")
  
  return(clustered_parcoord_plot)
  
}
  
  
#plot confusion matrix for two clustering methods
#adapted from https://ragrawal.wordpress.com/2011/05/16/visualizing-confusion-matrix-in-r/
plot.confusionmatrix <- function(col1,col2,external=FALSE){
  
  require("ggplot2")
  
  #generate random data 
  data <- data.frame(col1,col2)
  names(data) <- c("Actual","Predicted")
  head(data) 
  
  #compute frequency of actual categories
  actual <- as.data.frame(table(data$Actual))
  names(actual) = c("Actual","ActualFreq")
  head(actual)
  
  #build confusion matrix
  confusion = as.data.frame(table(data$Actual, data$Predicted))
  names(confusion) = c("Actual","Predicted","Freq")
  
  #calculate percentage of test cases based on actual frequency
  confusion = merge(confusion, actual, by=c("Actual"))
  confusion$Percent = confusion$Freq/confusion$ActualFreq*100
  
  #render plot
  # we use three different layers
  # first we draw tiles and fill color based on percentage of test cases
  tile <- ggplot() +
    geom_tile(aes(x=Actual, y=Predicted,fill=Percent),data=confusion, color="black",size=0.1) 
  
  #selective labelling based on whether external/gold reference is used
  if (external == TRUE){
    tile <- tile + labs(x="Actual",y="Predicted")
  } else {
    tile <- tile + labs(x="Cluster Method 1",y="Cluster Method 2")
  }
  
  tile = tile + 
    geom_text(aes(x=Actual,y=Predicted, label=sprintf("%.1f", Percent)),data=confusion, size=3, colour="black") +
    scale_fill_gradient(low="grey",high="red")
  
  # lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
  tile = tile + 
    geom_tile(aes(x=Actual,y=Predicted),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=0.3, fill="black", alpha=0) 

  return(tile)
}

#plot silhouettes
plot.silhouettes <- function(df_sils){
  #melt into long
  df_sils_long <- melt(df_sils,id.vars = c("k"),variable.name="method")
  
  #create ggplot object
  silhouettes_plot <- ggplot(df_sils_long, aes(x = k, y = value,color=method, group = method)) + geom_point() + geom_line()
  
  return(silhouettes_plot)
}