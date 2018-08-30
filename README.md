# BBSD-Public
## Introduction
Scenario discovery using time series clustering in R and Python. Created for my MSc thesis "Behavior-Based Scenario Discovery" at TU Delft, which is available at http://repository.tudelft.nl/.

This repository is structured into two folders, representing a simple (budworms) and complex (shale gas) example of time series clustering-based rule induction. Additionally, there is a Functions file for R which contains all the custom functions I developed. Finally, there is a third folder containing deprecated, work in progress and other auxiliary files from my research, which I provide mainly for documentation purposes related to my thesis. This folder also contains a third example, the Brusselator model, which is very similar to the budworms model and therefore is not discussed further.

## Analysis Code

For each example, a number of RStudio scripts and Jupyter Notebooks are provided. They are numbered in order of analysis and are intended to give an impression of how behavior-based scenario discovery can be conducted. The files also indicate where it is necessary to move data between R and Python, and vice-versa.

The order of analysis can be generalized as follows, and is reflected in the file numbering:
1) Create or load time series data in Python, export them from Python to R.
2) Load and explore time series data in R.
3) Investigate likely inherent cluster counts in the data in R.
4) Cluster the data using a chosen cluster count and a variety of clustering methods in R.
5) Export clustering solutions from R for Python.
6) Perform rule induction in Python.
7) Visualize induced rules in R.

## Functions File
The functions file contains a variety of custom functions I developed for this thesis. I explain them and their possible uses in the following.

### get.clusters.df <- function(df_ts,methods,k) 
Main utility of this thesis. Takes a dataframe of time series (rows are time series, columns are data points in time series), one or multiple clustering algorithms, and a cluster count `k`. It returns a dataframe (ncol = number of clustering methods, nrow = number of time series) where each column represents the hard partitional (partition around medoids) clustering solution for one of the clustering methods specified. This function calls multiple subordinate functions which are not described in detail here. The methods must exactly match one of the implemented methods. Currently, the implemented and tested methods are: 
- TSclust_methods <- c("ACF","CID","CORT","DWT","LLR","LPC","PDC","PER","PIC")
- dtwclust_methods <- c("DTW","SBD","GAK","TAD")
- seqHMM_methods <- c("LCM")

Further untested but technically implemented methods are:
- TSclust_methods <- c("MAH","FRE","SAX","ISD")


A straightforward execution might be:
```
mthds <- c("ACF","CID")
df_clusters <- get.clusters.df(df_outcomes,mthds,5)
```

As some methods are stochastically influenced, multiple iterations of the same method are possible. Columns are automatically numbered/renamed to remain distinct.
```
mthds <- c("LLR",rep("DTW",5))
df_clusters <- get.clusters.df(df_outcomes,mthds,5)
```

### match.clusters <- function(df_clusters,ref_col,logging=FALSE)
When comparing two (or more) clustering solutions, the cluster identifiers are often mismatched even if the clustering solutions are comparable, since the partitioning medoids are randomly chosen. This function automatically re-indexes the clustering solutions to match a reference solution as closely as possible using the Hungarian matching algorithm. This makes it easier to analytically compare multiple clustering solutions. The input is expected to be an output from `get.clusters.df`. The output is a dimensionally identical dataframe of clustering solutions, but with re-indexed cluster indices. In the example below, the CID clustering solution is used as reference. Option `logging=TRUE` pushes reindexing steps to the console for debugging.
```
mthds <- c("ACF","CID")
df_clusters <- get.clusters.df(df_outcomes,mthds,5)
df_clusters.matched <- match.clusters(df_clusters,"CID",logging=TRUE)
```

### compare.silhouettes <- function(df_ts,methods,kmin=2,kmax=10,kstep=1,logging=FALSE)
To find likely/intrinsic cluster counts `k`, cluster validity indices (silhouette widths) for different `k` values can be compared. This can be performed across multiple clustering methods to reduce bias. Function inputs are a dataframe of time series, a list of clustering methods, a range of `k` values to investigate (minimum, maximum and step size between those bounds). Logging provides console output of current operations and progress. Output is a dataframe (nrow = number of `k` values to consider, ncol = number of clustering methods to consider) where each cell is the average silhouette width for a given method and `k` combination.
```
mthds <- c("ACF","CID")
df_sils <- compare.silhouettes(df_outcomes,mthds,4,24,2,logging=FALSE)
```

### get.centroids <- function(df_ts,clusters)
The centroids/medoids of a partitional clustering solution are the objectively most dissimilar/distant elements in the set of clustered time series. Knowing these centroids can be useful to visualize archetypal model behaviors, or as inputs for robust decision making. This function takes a dataframe of time series and an existing clustering solution, and identifies the cluster centroids using Euclidean distance-based partitioning around medoids. Output is a dataframe containing the identified centroids.
```
mthds <- c("ACF","CID")
df_clusters <- get.clusters.df(df_outcomes,mthds,5)
df_CID_centroids <- get.centroids(df_outcomes,df_clusters$CID)
```

### plot.timeseries <- function(df_ts)
This function takes a dataframe of time series, and returns a ggplot-generated line plot of the time series in the dataframe with random colors.

```
line_plot <- plot.timeseries(df_outcomes)
```

### plot.timeseries.clustered <- function(df_ts,clusters)
This function takes a dataframe of time series and a clustering solution, and returns a ggplot-generated line plot of the time series, colored using the clustering solution.
```
mthds <- c("ACF","CID")
df_clusters <- get.clusters.df(df_outcomes,mthds,5)
line_plot_clustered <- plot.timeseries.clustered(df_outcomes,df_clusters$CID)
```

### plot.experiments <- function(df_expt,clusters)
This function is a convenience function to quickly show the experiment inputs generated by EMA Workbench in Python using a parallel coordinates plot in R, colored by assigned clusters. `df_expt` is expected to be a dataframe'd Experiments object from EMA Workbench pushed to R through Feather-Format.

```
mthds <- c("ACF","CID")
df_clusters <- get.clusters.df(df_outcomes,mthds,5)
expts_plot <- plot.experiments(df_experiments,df_clusters$CID)
```

### plot.confusionmatrix <- function(col1,col2,external=FALSE)
Two clustering solutions can easily be compared using a confusion matrix. This function provides a streamlined generator for such matrices. Inputs are two clustering solution columns (e.g. generated by `get.clusters.df`), and an `external` tag. The tag should be `FALSE` if two generated clustering solutions are compared, and `True` if one of the clustering solutions represents some external ground truth. If this is the case, provide the ground truth as the first clustering solution. The output is a figure representing a confusion matrix of the two solutions, shaded to visually indicate where the solutions agree/disagree. Note that the clustering solutions should be index-matched first, e.g. using `match.clusters`.
```
mthds <- c("ACF","CID")
df_clusters <- get.clusters.df(df_outcomes,mthds,5)
df_clusters.matched <- match.clusters(df_clusters,"CID",logging=TRUE)
confma_CID_ACF <- plot.confusionmatrix(df_clusters$CID,df_clusters$ACF,external=FALSE)
```

### plot.silhouettes <- function(df_sils)
This is a convenience function for plotting the output of `compare.silhouettes`. Takes the output of that function as input, and returns a ggplot-generated line plot of the silhouette widths over the investigated `k` values.
```
mthds <- c("ACF","CID")
df_sils <- compare.silhouettes(df_outcomes,mthds,4,24,2,logging=FALSE)
sils_plot <- plot.silhouettes(df_sils)
```

### Extensibility
The presented functions file is easily extensible with further clustering methods and validity indices. 

To add clustering methods, add them to the catalog of implemented clustering methods, and extend `get.clusters` with an appropriate subordinate function. Consult `get.TSclust.clusters` etc. as a template for extension.

To add validity indices, create additional functions mirroring `compare.silhouettes`. It may also be useful to create a superordinate function similar to `get.clusters` for validity indices, once multiple are implemented.
