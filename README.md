# BBSD-Public
Scenario discovery using time series clustering in R and Python. Created for my MSc thesis "Behavior-Based Scenario Discovery" at TU Delft, which is available at http://repository.tudelft.nl/.

This repository is structured into two folders, representing a simple (budworms) and complex (shale gas) example of time series clustering-based rule induction. Additionally, there is a Functions file for R which contains all the custom functions I developed. Finally, there is a third folder containing deprecated, work in progress and other auxiliary files from my research, which I provide mainly for documentation purposes related to my thesis. This folder also contains a third example, the Brusselator model, which is very similar to the budworms model and therefore is not discussed further.

For each example, a number of RStudio scripts and Jupyter Notebooks are provided. They are numbered in order of analysis and are intended to give an impression of how behavior-based scenario discovery can be conducted. The files also indicate where it is necessary to move data between R and Python, and vice-versa. I am currently developing a more concise examples file to showcase how every developed function can be used.

The order of analysis can be generalized as follows, and is reflected in the file numbering:
1) Create or load time series data in Python, export them from Python to R.
2) Load and explore time series data in R.
3) Investigate likely inherent cluster counts in the data in R.
4) Cluster the data using a chosen cluster count and a variety of clustering methods in R.
5) Export clustering solutions from R for Python.
6) Perform rule induction in Python.
7) Visualize induced rules in R.
