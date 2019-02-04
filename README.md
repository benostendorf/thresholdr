# thresholdr

## Overview

thresholdr is a collection of auxiliary functions that serve to calculate latencies between events in longitudinal data: 

* `calculate_volume()` calculates the volume of an ellipsoid object given width and length. 
* `time_to_one_threshold()` takes a matrix with longitudinal measurements and timepoints and returns a matrix with latencies until a given threshold was met and status (endpoint met versus censored). 
* `latency_two_thresholds()` takes a matrix with longitudinal measurements and timepoints and retunrs a matrix with latencies between two measurements and status (endpoint met versus censored). 
* `survival_custom()` provides a customized survival plotting function. 

## Installation

```
# install.packages("devtools") 
devtools::install_github("bnostendorf/thresholdr", build_vignettes = TRUE)
```
