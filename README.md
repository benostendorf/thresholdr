# thresholdr

## Overview

thresholdr is a collection of auxiliary functions that serve to calculate latencies between events in longitudinal data: 

* `latency_one_threshold()` takes a matrix with longitudinal measurements and timepoints and returns a matrix with (i) latencies until a given threshold was reached and (ii) status (endpoint met versus censored). 
* `latency_two_thresholds()` takes a matrix with longitudinal measurements and timepoints and returns a matrix with (i) latencies between two measurements and (ii) status (endpoint reached versus censored). 
* `calculate_volume()` calculates the volume of an ellipsoid object given width and length. 
* `survival_custom()` provides a customized survival plotting function. 

## Installation

```
# install.packages("devtools") 
devtools::install_github("benostendorf/thresholdr", build_vignettes = TRUE)
```

## Usage

```


```