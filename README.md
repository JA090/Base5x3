---
title: "Base5x3"
---

Given an interval of effect sizes that are not materially significant (equivalent to zero), the tool considers 5 statistical tests to be performed on trial data: whether the effect size is at least the lower interval bound, is less than that, is at most the upper interval bound, is more than that, or is in the interval. Tests can be performed at up to 3 alpha levels. The tool is designed to encourage more thoughtful statistical design prior to data collection as well as more nuanced reporting of findings. For details see Aisbett et al., *“5×3: A Practical Approach to Encouraging Thoughtful Statistical Analyses.”*

## Installation
Install from GitHub:
```r
# install.packages("remotes")  # if you don't already have it
remotes::install_github("JA090/Base5x3")
```
***
## Overview
There are two main usage modes:  
- **Interactive mode (Shiny app)** – use a browser‑based interface to set parameters and view results live.    
- **Batch mode** – produce charts from parameters in an Excel spreadsheet for reproducible automated runs.  
The interactive version can also be accessed online at  
[https://meraglimja.shinyapps.io/5x3Tool](https://meraglimja.shinyapps.io/5x3Tool)


## 1. Interactive Mode – Shiny App
Use this mode when you want to explore parameter settings and see outputs immediately.
```r
library(Base5x3)
run_app()
```
This launches an interactive Shiny dashboard in your browser that has tabs to support Design or Analysis phases.  
Adjust parameters and explore charts. All calculations use the same core functions as the batch workflow.


## 2. Batch Mode from Excel input
Use this mode for reproducible or automated runs.

### Step 1 – Create or edit the Excel template
An example template that produces the figures in Aisbett et al. is in:
```
inst/extdata/charts.xlsx
```
Do not edit the first column, which contains the parameter names used in the R code.  
A parameter set is entered in column 2, then optionally other parameter sets in columns 3, 4 etc. 


### Step 2 – Save parameters from the Excel sheet into an `.rda` file 
Run `read_params_Excel("inst/extdata/yourExcel", header=FALSE)`. This automatically creates a file `my_parameters.rda` where my_parameters is the first row entry in the Excel spreadsheet. 
This file can be re‑loaded later using `load()` without needing to reopen Excel.

### Step 3 - Load previously saved parameters and run. 
```r
load("my_parameters.rda")
buildFigure(my_parameters)
```

This will output a Design or Analysis chart (depending on parameters).
More complex options to output two charts with one or two legends are available. For example, this code will produce a pdf with two charts sharing the legend from the second chart.
```r
devtools::load_all()
pdf("myChart.pdf", width = 13, height = 5)
  buildFigure(param = my_parameters1, param2 = my_parameters2)
dev.off()

```

###  Expected parameters
 alpha: 6x3 matrix with first row containing alpha values, other rows "*" if want one-sided test at that level  
 power: 5x3 matrix of powers for sample size estimation, else 0  
 sample: Total sample size (provisional if computing). Invalid values default to 50  
 MML, MMU: Boundaries of minimum meaningful effect magnitudes  
 xmin, xmax: Range of effect sizes to display  
 zmin, zmax: Range of SEs or sample sizes to display  
 chartType: Design for sample size estimation phase, otherwise Analysis   
 Study: Fraction of sample in larger group (two-group study) or 1 (single-group)  
 chartBW: TRUE for black & white chart  
 colorvec: Vector of colors for regions and data points  
 ltyBW : line type for B&W charts  
 ES :  effect size (possibly comma separated list)  
 dataV : variance of data (or possible SE in Analysis phase)  
 VES :  TRUE if SE rather than variance  
 labels :  6-length vector of terminology for the 5 tests + no result  
 levels :  3-length vector of terminology for the strength of test levels (in order of increasing strength: later entries may be blank)  

### Excel spreadsheet
alpha is entered as a column of height 6, with each entry of the form c(x,x,x)   
power is entered as a column of height 5  
MML and MMU are entered in row labelled MM as c(MML,MMU)  
ranges are entered in row labelled limits as c(xmin,xmax,zmin,zmax)  
colorvec will be thr default if the row labelled color is blank. Otherwise enter .csv filename containing color palette (see User Manual)  
labels and levels are default terminology if row labelled terminology is blank, otherwise enter .csv filename  

## Main functions 

| Function | Purpose |
|-----------|----------|
| `run_app()` | Launch the interactive Shiny interface |
| `read_params_excel()` | Read parameters from an Excel sheet |
| `buildFigure()` | Create a chart or charts to (i) in Design phase, visualize sample sizes needed to deliver nominated power for various tests, given anticipated effect size and variance or (ii) visualize test results 


# License
This project is licensed under the terms of the [GNU General Public License v3.0](LICENSE).


