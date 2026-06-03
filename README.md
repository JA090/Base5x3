---
title: "Base5x3"
---

Given an interval of effect sizes [MML, MMU] that are not materially significant (i.e., equivalent to zero), the tool considers 5 statistical t-tests to be performed on trial data: viz., 
testing for an effect size at least the lower bound MML, no greater than the upper bound MMU, less than MMU, more than MML, or in the interval [MML, MMU]. Tests can be performed at up to 3 alpha levels. 
The tool has Design and Analysis options, and is designed to encourage more thoughtful statistical design prior to data collection as well as more nuanced reporting of findings. For details see Aisbett et al., *“5×3: A Practical Approach to Encouraging Thoughtful Statistical Analyses.”*

## Installation
To install this package, make sure you have R (>=4.1.0) and Rstudio installed. 
Then install the "remotes" package if you don't have it (install.packages("remotes").
Finally, install the 5x3 package from github:

```r
remotes::install_github("JA090/Base5x3")
```

## Overview
There are two main usage modes:  
- **Interactive mode (Shiny app)** – use a browser‑based interface to set parameters and view results live.    
- **Batch mode** – produce charts from parameters in an Excel spreadsheet for reproducible automated runs.  
The interactive version can also be accessed online at [https://meraglimja.shinyapps.io/5x3Tool](https://meraglimja.shinyapps.io/5x3Tool).

All modes use the same core functions.

## 1. Interactive Mode – Shiny App
Use this mode when you want to explore parameter settings and see outputs immediately.

```r
library(Base5x3)
run_app()
```
This launches an interactive Shiny dashboard in your browser that has tabs to support Design or Analysis stages of your research. It is easy to adjust parameters and explore charts. 

The paper Aisbett et al., as well as a User Manual, can be accessed through the Supporting Documents button near the top left of the page. 
The look of the charts (colors and terminology) can be changed by uploading .csv files as described in the Manual.
Examples of color and terminology files respectively are downloaded with the Base5x3 package and can be found in the folder:

```r
system.file("extdata", package="Base5x3")
```
## 2. Batch Mode from Excel input
Use this mode for reproducible or automated runs. For example, to re-create Fig 1a in Aisbett et al, 

```r
library(Base5x3)
build_figure(Fig1a)
```

### Step 1 – Edit the Excel template to specify your desired chart(s)
An example template that produces all the charts in the figures in Aisbett et al. is

```r
system.file("extdata", "charts.csv",package="Base5x3")
```

Do not edit the first column, which contains the parameter names used in the R code.  
A set of parameters to create a chart is entered in column 2, then optionally other sets are entered in columns 3, 4 etc. 

### Step 2 – Save parameters from your Excel sheet into an `.rda` file 
Run `Base5x3::read_params_Excel("Excel.csv")`where Excel.csv is your edited file (in the same folder as the template). This automatically creates a file `data/my_name.rda` where my_name is the first row entry in the Excel spreadsheet. 
This file can be re‑loaded later using `load()` without needing to reopen Excel.

### Step 3 - Load previously saved parameters and run. 

```r
load("data/my_name.rda")
Base5x3::build_figure(my_name)
```
This will output a Design or Analysis chart (depending on parameters).

More complex options to output two charts with one or two legends are available. 
For stable results, rather than use the plot panel, save the chart as a pdf or jpg. For example, this code will produce a pdf with two charts sharing the legend from the second chart.
```r
pdf("myChart.pdf", width = 13, height = 5)
  build_figure(param = my_parameters1, param2 = my_parameters2)
dev.off()

```
#### Expected parameters
 **chartType**: Design for sample size estimation stage, otherwise Analysis.  
 **alpha**: 6x3 matrix with row names automatically provided as a blank followed by names for the 5 test hypotheses. The first row contains up to three test alpha values, and other rows contain "*" if that hypothesis is to be tested at that level.  
 **power**:  5x3 numerical matrix of % powers required for each of the tests. Only used if chartType=Design. 
 **sample**: Total sample size (provisional estimate if chartType = Design).  
 **MML, MMU**: Boundaries of materially significant values.  
 **xmin, xmax**: Range of displayed effect sizes.  
 **zmin, zmax**: Range of displayed standard errors (chartType = Analysis) else total sample sizes.  
 **Study**: Fraction of sample in larger group (two-group study) or 1 (single-group).  
 **chartBW**: TRUE for black & white chart.  
 **colorvec**: 11-length vector of colors for regions and data points, with the first 3 colours for tests of the hypothesis that the effect size is at least MML, the next 3 for tests it is at most MMU, then 3 for tests that it is in [MML, MMU]. The next colour is to denote no hypothesis is rejected ( or no test has suffieient power in Design stage), and the last color is for data points or lines.  
 **ltyBW**: 6-length vector of line types for B&W charts, with the first 3 types for tests of the hypothesis that the effect size is less than MMU and the other for the hypothesis it is greater than MML.  
 **ES**:  effect size or anticipated effect size (possibly comma separated list).   
 **dataV**: variance of data (in Analysis stage, may be a comma separated list of the same length as ES, and may be standard errors rather than variances).   
 **VES**:  FALSE if trial summary data arestandard errors, not variances.  
 **labels**:  6-length vector of terminology for rejection of each of the 5 test hypotheses + term for when none rejected.   
 **levels**:  3-length vector of terminology for the strength of test levels (in order of increasing strength: later entries may be blank).   

#### More about the Excel spreadsheet
**alpha** is entered as a column of height 6, with each entry of the form c(x, x, x).   
**power** is entered as a column of height 5.  
**MML** and **MMU** are entered in row labelled MM as c(MML, MMU).  
**display ranges** are entered in a row labelled limits as c(xmin, xmax, zmin, zmax).  
**colorvec** will be the default if the row labelled color is blank. Otherwise enter .csv filename containing color palette (see User Manual).  
**labels** and **levels** are default terminology if row labelled terminology is blank, otherwise enter .csv filename.  

## Main functions 

| Function | Purpose |
|-----------|----------|
| `run_app()` | Launch the interactive Shiny interface |
| `read_params_excel()` | Read parameters from an Excel sheet |
| `build_figure()` | Create a chart or charts to (i) at Design stage, visualize sample sizes needed to deliver nominated power for various tests, given anticipated effect size and variance or (ii) visualize study summary data 


# License
This project is licensed under the terms of the [GNU General Public License v3.0](LICENSE).


