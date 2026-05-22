# ==============================================================================
# 5x3 VISUALIZATION TOOL
# ==============================================================================

# -------- Load required libraries ---------------------------------------------
library(shiny)          # Core Shiny framework
library(bslib)          # Bootstrap themes and modern UI components
library(htmltools)      # HTML generation and manipulation
library(shinyWidgets)   # Enhanced input widgets
library(shinyMatrix)    # Matrix input UI component
library(shinyjs)        # JavaScript operations in Shiny
library(shinyjqui)      # jQuery UI: draggable/resizable elements
library(shinyBS)        # Bootstrap tooltips and popovers


# -------- Source all custom R functions from the ./R directory ----------------
r_files <- list.files(path = "../R", pattern = "\\.R$", full.names = TRUE)
for (file in r_files) {
  source(file, encoding = "UTF-8")
}

# -------- Set margins of material significance --------------------------------
MML <- -1
MMU <- 0
sliderVal <- c(MML, MMU)

# -------- Formal terms for hypothesis test outcomes ---------------------------
Labels <<- matrix(data = NA, nrow = 2, ncol = 6)
Labels[1, 6] <- "NO TEST WITH POWER"  # Term in design stage
Labels[2, 6] <- "NO TEST REJECTED"    # Term for analysis stage

# Default nomenclature prefixes for design/analysis stages
prefix <<- c("TEST FOR ", "REJECT ")
notest <<- c("NO TEST WITH POWER", "NO TEST REJECTED")

# Build formal labels using material significance margins
for (pos in 1:2) {
  Labels[pos, 1:5] <- c(
    paste0(prefix[pos], "E \u2265 ", sliderVal[1]),
    paste0(prefix[pos], "E \u2264 ", sliderVal[2]),
    paste0(prefix[pos], "E > ", sliderVal[2]),
    paste0(prefix[pos], "E < ", sliderVal[1]),
    paste0(prefix[pos], "E > ", sliderVal[1], " & E < ", sliderVal[2])
  )
}

Labels0 <- Labels  # Use to return to default terminology

# -------- Formal strength terms for tests -------------------------------------
# Note: If test levels are changed, these terms will be modified
firstA <- c(.025, .0025, "")
Levels <<- matrix(data = NA, nrow = 2, ncol = 3)
Levels[1, ] <- paste0("α = ", firstA)  # Design stage
Levels[2, ] <- paste0("p < ", firstA)  # Analysis stage

# -------- Color palette & plotting styles -------------------------------------
# Rejection regions of the tests, region where no test selected, and data points
colorvec0 <<- c(
  "#a6d854", "#66c2a5", "green",
  "#fc8d62", "#e5c494", "#ffd92f",
  "purple", "#e78ac3", "#8da0cb",
  "lightgray", "black"
)

# Prepare for monochrome option (will be overlaid with bars)
colorvecBW <<- c(
  "lightgray", "darkgray", "gray35",
  "lightgray", "darkgray", "gray35",
  "#E5E7EB", "#6B7280", "#374151",
  "snow2", "black"
)

ltyBW <<- c("dashed", "longdash", "solid", "twodash", "dotdash", "dotted")
colorvec <<- colorvec0
inputBW <<- FALSE

# -------- Default plot ranges -------------------------------------------------
# X-axis represents effect size
xmin0 <<- -1.5
xmax0 <<- 1.5

# Y-axis represents standard error in the analysis stage
ymin0 <<- 0
ymax0 <<- 0.6

# Y-axis represents total sample size in the design stage
ymin0SS <<- 16
ymax0SS <<- 1000

# Slider range
slide1 <- xmin0
slide2 <- xmax0

# -------- Test and power matrices ---------------------------------------------
# Test matrix: 1st row contains selected alpha values, other entries = '*' if active
D <- c(
  firstA,
  "*", "*", "",
  "*", "*", "",
  "", "*", "",
  "", "*", "",
  "", "*", ""
)

# Define dimensions and labels for test matrices
inputalps <<- matrix(D, 6, 3, byrow = TRUE, dimnames = list(c("", Labels[1, 1:5])))
alpha <- matrix(D, 6, 3, byrow = TRUE, dimnames = list(c("", Labels[1:5])))

# Power matrix in % : 5 tests x 3 alpha levels
inputpower <<- matrix(rep(80, 15), 5, 3, dimnames = list(Labels[1, 1:5], firstA))

# -------- Initial data (Analysis stage) / Estimated data (Design stage) ------------------------
anteSS <- 200
anteES <- ".6, 1.4"
anteVar <- 1
dataMean <- "-0.5, 0.6, 1.2"
dataV <- "0.134, 0.19, 0.5"
VES <- FALSE

# ----------Start with Analysis tab active
firstTab <- "Analysis"
labels <- Labels[2, ]
pos <- 2
