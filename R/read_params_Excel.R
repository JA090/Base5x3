# =============================================================================
#  Read from Excel spreadsheet "charts.csv" to create .rda parameter files for charts descibed in Aisbett et al.,
#          "5x3: A Practical Approach to Encouraging Thoughtful Statistical Analysis"
# =============================================================================

# -----------------------------------------------------------------------------
# DEFAULT COLOR PALETTES
# -----------------------------------------------------------------------------

# Color palette for rejection regions, "no test selected" region, and data points
# Order: 3 greens, 3 oranges/yellows, purple/pink/blue, gray (no test), black (data)
colorvec0 <- c(
  "#a6d854", "#66c2a5", "green",
  "#fc8d62", "#e5c494", "#ffd92f",
  "purple", "#e78ac3", "#8da0cb",
  "lightgray", "black"
)

# Optional monochrome palette (will be overlaid with bars for differentiation)
colorvecBW <- c(
  "lightgray", "darkgray", "gray35",
  "lightgray", "darkgray", "gray35",
  "#E5E7EB", "#6B7280", "#374151",
  "snow2", "black"
)

# Line types for monochrome charts
ltyBW <- c("solid", "longdash", "dashed", "dotted", "dotdash", "twodash")

# -----------------------------------------------------------------------------
# READ PARAMETER FILE
# -----------------------------------------------------------------------------
read_params_Excel <- function(filename) {
Params <- read.csv(filename)

# -----------------------------------------------------------------------------
# CREATE .RDA PARAMETER SET FOR EACH chart
# -----------------------------------------------------------------------------
for (fig in 2:length(Params[1,])) {

  # --- Figure identification ---
  myName   <-  Params[1, fig]


  # --- Chart parameters from spreadsheet ---
  chartType <- Params[2, fig]
  MM      <- eval(parse(text = Params[16, fig]))   # Margins of material significance
  limits  <- eval(parse(text = Params[17, fig]))   # Chart limits: x1, x2, y1, y2
  sample  <- as.numeric(Params[14, fig])           # Total sample size
  Study   <- as.numeric(Params[15, fig])           # Proportion in larger group
  ES      <- as.character(Params[18, fig])         # Effect size (comma-separated if multiple)
  dataV   <- Params[19, fig]                       # Data variance (possibly comma-separated)
  chartBW <- as.logical(Params[22, fig])           # TRUE for monochrome chart
  # Extract and sort alpha levels (descending)
  firstA <- sort(eval(parse(text = Params[3, fig])), decreasing = TRUE) # alpha levels

  # ---------------------------------------------------------------------------
  # SET UP LEGEND TERMINOLOGY
  # Terminology varies between design phase (tests) and analysis phase (results)
  # ---------------------------------------------------------------------------

  if (!(is.na(Params[21, fig]) | Params[21, fig] == "")) {
    # --- Read informal terminology from external file ---
    Terms  <- read.csv(
      paste0(getwd(), "/", parse(text = Params[21, fig])),
      header = FALSE
    )
    levels <- as.vector(Terms[2, ][Terms[2, ] != ""])


    if (chartType == "Analysis") {
      # Analysis phase: use terminology as-is
      labels <- c(paste0(Terms[1, ]), Terms[3, 1])
    } else {
      # Design phase: rephrase in terms of tests rather than results
      labels <- c(paste0("TESTING FOR ", Terms[1,]),"NO TEST HAS POWER")
    }

  } else {
    # --- No informal terminology: create formal test/strength terms from alpha levels ---
    # Create level labels based on phase
    if (chartType == "Design") {
      levels <- paste0("alpha =", firstA)
    }
    if (chartType == "Analysis") {
      levels <- paste0("p < ", firstA)
    }

    # Default nomenclature prefixes for design/analysis phases
    prefix <- c("TEST FOR ", "REJECT ")
    notest <- c("NO TEST WITH POWER", "NO TEST REJECTED")
    pos <- if (chartType == "Analysis") 2 else 1
    # Build formal labels using material significance margins
    labels <- c(
      as.expression(bquote(paste(.(prefix[pos]), E >= .(MM[1])))),
      as.expression(bquote(paste(.(prefix[pos]), E <= .(MM[2])))),
      as.expression(bquote(paste(.(prefix[pos]), E > .(MM[2])))),
      as.expression(bquote(paste(.(prefix[pos]), E < .(MM[1])))),
      as.expression(bquote(paste(.(prefix[pos]), E > .(MM[2]), " & ", E < .(MM[1])))),
      bquote(.(notest[pos]))
    )

    # Special case: if margins are equal, consolidate labels
    if (MM[1] == MM[2]) {
      labels[1] <- labels[3]
      labels[2] <- labels[4]

    }
  }

  # ---------------------------------------------------------------------------
  # SET UP TEST AND POWER MATRICES
  # ---------------------------------------------------------------------------

  # Test matrix: row 1 = selected alpha values, other rows = '*' if test is active
  alpha <- matrix(
    data     = rep("", 18),
    nrow     = 6,
    ncol     = 3,
    dimnames = list(c("", labels[1:5]))
  )

  # Power matrix: target power for each test
  power <- matrix(
    data     = rep(0, 15),
    nrow     = 5,
    ncol     = 3,
    byrow    = TRUE,
    dimnames = list(labels[1:5])
  )

  # Populate alpha row (first row)
  for (j in 1:length(firstA)) {
    alpha[1, j] <- firstA[j]
  }

  # Populate test indicators and power values from spreadsheet
  for (i in 1:5) {
    a <- eval(parse(text = Params[3 + i, fig]))   # Alpha values for test i
    p <- eval(parse(text = Params[8 + i, fig]))   # Power values for test i

    if (length(a) > 0) {
      for (j in 1:length(a)) {
        alpha[i + 1, j] <- a[j]
      }
    }
    if (length(p) > 0) {
      for (j in 1:length(p)) {
        power[i, j] <- p[j]
      }
    }
  }
  # ---------------------------------------------------------------------------
  # SET UP COLOR VECTOR
  # Colors may be read from external file or use defaults
  # ---------------------------------------------------------------------------

  if (is.na(Params[20, fig]) | Params[20, fig] == "") {
    # Use default palette based on monochrome setting
    colorvec <- if (chartBW) colorvecBW else colorvec0
  } else {
    # Read custom colors from external file
    Col <- read.csv(
      paste0(getwd(), "/", parse(text = Params[20, fig])),
      header = FALSE
    )
    colorvec <- c(
      t(Col[1, 1:3]),
      t(Col[2, 1:3]),
      t(Col[3, 1:3]),
      Col[4, 1],
      Col[5, 1]
    )
  }

  # ---------------------------------------------------------------------------
  # CREATE AND SAVE THE .RDA FILE
  # ---------------------------------------------------------------------------

  # Bundle all parameters into a list
  myData <- list(
    chartType = chartType,
    alpha     = alpha,
    power     = power,
    sample    = sample,
    Study     = Study,
    MML       = MM[1],
    MMU       = MM[2],
    xmin      = limits[1],
    xmax      = limits[2],
    zmin      = limits[3],
    zmax      = limits[4],
    levels    = levels,
    labels    = labels,
    colorvec  = colorvec,
    ltyBW     = ltyBW,
    ES        = ES,
    dataV     = dataV,
    VES       = FALSE,
    chartBW   = chartBW
  )

  # Assign to named object and save
  assign(myName, myData)
  save(
    list = myName,
    file = paste0(getwd(), "/data/", myName, ".rda")
  )
}}
