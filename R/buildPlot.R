#' Build Chart
#'
#' @description Creates a complete chart displaying rejection regions for
#'   multi-level statistical tests. Handles both Design phase (sample size
#'   estimation) and Analysis phase visualizations.
#'
#' @param param List containing all chart parameters (see details)
#' @details
#' The param list should contain:
#' \describe{
#'   \item{alpha}{Matrix with first row containing alpha values, other rows "*" if one-sided test at that level}
#'   \item{power}{Matrix of powers for sample size estimation, else 0}
#'   \item{sample}{Total sample size (provisional if computing). Invalid values default to 50}
#'   \item{var}{Estimated variance for sample size estimation}
#'   \item{MML}{Boundaries of minimum meaningful effect magnitudes}
#'   \item{MMU}{ditto}
#'   \item{xmin}{Range of effect sizes to display}
#'   \item{xmax}{ditto}
#'   \item{zmin}{Range of SEs or sample sizes to display}
#'   \item{zmax}{ditto}
#'   \item{chartType}{"Design" for sample size estimation phase, otherwise analysis phase}
#'   \item{Study}{Fraction of sample in larger group (two-group study) or 1 (single-group)}
#'   \item{chartBW}{TRUE for black & white chart}
#'   \item{colorvec}{Vector of colors for regions and data points}
#'   \item{ltyBW}{line type for B&W charts}
#'   \item{ES}{number of comma separated list of effect sizes}
#'   \item{dataV}{variance of data in Design phase, data SE or variance in Analysis phase}
#'   \item{VES}{TRUE if variance rather than SE in Analysis data}
#'   }
#' @importFrom grDevices as.graphicsAnnot xy.coords
#' @importFrom graphics lines par points polygon rect segments strheight strwidth text xinch yinch
#' @importFrom stats qt
#' @importFrom utils read.csv
#' @export
buildPlot <- function(param) {

  with(param, {
     # -------------------------------------------------------------------------
    # CALCULATE DEGREES OF FREEDOM AND STUDY DESIGN FACTOR
    # -------------------------------------------------------------------------
    samp <- if (is.na(sample) || sample < 2) 50 else sample
    # Degrees of freedom: n - 2 for two-group, n - 1 for single-group
    DOF <- samp - 2 + as.integer(Study)

    # Study design factor for SE calculations
    # For two-group: sqrt(1/r + 1/(1-r)) where r is proportion in larger group
    # For single-group (Study = 1): factor = 1
    r <- if (Study < 1) sqrt(1 / Study + 1 / (1 - Study)) else 1

    # -------------------------------------------------------------------------
    # BUILD ALPHA AND POWER MATRICES
    # Convert from display format to numeric matrices for calculations
    # -------------------------------------------------------------------------

    # Initialize 5×3 matrices (5 test types × 3 significance levels)
    alphas <- matrix(data = rep(NA, 15), nrow = 5, ncol = 3)
    powers <- matrix(data = rep(NA, 15), nrow = 5, ncol = 3)

    # Populate matrices where tests are active (marked with "*")
    for (j in 1:3) {
      if (alpha[1, j] != "" && as.numeric(alpha[1, j]) < 1) {
        for (k in 1:5) {
          if (!is.na(alpha[k + 1, j]) && alpha[k + 1, j] == "*") {
            alphas[k, j] <- as.numeric(alpha[1, j])
            powers[k, j] <- power[k, j]
          }
        }
      }
    }

    # -------------------------------------------------------------------------
    # SET UP Y-AXIS (SE or Sample Size) - REVERSED
    # -------------------------------------------------------------------------

    if (chartType == "Design") {
      # Design phase: y-axis shows sample size
      # Convert zmin/zmax from sample size to SE scale
      # Reversed: ymin > ymax so axis runs top-to-bottom
      ymin <- r / sqrt(zmin)  # smaller SE at bottom (larger n)
      ymax <- r / sqrt(zmax)
      xlab <- "anticipated effect size E"
      ylab <- "sample size"

      # Generate equi-spaced tick positions in SE units
      ytick_pos <- seq(from = ymin, to = ymax, length.out = 5)
      ytick <- as.integer((r/ytick_pos)^2)

    } else {
      # Analysis phase: y-axis shows SE directly
      # Reversed: ymin > ymax so axis runs top-to-bottom
      ymin <- zmax  # Larger SE will be plotted at bottom
      ymax <- zmin  # Smaller SE at top
      xlab <- "effect size E"
      ylab <- "standard error SE"

      # Generate tick positions
      ytick <- seq(from = zmin, to = zmax, length.out = 5)
      ytick <- round(ytick, digits = 2)
      ytick_pos <- ytick
    }

    # -------------------------------------------------------------------------
    # CREATE BASE PLOT
    # -------------------------------------------------------------------------

    par(mar = c(4, 4, 3, 1), cex = 1.1)

    plot(
      x    = 0,
      y    = 0,
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax),
      type = "n",
      xlab = xlab,
      ylab = ylab,
      axes = FALSE
    )

    # -------------------------------------------------------------------------
    # DRAW REJECTION REGIONS
    # -------------------------------------------------------------------------

    drawRegions(
      chartType = chartType,
      alphas    = alphas,
      power     = powers,
      MML       = MML,
      MMU       = MMU,
      xmin      = xmin,
      xmax      = xmax,
      ymin      = ymax,  # Pass actual min SE (smaller SE / larger n)
      ymax      = ymin,  # Pass actual max SE (larger SE / smaller n)
      ytick     = ytick,
      DOF       = DOF,
      colorvec  = colorvec,
      chartBW   = chartBW,
      ltyBW     = ltyBW
    )

    # -------------------------------------------------------------------------
    # DRAW AXES
    # -------------------------------------------------------------------------

    # X-axis (effect size)
    axis(side = 1, tick = TRUE, las = 1)

    # Y-axis with appropriate labels
    axis(
      side   = 2,
      at     = ytick_pos,
      labels = ytick,
      tick   = TRUE,
      las    = 1,
    )

    # -------------------------------------------------------------------------
    # DRAW TICK MARKS AT MATERIAL SIGNIFICANCE MARGINS
    # -------------------------------------------------------------------------
    #shift labels so don't overlap if necessary
    if(MMU>MML)
      shift=.01*(xmax-xmin)/(MMU-MML)
    else
      shift=0

    if (MML >= xmin && MML <= xmax) {
      axis(
        side   = 3,
        at     = MML,
        labels=F,
        line=0,
        tick   = TRUE,
        pos=ymax ,
        tcl=-.9,
      )

      axis(side=3, at = MML-shift, lwd.tick=0,pos=ymax,labels=round(MML,3))

    }

    if (MMU != MML && MMU >= xmin && MMU <= xmax) {
      axis(
        side   = 3,
        at     = MMU,
        line=0,
        labels=F,
        tick   = TRUE,
        pos=ymax,
        tcl=-.9,
      )
      axis(side=3, at = MMU+shift, lwd.ticks=0,pos=ymax,labels=round(MMU,3))
    }

    # -------------------------------------------------------------------------
    # PLOT DATA POINTS (if provided in Analysis) ELSE  LINES AT ANTICIPATED EFFECT SIZES
    # -------------------------------------------------------------------------
    if (length(ES) > 0) { if (chartType=="Analysis"){
      datadisplay(
        ES,
        dataV,
        VES,
        sample,
        Study,
        colorvec[11],
        1.2,
        xmin,
        xmax,
        zmin,
        zmax
      )
    }

    else
    {drawLines(ES,xmin,xmax,ymin,ymax,colorvec[11])}
  }



  })
}
