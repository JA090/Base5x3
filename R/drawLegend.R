#' Draw Legend for Multi-Level T est Charts
#'
#' @description Creates a composite legend for displaying test results at multiple
#'   significance levels. The legend is composed of multiple sub-legends, one for
#'   each test type, with entries corresponding to test levels.
#'
#' @param param List containing  parameters (see details)
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
#'    }
#' @return The base plot object for the legend
#' @importFrom grDevices as.graphicsAnnot xy.coords
#' @importFrom graphics lines par points polygon rect segments strheight strwidth text xinch yinch
#' @importFrom stats qt
#' @importFrom utils read.csv
#' @export
drawLegend <- function(param) {

  with(param, {
  # ---------------------------------------------------------------------------
  # CREATE BLANK PLOT FOR LEGEND PLACEMENT

  # Legend goes on a dummy plot so it can be separately draggable in app version
  # ---------------------------------------------------------------------------
    Leg <- plot(
    x    = NA,
    y    = NA,
    xlim = c(0, 1),
    ylim = c(1, 0),
    axes = FALSE,
    main = "",
    xlab = "",
    ylab = ""
  )

  # ---------------------------------------------------------------------------
  # RECORD WHICH TESTS ARE AT WHICH LEVELS
  # ---------------------------------------------------------------------------

  # Matrix tracking alpha levels for each of 5 tests × 3 possible levels
  editedAlphas <<- matrix(
    data = rep(NA, 15),
    nrow = 5,
    ncol = 3
  )

  #Only put in legend entries for regions that will be shown on the chart
  #  depends also on phase
  if (chartType=="Analysis")
      ymin=zmin
  else
      {ymin=sqrt(1/zmax)
      if (Study<1 )ymin= ymin*sqrt(1 / Study + 1 / (1 - Study))
      }

  for (j in 1:3) {
    if (alpha[1,j]!="") if(alpha[1, j] < 1) {
      q <- qt(1 - as.numeric(alpha[1, j]), sample - 2 + as.integer(Study))
      if (chartType == "Design")
        q <- q + qt(1 - .5 * (1 - power[5, j] / 100), sample - 2 + as.integer(Study))

      K=integer(0)

      if (xmin < MML)
        if ((MML - xmin) > q * ymin)
          K = c(K, 1)
      if (xmin < MMU)
        if ((MMU - xmin) > q * ymin)
          K = c(K, 3)
      if (xmax > MMU)
        if ((xmax - MMU) > q * ymin)
          K = c(K, 2)
      if (xmax > MML)
        if ((xmax - MML) > q * ymin)
          K = c(K, 4)
      if (MML  < xmax | MMU > xmin)
        if (ymin < (MMU - MML) / (2 * q))
          K = c(K, 5)

      for (k in K) {
        if (alpha[k + 1, j] == "*")
          editedAlphas[k, j] <- as.numeric(alpha[1, j])
        }
      }
    }

  # ---------------------------------------------------------------------------
  # BUILD COLOR/LINE TYPE MATRIX
  # Colors correspond to each of the 4 one-sided tests and equivalence test
  # at each of the 3 possible levels
  # ---------------------------------------------------------------------------

  matrixCol <- matrix(NA, nrow = 5, ncol = 3)

  # Base color/line type vector
  d <- c(
    colorvec[1:6],
    colorvec[4:6],
    colorvec[1:3],
    colorvec[7:9]
  )

  # For B&W charts, use line types instead of colors for non-inf/non-sup
  if (chartBW) {
    d[7:12] <- ltyBW[c(4:6, 1:3)]
  }

  # Populate color matrix
  for (k in 1:5) {
    for (j in 1:3) {
      matrixCol[k, j] <- d[(k - 1) * 3 + j]
    }
  }

  # Matrix for colors/line types of tests actually nominated
  matrixColorsUsed <- matrix(nrow = 5, ncol = 3)

  # Number of test levels for each of the 4 one-sided tests + equivalence
  len <- rep(0, 5)

  # ---------------------------------------------------------------------------
  # GET LABELS AND COLORS FOR TESTS ACTUALLY CONDUCTED
  # ---------------------------------------------------------------------------

  Used <- matrix(data = NA, nrow = 5, ncol = 3)

  for (k in 1:5) {
    i <- 1
    for (j in 1:3) {
      if (!is.na(editedAlphas[k, j])) {
        Used[k, i] <- levels[j]
        matrixColorsUsed[k, i] <- matrixCol[k, j]
        i <- i + 1
      }
    }
    len[k] <- i - 1
  }

  # ---------------------------------------------------------------------------
  # WRITE OUT LEGENDS
  # ---------------------------------------------------------------------------

  legSize <- 1
  scale <- 0.065  # Positioning parameter for each legend

  # --- Superiority and Inferiority entries (tests 1-2) ---

  for (k in 1:2) {
    s <- 0

    # Get y-axis position based on previous legend
    if (k > 1 && len[1] > 0) {
      s <- l1$text$y[1] + scale
    }

    if (len[k] > 0) {
      l1 <<- legend2(
        x       = 0,
        y       = s,
        ncol    = 3,
        pt.cex  = 3,
        title   = labels[k],
        title.adj = 0,
        fill    = matrixColorsUsed[k, 1:len[k]],
        border  = matrixColorsUsed[k, 1:len[k]],
        legend  = Used[k, 1:len[k]],
        cex     = legSize,
        bty     = "n"
      )
    }

    # Overlay inferiority with hatch pattern for B&W charts
    if (k == 1 && chartBW) {
      legend2(
        x       = 0,
        y       = 0,
        ncol    = 3,
        title   = "",
        density = c(15, 15, 15),
        angle   = c(45, 45, 45),
        fill    = c("white", "white", "white"),
        border  = matrixColorsUsed[k, 1:len[k]],
        legend  = Used[k, 1:len[k]],
        cex     = legSize,
        bty     = "n"
      )
    }
  }

  # --- Non-inferiority, Non-superiority, and Equivalence entries ---
  # Only drawn if margins of material significance differ

  if (MMU>MML) {

    # Non-inferiority and Non-superiority entries (tests 3-4)
    # Displayed as lines rather than filled boxes
    for (k in intersect(K, 3:4)) {
      if (chartBW) {
        lty <- matrixColorsUsed[k, 1:len[k]]
        col <- rep("black", len[k])
      } else {
        lty <- 1
        col <- matrixColorsUsed[k, 1:len[k]]
      }

      if (len[k] > 0) {
        l1 <- legend2(
          x         = 0,
          y         = l1$text$y[1] + scale,
          ncol      = 3,
          title     = labels[k],
          title.adj = 0,
          legend    = Used[k, 1:len[k]],
          cex       = legSize,
          bty       = "n",
          lty       = lty,
          lwd       = 3,
          col       = col
        )
      }
    }

    # Equivalence entry (test 5)
    if (len[5] > 0) {
      l2 <- legend2(
        x         = 0,
        y         = l1$text$y[1] + scale,
        ncol      = 3,
        title     = labels[5],
        title.adj = 0,
        legend    = Used[5, 1:len[5]],
        cex       = legSize,
        fill      = matrixColorsUsed[5, 1:len[5]],
        border    = matrixColorsUsed[5, 1:len[5]],
        bty       = "n"
      )

      # Overlay equivalence with hatch pattern for B&W charts
      if (chartBW) {
        legend2(
          x       = 0,
          y       = l1$text$y[1] + scale,
          ncol    = 3,
          title   = "",
          density = c(15, 15, 15),
          angle   = 0,
          fill    = c("white", "white", "white"),
          border  = matrixColorsUsed[5, 1:len[5]],
          legend  = Used[5, 1:len[5]],
          cex     = legSize,
          bty     = "n"
        )
      }

      l1 <- l2
    }
  }

  # --- Inconclusive entry (always last) ---

  legend2(
    x         = 0,
    y         = l1$text$y[1] + scale,
    ncol      = 3,
    title     = labels[6],
    title.adj = 0,
    legend    = "      ",
    fill      = colorvec[10],
    border    = colorvec[10],
    bty       = "n",
    pt.cex    = 10,
    cex       = legSize
  )

  return(Leg)
  })
}
