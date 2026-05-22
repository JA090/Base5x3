#' Draw Multi-Level Confidence Intervals
#'
#' @description Depicts multilevel CIs for effect size measurements.
#'   CIs corresponding to different test levels are drawn over each other,
#'   with the weakest tests (shortest CIs) shown with the thickest lines.
#' @param param List containing  parameters (see details)
#' @details
#' The param list should contain:
#' \describe{
#'   \item{alpha}{Matrix with first row containing alpha values, other rows "*" if one-sided test at that level}
#'   \item{sample}{Total sample size (provisional if computing). Invalid values default to 50}
#'   \item{var}{Estimated variance for sample size estimation}
#'   \item{MML}{Boundaries of minimum meaningful effect magnitudes}
#'   \item{MMU}{ditto}
#'   \item{xmin}{Range of effect sizes to display}
#'   \item{xmax}{ditto}
#'   \item{zmin}{Range of SEs or sample sizes to display}
#'   \item{zmax}{ditto}
#'   \item{chartType}{"Design" for sample size estimation stage, otherwise analysis stage}
#'   \item{Study}{Fraction of sample in larger group (two-group study) or 1 (single-group)}
#'   \item{chartBW}{TRUE for black & white chart}
#'   \item{levels}{vector of names for strengths of tests}
#'   \item{ES}{number of comma separated list of effect sizes}
#'   \item{dataV}{variance of data in Design stage, data SE or variance in Analysis stage}
#'   \item{VES}{TRUE if variance rather than SE in Analysis data}
#'   }
#' @param lab Label for the effect size axis (default: "effect size")
#' @param horizontal TRUE to orient CIs parallel to x-axis (default: TRUE)
#' @param legendPos Legend position using standard plot values (default: "topleft")
#' @importFrom grDevices as.graphicsAnnot xy.coords
#' @importFrom graphics lines par points polygon rect segments strheight strwidth text xinch yinch
#' @importFrom stats qt
#' @importFrom utils read.csv
#' @export
drawCIs <- function(param,
                    lab = "effect size",
                    horizontal = TRUE,
                    legendPos = "topleft") {


with(param, {
     alps=as.numeric(alpha[1,])

  # ---------------------------------------------------------------------------
  # PARSE AND PREPARE INPUT DATA
  # ---------------------------------------------------------------------------

  # Adjust for two-group studies

  if (Study < 1) {
    Study <- sqrt(1 / Study + 1 / (1 - Study))
  }

  # Parse standard errors from comma-separated string
  SE <- suppressWarnings(as.numeric(unlist(strsplit(dataV, split = ","))))

  # Convert variances to SEs if needed
  if (VES) {
    SE <- Study * sqrt(SE / sample)
  }

  # If level names appear to be formal, convert to two-sided (multiply by 2)
 levelName=rep("",length(alps))

    if(any(grepl("p <",levels))|any(grepl("p<",levels)))
      levelName <- paste0(100*(1 - 2 * alps),"% CI")
    else levelName=levels


  # Parse effect sizes from comma-separated string
  m <- as.numeric(unlist(strsplit(ES, split = ",")))
  lm <- length(m)

  # ---------------------------------------------------------------------------
  # CALCULATE TEST PARAMETERS
  # ---------------------------------------------------------------------------

  # Order alpha levels (ascending)
  ord <- order(alps, decreasing = FALSE)

  # Validate and set default sample size
  if (is.na(sample) | sample < 2) {
    sample <- 50
  }

  # Calculate degrees of freedom
  DOF <- sample - 2 + as.integer(Study)

  # Calculate critical points from t-distribution
  cp <- qt(1 - alps / 2, DOF)

  # Determine number of active tests (exclude NA values)
  numTests <- 3
  for (i in 0:1) {
    if (is.na(alps[3 - i])) {
      numTests <- numTests - 1
    }
  }

  # ---------------------------------------------------------------------------
  # SET UP PLOT PARAMETERS
  # ---------------------------------------------------------------------------

  lim <- c(xmin, xmax)
  cex <- 1
  # Color and line width settings
  if (chartBW) {
    col <- c("black", "black", "black")
    lwd <- ord
  } else {
    col <- c("red", "blue", "black")
    lwd <- ord
  }

  # Configure axes based on orientation
  if (horizontal) {
    x <- m
    y <- 1:lm
    xlab <- lab
    ylab <- NA
    side <- 2
    oside <- 1
    xlim <- lim
    ylim <- c(0, lm + 2)
  } else {
    x <- 1:lm
    y <- m
    xlab <- NA
    ylab <- lab
    side <- 1
    oside <- 2
    ylim <- lim
    xlim <- c(0, lm + 2)
  }

  # ---------------------------------------------------------------------------
  # CREATE BASE PLOT
  # ---------------------------------------------------------------------------

  plot(
    x       = x,
    y       = y,
    xlim    = xlim,
    ylim    = ylim,
    xlab    = xlab,
    ylab    = ylab,
    cex.lab = cex,
    axes    = FALSE
  )

  # Add legend
  legend(
    x      = legendPos,
    legend = levelName[1:numTests],
    lwd    = ord,
    col    = col,
    bty    = "n",
    horiz  = TRUE,
    cex    = cex
  )

  # Add axis
  axis(side = oside, tick = TRUE, las = 1, cex = cex)

  # ---------------------------------------------------------------------------
  # DRAW CONFIDENCE INTERVALS
  # ---------------------------------------------------------------------------

  ticklab <- 1:lm

  for (i in 1:lm) {
    # Add tick labels
    mtext(ticklab, side = side, at = 1:lm, las = 1)

    # Set up coordinates for CI caps
    y_pos <- c(i, i)
    yU <- c(i - lm / 30, i + lm / 30)
    yL <- yU

    if (!horizontal) {
      x_pos <- y_pos
      xU <- yU
      xL <- yL
    }

    # Draw CIs for each jj level (strongest to weakest)
    for (jj in numTests:1) {
      # Calculate CI bounds
      l <- m[i] - cp[jj] * SE[i]
      u <- m[i] + cp[jj] * SE[i]

      if (horizontal) {
        x_ci <- c(u, l)
        xL <- c(l, l)
        xU <- c(u, u)

        # Draw  margin lines
        lines(c(MML, MML), c(0, lm+1), lty = 2)
        lines(c(MMU, MMU), c(0, lm+1), lty = 3)
      } else {
        y_ci <- c(u, l)
        yL <- c(l, l)
        yU <- c(u, u)

        # Draw margin lines
        lines(c(0, lm+1), c(MML, MML), lty = 2)
        lines(c(0, lm+1), c(MMU, MMU), lty = 3)
      }

      # Draw CI line
      lines(
        x   = if (horizontal) x_ci else x_pos,
        y   = if (horizontal) y_pos else y_ci,
        lwd = lwd[jj],
        col = col[jj]
      )

      # Draw CI caps (lower and upper bounds)
      lines(xL, yL, lwd = 1, col = col[jj])
      lines(xU, yU, lwd = 1, col = col[jj])
    }
  }
  })
}
