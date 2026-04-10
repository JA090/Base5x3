#' Build PDF Figure for 5x3 Statistical Analysis Charts
#'
#' @description Function to compose figures such as those in Aisbett et al.,
#'   "5x3: A Practical Approach to Encouraging Thoughtful Statistical Analysis"
#' @param param List containing chart parameters (see Details)
#' @param param2 Optional second parameter list for two-panel figures (default: NULL)
#' @details
#' The param and param2 lists should contain:
#' \describe{
#'   \item{alpha}{Matrix with first row containing alpha values, other rows "*" if one-sided test at that level}
#'   \item{power}{Matrix of powers for sample size estimation, else 0}
#'   \item{sample}{Total sample size (provisional if computing). Invalid values default to 50}
#'   \item{MML, MMU}{Boundaries of minimum meaningful effect magnitudes}
#'   \item{xmin, xmax}{Range of effect sizes to display}
#'   \item{zmin, zmax}{Range of SEs or sample sizes to display}
#'   \item{chartType}{"Design" for sample size estimation phase, otherwise analysis phase}
#'   \item{Study}{Fraction of sample in larger group (two-group study) or 1 (single-group)}
#'   \item{chartBW}{TRUE for black & white chart}
#'   \item{colorvec}{Vector of colors for regions and data points}
#'   \item{ltyBW} line type for B&W charts
#'   \item{ES} number of comma separated list of effect sizes
#'   \item{dataV} variance of data (or possible SE in Analysis phase)
#'   \item{VES} TRUE if SE rather than variance
#'   \item{labels} terminology for the 5 tests
#'   \item{levels} terminology for the test levels (up to 3)
#'   }
#' @param legend If two-chart figure, use legend from first chart if 1, second chart if 2, or both if 3
#' @param arrangement Layout option: T = vertical stack with legend on right,F = horizontal row (default: 2)
#' @return nothing - called for its creation of a plot
#' @export
build_figure <- function(param,
                        param2 = NULL,
                        legend = 2,
                        arrangement = F) {
  # ---------------------------------------------------------------------------
  # POSITION FIRST PLOT
  # ---------------------------------------------------------------------------

  if (is.null(param2)) {
    # Single plot: use left portion of figure
    par(fig = c(0, 0.555, 0, 1))
  } else {
    if (arrangement | legend == 3)
      # Vertical arrangement: first plot in upper half
      par(fig = c(0, 0.555, 0.5, 0.975))
    else
      # Horizontal arrangement: first plot on left
      par(fig = c(0, 0.375, 0, 1))
  }
  par(cex = 0.9)
  buildPlot(param)


  # ---------------------------------------------------------------------------
  # POSITION SECOND PLOT (if provided)
  # ---------------------------------------------------------------------------

  if (is.null(param2))
    # Single plot: position legend to the right
    par(fig = c(0.43, 1, 0, 1), new = TRUE)
  else {
    # Label the first plot
    mtext("(a)",
          side = 3,
          line = 1.3,
          cex = 1.3)

    # Position second plot based on arrangement
    if (arrangement | legend == 3)
      # Vertical: second plot in lower half
      par(fig = c(0, 0.555, 0.025, 0.5), new = TRUE)
    else
      # Horizontal: second plot in middle
      par(fig = c(0.375, 0.75, 0, 1), new = TRUE)

    buildPlot(param2)

    # Label second plot
    mtext(
      "(b)",
      side = 3,
      adj = 0.5,
      line = 1.3,
      cex = 1.3
    )
    par(cex = 0.9)
    #position and draw first legend if both plots will have a legend
    if (legend == 3) {
      par(fig = c(.46, 1, .5, .975), new = TRUE)
      drawLegend(param)
    }
    # Position legend (centered between plots for two-panel figures)
    if (!legend == 3) {
      if (arrangement)
        par(fig = c(0.46, 1, 0.1, 0.75), new = TRUE)
      else
        par(fig = c(0.7, 1, 0, 1), new = TRUE)
    }
    else
      par(fig = c(.46, 1, 0, .5), new = TRUE) # second legend
  }

  # ---------------------------------------------------------------------------
  # DRAW LEGEND
  # ---------------------------------------------------------------------------

  par(cex = 0.9)
  legparam <- if (legend == 1 | is.null(param2))
    param
  else
    param2
  drawLegend(legparam)
}
