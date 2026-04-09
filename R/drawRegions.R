#' Draw Rejection Regions for Multi-Level T-Tests
#'
#' @description Depicts rejection regions for various t-tests on a chart.
#'   When computing sample size (Design phase), these regions must achieve
#'   the desired power. The tests depend on how many test levels have been
#'   requested and whether there are effects equivalent to zero.
#'
#' @param chartType "Design" for sample size estimation phase, otherwise analysis phase
#' @param alphas 5 x 3 matrix of alpha levels, with possible NA entries
#' @param power 5 x 3 matrix of power levels (zero unless in Design phase)
#' @param MML Lower boundary of minimum meaningful effect magnitude
#' @param MMU Upper boundary of minimum meaningful effect magnitude
#' @param xmin Minimum x-axis value to display (effect size)
#' @param xmax Maximum x-axis value to display (effect size)
#' @param ymin Minimum y-axis value to display (in SE units, even in Design phase)
#' @param ymax Maximum y-axis value to display (in SE units)
#' @param ytick Vector of y-axis values at which tick marks are placed
#' @param DOF Degrees of freedom for t-tests
#' @param colorvec Vector of colors for regions (color or B&W version)
#' @param chartBW TRUE if black & white chart
#' @param ltyBW Vector of line types for non-inferiority/non-superiority in B&W charts
#'
#' @export
drawRegionsVT <- function(chartType,
                          alphas,
                          power,
                          MML,
                          MMU,
                          xmin,
                          xmax,
                          ymin,
                          ymax,
                          ytick,
                          DOF,
                          colorvec,
                          chartBW,
                          ltyBW) {

  # ---------------------------------------------------------------------------
  # CALCULATE TURNING POINTS FOR REJECTION REGION POLYGONS
  #
  # Identify whether each polygon touches SE = ymin or SE = ymax, or instead

  # touches effect size xmin or xmax.
  #
  # Matrices are named for the alternative hypothesis:
  #   inf    = test E >= L (inferiority)
  #   sup    = test E <= U (superiority)
  #   noninf = test E < L (non-inferiority)
  #   nonsup = test E > U (non-superiority)
  #
  # Suffix "0" indicates y = ymin; no suffix indicates y = ymax
  # ---------------------------------------------------------------------------

  # Initialize turning point matrices (4 rows × 2 cols for x,y coordinates)
  inf     <- matrix(nrow = 4, ncol = 2)
  sup     <- matrix(nrow = 4, ncol = 2)
  noninf  <- matrix(nrow = 4, ncol = 2)
  nonsup  <- matrix(nrow = 4, ncol = 2)
  inf0    <- matrix(nrow = 4, ncol = 2)
  sup0    <- matrix(nrow = 4, ncol = 2)
  noninf0 <- matrix(nrow = 4, ncol = 2)
  nonsup0 <- matrix(nrow = 4, ncol = 2)

  # Calculate turning points for each of the 3 potential alpha levels
  for (k in 1:3) {
    # Points at y = ymin
    inf0[k, ]    <- plotpoints(xmin, ymin, xmin, xmax, ymin, ymax, MML, alphas[1, k], power[1, k], DOF)
    sup0[k, ]    <- plotpoints(xmax, ymin, xmin, xmax, ymin, ymax, MMU, alphas[2, k], power[2, k], DOF)
    nonsup0[k, ] <- plotpoints(xmin, ymin, xmin, xmax, ymin, ymax, MMU, alphas[3, k], power[3, k], DOF)
    noninf0[k, ] <- plotpoints(xmax, ymin, xmin, xmax, ymin, ymax, MML, alphas[4, k], power[4, k], DOF)

    # Points at y = ymax
    inf[k, ]    <- plotpoints(xmin, ymax, xmin, xmax, ymin, ymax, MML, alphas[1, k], power[1, k], DOF)
    sup[k, ]    <- plotpoints(xmax, ymax, xmin, xmax, ymin, ymax, MMU, alphas[2, k], power[2, k], DOF)
    nonsup[k, ] <- plotpoints(xmin, ymax, xmin, xmax, ymin, ymax, MMU, alphas[3, k], power[3, k], DOF)
    noninf[k, ] <- plotpoints(xmax, ymax, xmin, xmax, ymin, ymax, MML, alphas[4, k], power[4, k], DOF)
  }

  # Set corner points for polygon closure
  inf[4, ]    <- c(xmin, ymin)
  noninf[4, ] <- c(xmax, ymin)
  sup[4, ]    <- c(xmax, ymin)
  nonsup[4, ] <- c(xmin, ymin)

  # ---------------------------------------------------------------------------
  # DRAW BASE INCONCLUSIVE REGION
  # Mark entire chart area as inconclusive initially
  # ---------------------------------------------------------------------------

  polygon(
    x   = c(xmax, xmax, xmin, xmin),
    y   = c(ymin, ymax, ymax, ymin),
    col = colorvec[length(colorvec) - 1]
  )

  # ---------------------------------------------------------------------------
  # DRAW NON-INFERIORITY / NON-SUPERIORITY WHITE REGIONS
  # Only applicable when margins of material significance differ
  # ---------------------------------------------------------------------------

  if (MMU != MML) {
    # Non-inferiority region (weakest test level)
    k <- which.max(alphas[4, ])
    if (length(k) > 0 && xmax > MML) {
      polygon(
        x      = c(noninf0[k, 1], noninf[k, 1], xmax, xmax, noninf0[k, 1]),
        y      = c(noninf0[k, 2], noninf[k, 2], noninf[k, 2], ymin, ymin),
        col    = "white",
        border = NA
      )
    }

    # Non-superiority region (weakest test level)
    k <- which.max(alphas[3, ])
    if (length(k) > 0 && xmin < MMU) {
      polygon(
        x      = c(nonsup0[k, 1], nonsup[k, 1], xmin, xmin, nonsup0[k, 1]),
        y      = c(nonsup0[k, 2], nonsup[k, 2], nonsup[k, 2], ymin, ymin),
        col    = "white",
        border = NA
      )
    }
  }

  # ---------------------------------------------------------------------------
  # DRAW REJECTION REGIONS FOR EACH ALPHA LEVEL
  # ---------------------------------------------------------------------------
  for (k in 1:3) {

    # --- Inferiority regions ---
    if (xmin < MML && !is.na(alphas[1, k])) {
      polygon(
        x      = c(inf0[k, 1], inf[k, 1], xmin, xmin, inf0[k, 1]),
        y      = c(inf0[k, 2], inf[k, 2], inf[k, 2], ymin, ymin),
        col    = colorvec[k],
        border = NA
      )

      # Overlay with diagonal stripes for B&W charts
      if (chartBW) {
        polygon(
          x       = c(inf0[k, 1], inf[k, 1], xmin, xmin, inf0[k, 1]),
          y       = c(inf0[k, 2], inf[k, 2], inf[k, 2], ymin, ymin),
          density = 5,
          lty     = 1,
          lwd     = 1,
          col     = "white",
          angle   = 45,
          border  = NA
        )
      }
    }

    # --- Superiority regions ---
    if (xmax > MMU && !is.na(alphas[2, k])) {
      polygon(
        x      = c(sup0[k, 1], sup[k, 1], xmax, xmax, sup0[k, 1]),
        y      = c(sup0[k, 2], sup[k, 2], sup[k, 2], ymin, ymin),
        col    = colorvec[k + 3],
        border = NA
      )
    }

    # --- Equivalence regions ---
    # Boundary is approximated in sample size calculations since Type 2 error
    # is a sum of errors above MMU and below MML
    if (MML != MMU && xmax > MML && xmin < MMU) {
      for (k in 1:3) {
        if (!is.na(alphas[5, k])) {
          qtb <- qt(1 - alphas[5, k], DOF)

          # Create 1000 effect sizes in displayed range between MML and MMU
          xe <- max(MML, xmin) + 0:1000 * (min(MMU, xmax) - max(MML, xmin)) / 1000

          # Compute SE at boundary of equivalence region for each effect size
          ye <- pmin((xe - MML) / qtb, (MMU - xe) / qtb)

          # Adjust for power requirement if specified
          if (!is.na(power[5, k]) && power[5, k] > 0.1) {
            qtb3 <- qtb + suppressWarnings(
              qt(1 - (1 - power[5, k] / 100) * (MMU - xe) / (MMU - MML), DOF)
            )
            qtb4 <- qtb + suppressWarnings(
              qt(1 - (1 - power[5, k] / 100) * (xe - MML) / (MMU - MML), DOF)
            )
            ye <- pmin((xe - MML) / qtb4, (MMU - xe) / qtb3)
          }

          # Constrain points to display bounds
          for (i in 1:1001) {
            if (ye[i] < ymin) {
              xe[i] <- NA  # Blank out effect sizes where boundary won't display
            }
            if (ye[i] > ymax) {
              ye[i] <- ymax  # Don't extend beyond ymax
            }
          }

          # Draw equivalence polygon if valid points exist
          if (length(which.max(xe)) > 0) {
            polygon(
              x      = c(max(xmin, MML), xe, min(xmax, MMU)),
              y      = c(ymin, ye, ymin),
              col    = colorvec[k + 6],
              border = NA
            )

            # Overlay with horizontal stripes for B&W charts
            if (chartBW) {
              polygon(
                x       = c(max(xmin, MML), xe, min(xmax, MMU)),
                y       = c(ymin, ye, ymin),
                density = 5,
                lty     = 1,
                lwd     = 1,
                col     = "white",
                angle   = 0,
                border  = NA
              )
            }
          }
        }
      }
    }

    # --- Non-inferiority / Non-superiority boundary lines ---
    lineth <- 3
    col <- "black"

    for (k in 1:3) {
      # Non-superiority boundaries
      if (!is.na(alphas[3, k]) && xmax != nonsup[k, 1] && xmin < MMU) {
        if (!chartBW) {
          col <- colorvec[k + 3]
          lty <- 1
        } else {
          lty <- ltyBW[k + 3]
        }

        lines(
          x   = c(nonsup0[k, 1], nonsup[k, 1]),
          y   = c(nonsup0[k, 2], nonsup[k, 2]),
          col = col,
          lty = lty,
          lwd = lineth
        )
      }

      # Non-inferiority boundaries
      if (!is.na(alphas[4, k]) && xmin != noninf[k, 1] && xmax > MML) {
        if (!chartBW) {
          col <- colorvec[k]
          lty <- 1
        } else {
          lty <- ltyBW[k]
        }

        lines(
          x   = c(noninf0[k, 1], noninf[k, 1]),
          y   = c(noninf0[k, 2], noninf[k, 2]),
          col = col,
          lwd = lineth,
          lty = lty
        )
      }
    }
  }

  # ---------------------------------------------------------------------------
  # DRAW PLOT OUTLINE
  # ---------------------------------------------------------------------------

  polygon(
    x      = c(xmax, xmax, xmin, xmin),
    y      = c(ymax, ymin, ymin, ymax),
    border = "black",
    col    = NA
  )
}
