#' Presents summary data for a point selected on the chart created in buildPlot
#' @description FUNCTION to determine which rejection region a selected point lies in
#' @description When computing sample size, these regions must have the desired power.
#' @description The tests depend on how many test levels have been requested and whether there are effects equivalent to 0.
#' @param xm co-ordinates of points (xm = effect size, y = SE (even in sample size computation).
#' @param ym as above
#' @param alphas 6 x 3 matrix with alpha levels in first row, "*" entry where a test at the alpha isdesired.
#' @param chartType Design or Analysis
#' @param power 5 x 3 numeric matrix of % power levels.
#' @param MML Minimum meaningful effect boundaries
#' @param MMU as above
#' @param power (zero unless doing sample size calculation).
#' @param DOF for t-tests.
#' @param levels terms for strength of tests at different alphas/findings at different p values
#' @param labels terms for tests/findings

presentDecision <-
  function(xm, ym,
           alphas,
           chartType,
           power,
           MML, MMU,
           DOF,
           levels,
           labels)

  {
    response = "no point selected"
    if (!is.null(xm)) {

  # ===== only proceed if valid point


  #======Initialise
      response=labels[6] # if nothing conclusive

        if (chartType == "Analysis")
          start = "Strongest decision: "
        else  if (chartType == "Design")
          start = "Strongest test with sufficient power: "

        xm = as.numeric(xm)
        ym = as.numeric(ym)
        MML = as.numeric(MML)
        MMU = as.numeric(MMU)
        qtb = matrix(15 * 0, nrow = 5, ncol = 3)

        for (k in 1:5)
          for (j in 1:3) {
            if (!(is.numeric(power[k, j])) | power[k, j] < 0.1)
              qtb[k, j] = 0
            else
              qtb[k, j] = suppressWarnings(qt(power[k, j]/100, DOF))
          }

  # ====== now look for strongest test at point (with sufficient power in design chartType)

        cat = 0

        if (xm <= MML) {
        #======= check if inferiority or non-superiority test satisfied

          # first do inferiority, as a stronger test
          for (j in 1:3) {
            if (alphas[2, j] == "*")
              if (ym < (-xm + MML) / (qt(1 - as.numeric(alphas[1, j]), DOF) +
                                      qtb[1, j]))
                cat = j
          }
          if (cat > 0)
            response = paste0(start, labels[1]," at level ", levels[cat]) #start, " E > L at p < ", as.numeric(alphas[1, cat]))
          # if no inferiority test satisfied, try non-superiority
          else
          {
            for (j in 1:3) {
              if (alphas[4, j] == "*")
                if (ym < (-xm + MMU) / (qt(1 - as.numeric(alphas[1, j]), DOF) +
                                        qtb[3, j]))
                  cat = j
            }
            if (cat > 0) {
              response = paste0(start, labels[3]," at level ",levels[cat])
            }
          }
        }
       #----------end of inferiority checks

        if (xm >= MMU) {
          #====== check if any superiority or non-inferiority test satisfied
          for (j in 1:3) {
            if (alphas[3, j] == "*")
              if (ym < (xm - MMU) / (qt(1 - as.numeric(alphas[1, j]), DOF) +
                                     qtb[2, j]))
                cat = j
          }

          if (cat > 0)
            response = paste0(start, labels[2]," at level ",levels[cat])
          else
          {
            for (j in 1:3) {
              if (alphas[5, j] == "*")
                if (ym < (xm - MML) / (qt(1 - as.numeric(alphas[1, j]), DOF) +
                                       qtb[4, j]))
                  cat = j
            }
            if(cat > 0)
              response = paste0(start, labels[4]," at level ",levels[cat])
          }
        }
    #---------------- end of superiority checks

        if (xm > MML)
          if (xm < MMU) {

            #======= check for equivalence, noting in the design chartType a sample size may have sufficient power to reject E< L and E>U but not both at the same time

            for (j in 1:3) {
              if (alphas[6, j] == "*") {
                qta = qt(1 - as.numeric(alphas[1, j]), DOF)
                if (ym < (-xm + MMU) / (qta + qtb[5, j]))
                  if (ym < (xm - MML) / (qta + qtb[5, j]))
                  {
                    # still may not have sufficient power
                    ye = pmin((xm - MML) / qta, (MMU - xm) / qta)
                    if (!is.na(power[5, j]))
                      if (power[5, j] > .001) {
                        qta3 = qta + suppressWarnings(qt(1 - (1 - power[5, j]/100) * (MMU - xm) / (MMU -
                                                                                                 MML), DOF))
                        qta4 = qta + suppressWarnings(qt(1 - (1 - power[5, j]/100) *
                                                           (xm - MML) / (MMU - MML), DOF))
                        ye = pmin((xm - MML) / qta4, (MMU - xm) / qta3)
                      }
                    if (ym <= ye)
                      cat = j
                  }
              }
            }
            if (cat > 0)
              response = paste0(start, labels[5]," at level ",levels[cat])
          }
      }

      return(response)
    }

