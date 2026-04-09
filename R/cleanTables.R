#' Cllean and reorganise tables of alpha levels and power
#' @description Function to reorder tables so larger column numbers have lower alphas, and all alphas are between 0 and 0.5.
#' @param alpha -matrix table in which first row is alphas and  entries are "*" or null. Other entries will be converted to nulls
#' @param power -matrix of power(0 if in analysis phase)
#' @param MM -difference between upper and lower boundaries of interval equivalent to zero/not materially significant
#' @param level - name for finding at each level
#'
 cleanTables <-
  function(alpha, power,MM, level) {

    #' Clean up test table.
    #'
    #' Unless a column is headed by a number between 0 and .5, blank out the column
    for (j in 1:3) {
      if (is.na(alpha[1, j]))
        alpha[1, j] = ""
      else if (is.na(as.numeric(alpha[1, j])))
        alpha[1, j] = ""
      else if ((as.numeric(alpha[1, j]) >= .5) |
               (as.numeric(alpha[1, j]) <= 0))
        alpha[1, j] = ""
    }
    for (j in 1:3) {
      if (alpha[1, j] == "")
        alpha[, j] = ""
      else
        for (i in 2:6)
          if (!alpha[i, j] == "*")
            alpha[i, j] = ""
    }

    #' Ensure composite test is at the same alpha
    for (j in 1:3) {
      if (!(alpha[4, j] == "*") | !(alpha[5, j] == "*"))
        alpha[6, j] = ""
    }

    #' delete columns with no tests specified
    for (j in 1:3) {
      hit = NA
      for (i in 2:6) {
        if (alpha[i, j] == "*")
          hit = 1
      }
      if (is.na(hit)) {
        alpha[, j] = ""
        power[, j] = 0
      }
    }

   #' if MM= 0, delete everything but tests for non-superiority and non-inferiority
   if (MM == 0) for (j in 1:3) {
    for (i in 4:6) {
      alpha[i, j] = ""
      power[i-1, j] = 0
    }
  }

    #reorder columns if necessary; keep level name
    ord = order(as.numeric(alpha[1, ]), decreasing = TRUE)
    alpha <- alpha[, ord]
    level<- level[, ord]
    power[, 1:3] <- power[, ord]


    return(c(alpha, as.numeric(power)))
  }
