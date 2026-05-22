# Function to draw vertical lines at anticipated effect sizes in Design stage

drawLines <- function(ES,xmin,xmax,ymin, ymax,col){
if (!is.na(ES)) {
  es_vals <-
    suppressWarnings(as.numeric(unlist(strsplit(ES, split = ","
    ))))
  for (x in es_vals) {
    if (x > xmin && x < xmax) {
      lines(c(x, x), c(ymin,ymax), lwd = 2, col = col)
    }
  }
}
}
