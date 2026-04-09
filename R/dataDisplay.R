#' FUNCTION to display data as integers on chart generated in buildPlot.
#' @description study data are plotted as points
#' @param datamean Comma-separated text of study effect sizes.
#' @param dataV Comma-separated text of study SEs or variances
#' @param VES true if variances in above list
#' @param sample total sample size
#' @param r - fraction of sample in larger group if 2 group
#' @param xmin effect size range
#' @param xmax ditto
#' @param ymin SE range
#' @param ymax ditto
#' @param cex size of numeral used to plot data
#' @param ptcol color to use for numbered points

#' @description
datadisplay <- function(
                        datamean,
                        dataV,VES, sample,r,
                        ptcol,cex,
                        xmin,xmax,ymin, ymax
                      )
{

  mean = suppressWarnings(as.numeric(unlist(strsplit(datamean, split = ","))))  #user-entered effect sizes (or effect size/SE pairs) to display
  if (r<1) r=sqrt(1/r+1/(1-r) )
  SE = suppressWarnings(as.numeric(unlist(strsplit(dataV, split = ",")))) #user-entered effect sizes (or effect size/SE pairs) to display
   if (VES) SE=r*sqrt(SE/sample)
    #' plot points on chart of effect size vs SE.
    if(length(mean)>0)
   for (i in 1:(length(mean) )) {
      m = mean[i]
     s=SE[i]

         if ((m>=xmin) & (m <= xmax) & (s <= ymax) & (s >=ymin)) {
           points(m,s,pch =as.character(i),cex = cex,col = ptcol)
           }

    }
}
