# =============================================================================
# Script: Build PDF Figures
# Purpose: Generate all figures for Aisbett et al.,
#          "5x3: A Practical Approach to Encouraging Thoughtful Statistical Analysis"
# To run: Unzip R.zip into folder R, unzip Data.zip into folder , unzip www.zip into folder www
# =============================================================================

# --- Source functions ---
my_f <- list.files(path="R", pattern= "\\.R$|\\.r$", full.names=T)
sapply(my_f,source)

# --- Get data ---
my_data <- list.files(path="Data", pattern= "\\.rda$", full.names=T)
sapply(my_data,load, envir=globalenv())



# -----------------------------------------------------------------------------
# TWO-PANEL FIGURES
# -----------------------------------------------------------------------------

# Figure 1: Two panels, horizontal layout
pdf("Fig1.pdf", width = 13, height = 5)
  build_figure(param = Fig1a, param2 = Fig1b)
dev.off()


# Figure 2: Two panels, horizontal layout
pdf("Fig2.pdf", width = 13, height = 5.5)
  build_figure(param = Fig2a, param2 = Fig2b)
dev.off()

# Figure 5: Two panels, horizontal layout, using first chart legend
pdf("Fig5.pdf", width = 13, height = 5.5)
build_figure(param = Fig5a,param2 = Fig5b, legend = 1)
dev.off()


# Figure 6: Two panels, horizontal layout with two legends
pdf("Fig6.pdf", width = 7, height = 9)
  build_figure(param = Fig6a, param2 = Fig6b, legend = 3)
dev.off()

# -----------------------------------------------------------------------------
# SINGLE-PANEL FIGURES
# -----------------------------------------------------------------------------

# Figure 7: Single panel
pdf("Fig7.pdf", width = 9, height = 5.5)
  build_figure(param = Fig7, legend=T)
dev.off()

# Figure 8: Single panel
pdf("Fig8.pdf", width = 9, height = 5.5)
  build_figure(param = Fig8)
dev.off()

# -----------------------------------------------------------------------------
# FIGURE 4: CUSTOM CONFIDENCE INTERVAL PLOT
# -----------------------------------------------------------------------------

pdf("Fig4.pdf", width = 8, height = 6)
  drawCIs(param = Fig2a)
dev.off()
# -----------------------------------------------------------------------------
# FIGURE 3: ANNOTATED SCREEN SHOT
# -----------------------------------------------------------------------------

if (!require("jpeg")) install.packages("jpeg"); library("jpeg")
if (!require("plotrix")) install.packages("plotrix"); library("plotrix")
pdf("Fig3.pdf",width=7,height=7.2,pointsize=18)
par(mar=c(0,0,0,0))
plot(x=c(-1,10), y=c(-1,10),type="n",axes=FALSE, ann=FALSE, xaxs="i", yaxs="i")
img <- readJPEG("data/overview.jpg")
xl=2; xr=9.71;yb=1;yt=(9.5*7.71/8.71)
rasterImage(img, xleft=xl,ybottom=yb,xright=xr,ytop=yt)
textbox(x=c(xl+1,xl+3),y=yt+.5, textlist=c("Tabs to select stage of research"),border=NA,cex=.5)
arrows(x0=xl+1.9,x1=xl+3.5,y0=yt+.1,y1=yt-2.4, col="darkblue",lwd=1.3,length=.07)
textbox(x=c(xl+3.3,xl+5),y=yt+1, textlist=c("Slider to set margins of material significance"),border=NA,cex=.5)
arrows(x0=xl+4.1,x1=xl+4.1,y0=yt+.2,y1=yt-3.25, col="darkblue",lwd=1.3,length=.07)
textbox(x=c(xr-2,xr),y=yt+.5, textlist=c("Closable pane with overview of the tool"),border=NA,cex=.5)
arrows(x0=xr-1,x1=xr-1,y0=yt+.1,y1=yt-.3, col="darkblue",lwd=1.3,length=.07)
textbox(x=c(xl+1.5,xl+3),y=yb-1, textlist=c("Chart"),border=NA,cex=.5)
arrows(x0=xl+2,x1=xl+3,y0=yb-.8,y1=yb+1.2, col="darkblue",lwd=1.3,length=.07)
textbox(x=c(xr-1.5,xr),y=yb-1, textlist=c("Draggable index"),border=NA,cex=.5)
arrows(x0=xr-1,x1=xr-1.3,y0=yb-.8,y1=yb+1.5, col="darkblue",lwd=1.3,length=.07)
textbox(x=c(xl+3.5,xl+6),y=yb-.2, textlist=c("Option to show multi-level CIS", "(offered only with Analysis tab)"),border=NA,cex=.5)
arrows(x0=xl+4.5,x1=xl+4.5,y0=yb-.2,y1=yb+.35, col="darkblue",lwd=1.3,length=.07)
textbox(x=c(xl-2.4,xl-.2),y=yt, textlist=c("Button to select documents to view or download"),border=NA,cex=.5)
arrows(x0=xl-.9,x1=xl+.1,y0=yt-.4,y1=yt-1.8, col="darkblue",lwd=1.3,length=.07)
textbox(x=c(xl-2.8,xl-.5),y=yt-1.7, textlist=c("Closable control pane with sub-panes to:"),border=NA,cex=.5)
arrows(x0=xl-1,x1=xl+.1,y0=yt-2,y1=yt-2.2, col="darkblue",lwd=1.3,length=.07)
textbox(x=c(xl-2.5,xl),y=yt-2.4, textlist=c("A. Describe study"),border=NA,cex=.5,font=3)
arrows(x0=xl-.8,x1=xl,y0=yt-2.5,y1=yt-2.8, col="darkblue",lwd=1.3,lty=2,length=.07)
textbox(x=c(xl-2.5,xl),y=yt-3.0, textlist=c("B. Select tests"),border=NA,cex=.5,font=3)
arrows(x0=xl-1.1,x1=xl,y0=yt-3.1,y1=yt-3.3, col="darkblue",lwd=1.3, lty=2,length=.07)
textbox(x=c(xl-2.5,xl-.5),y=yt-3.6, textlist=c("C. Change ranges","displayed on chart"),border=NA,cex=.5,font=3)
arrows(x0=xl-.8,x1=xl,y0=yt-3.8,y1=yt-3.8, col="darkblue",lwd=1.3,lty=2,length=.07)
textbox(x=c(xl-2.5,xl-.5),y=yt-4.4, textlist=c("D. Change chart color palette"),border=NA,cex=.5,font=3)
arrows(x0=xl-.7,x1=xl,y0=yt-4.4,y1=yt-4.3, col="darkblue",lwd=1.3,lty=2,length=.07)
textbox(x=c(xl-2.5,xl-.5),y=yt-5.1, textlist=c("E. Change the terminology used in the charts & elsewhere"),border=NA,cex=.5,,font=3)
arrows(x0=xl-1,x1=xl,y0=yt-5.2,y1=yt-5, col="darkblue",lwd=1.3,lty=2,length=.07)
dev.off()

