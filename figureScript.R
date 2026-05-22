# =============================================================================
# Script: Build PDF Figures
# Purpose: Generate figures 1, 2,4-8 for Aisbett et al.,
#          "5x3: A Practical Approach to Encouraging Thoughtful Statistical Analysis"
#Note: Fig 3 is generated from text boxes in MS word
# =============================================================================

# --- Load package functions ---
devtools::load_all()


# -----------------------------------------------------------------------------
# TWO-PANEL FIGURES
# -----------------------------------------------------------------------------

# Figure 1: Two panels, horizontal layout
pdf("Fig1.pdf", width = 13, height = 5)
  buildFigure(param = Fig1a, param2 = Fig1b)
dev.off()


# Figure 2: Two panels, horizontal layout
pdf("Fig2.pdf", width = 13, height = 5.5)
  buildFigure(param = Fig2a, param2 = Fig2b)
dev.off()

# Figure 5: Two panels, horizontal layout, using first chart legend
pdf("Fig5.pdf", width = 13, height = 5.5)
buildFigure(param = Fig4a,param2 = Fig4b, legend = 1)
dev.off()


# Figure 5: Two panels, horizontal layout with two legends
pdf("Fig6.pdf", width = 7, height = 9)
  buildFigure(param = Fig5a, param2 = Fig5b, legend = 3)
dev.off()

# -----------------------------------------------------------------------------
# SINGLE-PANEL FIGURES
# -----------------------------------------------------------------------------

# Figure 7: Single panel
pdf("Fig7.pdf", width = 9, height = 5.5)
  buildFigure(param = Fig6, legend=T)
dev.off()

# Figure 8: Single panel
pdf("Fig8.pdf", width = 9, height = 5.5)
  buildFigure(param = Fig7)
dev.off()

# -----------------------------------------------------------------------------
# FIGURE 4: CUSTOM CONFIDENCE INTERVAL PLOT
# -----------------------------------------------------------------------------

pdf("Fig4.pdf", width = 8, height = 6)
  drawCIs(param = Fig2a)
dev.off()
