# ==============================
# 5x3  VISUALIZATION TOOL
# ==============================
#' The server code is divided into 8 tasks:
#' 1. Prepare the slider that sets the interval [L,U]. Its range changes if the user changes the default ranges.
#' 2. Manage the axis ranges. These can be explicitly changed by the user, or changed when zooming and unzooming.
#' 3. Manage selection of tests (through a popup displaying a matrix) and of desired power in design phase.
#' 4. Change terminology used in the legend and in display of matrices of tests and powers.
#' 5. Change the chart color scheme.
#' 6. Display user manual
#' 7. Prepare charts and the legend. These depend on many reactive values.
#' 8. Manage display of information about a selected chart point.



server <- function(input, output, session) {

  #====================================================#
  #' 1. Manage slider that defines materially significant effect sizes
  #====================================================#

  # Render the slider
  output$Slider <- renderUI({
   sliderInput("LUX", "", min = slide1, max = slide2, value = sliderVal, step = .01, width = "100%")
  })

  # Dynamically update slider range if axis ranges are changed.
  # Keep the range outside the slider values to avoid automatically resetting them
  observeEvent(list(input$xmin, input$xmax), {
    if (length(input$xmin) >0 ) {
      slide1= min(input$xmin,input$LUX[1])
      slide2= max(input$xmax,input$LUX[2])
      updateSliderInput(session,
                        inputId="LUX",
                        min = slide1,
                        max = slide2,
                        value=c(input$LUX[1],input$LUX[2]),
                        step = max(1, (slide2-slide1) / 10) / 100)
    }})


  # update formal test terms with values for L & U
  observeEvent(input$LUX, {
    for (i in 1:2)
      Labels0[i, 1:5] <<-
        c(
          paste0(prefix[i], "E \u2265 ", input$LUX[1]),
          paste0(prefix[i], "E \u2264 ", input$LUX[2]),
          paste0(prefix[i], "E > ", input$LUX[2]),
          paste0(prefix[i], "E < ", input$LUX[1]),
          paste0(prefix[i], "E > ", input$LUX[1], " & E < ", input$LUX[2])
        )
    if (!input$newTerminology) {
      Labels <<- Labels0
      rownames(inputalps)[2:6] <<-rownames(inputpower) <<-
        Labels0[pos,1:5]

    }
  })




  #====================================================#
  #' 2. Manage variable chart ranges
  #====================================================#


  # Reactive values store dynamic chart ranges which can change through zooming or by changing base range
  ranges <- reactiveValues()

  # ======Change ranges if users select to change them
  observeEvent(list(input$tab, input$xmin, input$xmax, input$ymin, input$ymax, input$yminSS, input$ymaxSS),
               {    ranges$xmin <- input$xmin
                    ranges$xmax <- input$xmax

                #if tab changes between Analysis and Design phases, the y axis units will also change
                 if (input$tab == "Analysis") {
                   ranges$ymin <- input$ymin
                   ranges$ymax <- input$ymax
                 } else {
                   ranges$ymin <- input$yminSS
                   ranges$ymax <- input$ymaxSS
                 }
               })

  #===== Change ranges if users zoom or reset

  # Change for for analysis phase chart
  observeEvent(input$dclick, {
    b <- input$brush
    if (!is.null(b)) {
      ranges$xmin <- b$xmin; ranges$xmax <- b$xmax
      ranges$ymax <- b$ymax; ranges$ymin <- b$ymin
    } else { # reset if double click without brushing
      ranges$xmin <- input$xmin; ranges$xmax <- input$xmax
      ranges$ymin <- input$ymin; ranges$ymax <- input$ymax
    }
  })

  # change for design phase chart
  observeEvent(input$dclickSS, {
    b <- input$brushSS
    if (!is.null(b)) {
      # Adjust variance for two-group design
      fac <- input$var
      if (input$Study < 1) fac <- fac * (1 / input$Study + 1 / (1 - input$Study))
      ranges$xmin <- b$xmin; ranges$xmax <- b$xmax
      ranges$ymax <- fac / (b$ymin)^2; ranges$ymin <- fac / (b$ymax)^2
    } else {
      ranges$xmin <- input$xmin; ranges$xmax <- input$xmax
      ranges$ymin <- input$yminSS; ranges$ymax <- input$ymaxSS
    }

  })

  #========================================================#
  #' 3.  Manage selection of tests (and power settings in design phase)
  #========================================================#

  #=========If change tests selected, open a pop-up

  observeEvent(input$Alpha,  if( input$Alpha) {
    showModal(
      modalDialog(size = "l", value = FALSE,easyClose = TRUE,footer = NULL,
                  div( style = "font-size: 70%;",
                       matrixInput("A2", value = inputalps,
                                   rows = list(names = TRUE),
                                   cols = list(names = F)) ,
                       actionButton("submitAlpha","Submit"),

                  )))
  },ignoreInit=TRUE)

  # ====== On submit, close the popup and change matrices for tests and power(even if power not used)
  observeEvent(input$submitAlpha, {
    removeModal()
    inputalps <<- input$A2
    dd = cleanTables(inputalps, inputpower,input$LUX[2]-input$LUX[1], Levels) #reorganises matrices to keep tests in ascending strength
    inputalps[] <<- matrix(dd[1:18])
    inputpower[] <<- matrix(as.numeric(dd[19:33]))
    colnames(inputpower) <<- inputalps[1, 1:3]
    if (as.numeric(inputalps[1, 1]) > 0)
      for (i in 1:3)
        if(!is.na(as.numeric(inputalps[1, i])))
          {Levels[2, i] <<- paste0("p < ", inputalps[1, i])
          Levels[1, i] <<- paste0("α = ", inputalps[1, i])
        }
  })


  #=== If change power selected, open a pop-up then put new values into power matrix
  observeEvent(input$Power, if (input$Power) {
    showModal(modalDialog(
      size = "l",
      easyClose = TRUE,
      footer = NULL,
      div(
        style = "font-size: 70%;",
        p("Power as %. Entries that are not numbers between 0 and 100 will be converted to 0."),
        matrixInput(
          "power2",
          value = inputpower,
          rows = list(names = TRUE),
          cols = list(names = TRUE),
        ),
        actionButton("submitPower", "Submit"),
      )
    ))
  }, ignoreInit = T)

  observeEvent(input$submitPower, {
    removeModal()
    inputpower <<- input$power2
    suppressWarnings(mode(inputpower)<<- "numeric")
    # clean up table
    for (i in 1:5)
      for (j in 1:3)
        if (is.na(inputpower[i, j]) |
            (100 < inputpower[i, j]) | (inputpower[i, j] < 0))
          inputpower[i, j] <<- 0
  })


  #====================================================#
  #' 4. Manage terminology used in legend
  #====================================================#
  observeEvent(input$newTerminology, {

    #if switch for new terminology turns off, revert to formal terminology in test and power matrix

    if (!input$newTerminology)
    {  Labels <<- Labels0
    rownames(inputalps) <<- c("", Labels[pos, 1:5])
    rownames(inputpower) <<- rownames(inputalps)[2:6]

      # revert to formal terms for test strengths
      for (i in 1:3)
        if (!is.na(inputalps[1, i]) & !(inputalps[1, i] == ""))
          {Levels[2, i] <<- paste0("p < ", inputalps[1, i])
          Levels[1, i] <<- paste0("α = ", inputalps[1, i])
        }
    }
    else
      # if switch is on, a pop-up allows user to enter name of file containing new terminology
    {
      showModal(
        modalDialog(
          size = "l",
          title = "Upload new legend terminology.",
          easyClose = TRUE,
          footer = NULL,
          div(
            style = "font-size: 90%;",
            helpText(
              "File must be .txt or .csv. The first row must contain informal terms for rejection of each of the 5 tests, in the order described in the header. The second row contains terms for test strength, in order of decreasing alpha. The third row contains the term for no nominated test being rejected. See the manual."
            ),
            fileInput("file", "", accept = c("text/csv")),
            actionButton("submitTerms", "Submit"),
          )
        )
      )
    }
  }, ignoreInit = TRUE)

  file_data <- reactiveVal(NULL)
  observeEvent(input$file, {
    file_data(input$file)
  })

  # when user submits file name, read the file and change the terminology
  observeEvent(input$submitTerms, {
    removeModal()
    if (!is.null(file_data())) {
      dd <- read.csv(input$file$datapath, header = FALSE)
      # change row names in the test and power matrices
      dd <- t(dd)
      Labels[2,1:6] <<- c(dd[,1],dd[1,3])
      Labels[1,1:5] <<- paste0("TESTING FOR ", Labels[2,])
      Labels[1,6]<<- "NO TEST HAS ENOUGH POWER"
      rownames(inputalps)<<- c("", Labels[pos,1:5])
      rownames(inputpower) <<- rownames(inputalps)[2:6]
      #new terms for strength of tests/P-values
      Levels[2,] <<-  dd[1:3,2]
      Levels[1, ] <<- gsub("p <|p  <|p<", "α =", Levels[2,])
      file_data(NULL) # Clear the reactive value on reset

    }
  })

  #==============================================#
  #' 5. Manage chart colors
  #==============================================#

  # if monochrome switch on, change color palette

  observeEvent(input$BW, {
    if (input$BW) {
      inputBW <<- T
      colorvec <<- colorvecBW
    }
    else {
      inputBW <<- F
      colorvec <<- colorvec0
    }
  })

  # If user selects to change color palette, a pop-up gets the file name containing the new colors
  observeEvent(input$chartColor, {
    if (input$chartColor)
    {
      showModal(modalDialog(
        size = "l",
        title = "",
        easyClose = TRUE,
        footer = NULL,
        div(
          style = "font-size: 90%;",
          helpText(
            "Input must be .csv or .txt. The first row must contain colors for directional tests about the lower bound L at nominated alphas (in decreasing order).
                         The second row contains colors for the tests about the upper bound. The third row contains colours for rejection of both E > U and E < L.
                           The fourth row contains the colour for the region in which none of the nominated tests is rejected. The last row contains the color of plotted points
                                  (analysis phase) or feasible sample sizes (sample size calculation)."
          ),
          fileInput("fileCol", "", accept = c("text/csv")),
          actionButton("submitCol", "Submit"),
        )
      ))
    }
    #If switch turned to off, revert to initial colors
    else
      colorvec <<- colorvec0

  }, ignoreInit = TRUE)

  Cfile_data <- reactiveVal(NULL)
  observeEvent(input$fileCol, {
    Cfile_data(input$fileCol)
  })

# On submitting the file name, set up the new color scheme
  observeEvent(input$submitCol, {
    removeModal()
    if (!is.null(Cfile_data())) {
      inputBW <<- F
      dd <- read.csv(input$fileCol$datapath, header = FALSE)
      colorvec <<-
        c(t(dd[1, 1:3]), t(dd[2, 1:3]), t(dd[3, 1:3]), dd[4, 1], dd[5, 1])
      Cfile_data(NULL) # Clear the reactive value on reset
    }
  })

#===========================================================
#' 6. Display user manual on request
# ==========================================================
  observeEvent(input$Manual, {
    if (input$Manual)
      showModal(modalDialog(
        title = "",
          selectInput(
            "manual_choice",
            "Choose a document:",
            choices = list(
              "5x3 User Manual" = "manual.pdf",
              "Handbook on working with stakeholders" = "stakeholders.pdf",
              "Formal paper on 5x3" = "paper.pdf"
            )
          ),
          actionButton("open_manual", "Open document in new tab"),
        easyClose = TRUE,
        footer = NULL
      ))})
    # When user clicks inside the modal
    observeEvent(input$open_manual, {
      req(input$manual_choice)
      # Build URL that works locally and on shinyapps.io
      manual_url <- input$manual_choice
      # Open new tab
      runjs(sprintf("window.open('%s', '_blank')", manual_url))
      # Close the modal
      removeModal()
    })




  #====================================================#
  #' 7. Prepare charts and legends
  #====================================================#
  observeEvent(input$tab, {
    if (input$tab == "Analysis")
      pos=1
    else
      pos = 2
  })

  observeEvent(
    list(
      input$submitTerms,
      input$submitAlpha,
      input$Alpha,
      input$submitPower,
      input$LUX,
      input$chartColor,
      input$BW,
      Levels,
      input$newTerminology,
      file_data(),
      input$file,
      input$tab,
      input$submitCol,
      ranges$xmin,ranges$ymin,
      ranges$xmax,ranges$ymax
    ),
    #only proceed if interval equivalent to zero is set

    if (length(input$LUX[1]) > 0) {
      # prepare to draw charts
      pos <- if (input$tab == "Analysis") 2 else 1

      param=list(alpha=inputalps,
                 power=inputpower,
                 sample=input$sample,
                 MML=input$LUX[1],
                 MMU=input$LUX[2],
                 xmin=ranges$xmin,
                 xmax=ranges$xmax,
                 zmin=ranges$ymin,
                 zmax=ranges$ymax,
                 chartType=input$tab,
                 Study=input$Study,
                 chartBW=inputBW,
                 ES=input$ES,
                 dataV=input$var,
                 VES=input$VES,
                 levels=Levels[pos,],
                 labels=Labels[pos,],
                 colorvec=colorvec,
                 ltyBW=ltyBW)
      if (pos == 2) {
        param$power = matrix(data = NA,
                             nrow = 5,
                             ncol = 3)
        param$ES = input$dataMean
        param$dataV = input$dataV
      }
      #======= Draw legend as separate draggable panel=======#

      output$Legend <<- output$LegendSS <<- renderPlot({
        par(mar = c(0, 0, 0, 0))
        drawLegend(param)

      }
      , height = 300, width = 400)


      # ============ Design chart===================#
      if (input$tab == "Design")
        output$FigSS <- renderPlot({
          buildPlot(param)

        }, height = 500, width = 500) #end of design phase chart

       else
       {
      #==========Analysis chart & CI render ==================#

        output$Fig <- renderPlot({
          BP<-buildPlot(param)
          print(BP)
        }, height = 500, width = 500) # end of analysis chart

        # ==========chart of Confidence Intervals, which are optionally shown

        output$CIs <- renderPlot({
          if (length(input$dataMean) > 0 && length(input$LUX) > 0) {
            CI <- drawCIs(param,
              lab = "effect size",
              legendPos = "topleft"
            )
          }
          print(CI)}, height = 300, width = 600)
      }
    })

  #====================================================#
  #' 8. Manage pop-ups giving point information from charts
  #====================================================#

  #==== Pop-up info for analysis phase is the strongest decision at selected effect size and standard error, plus the 95% confidence interval
  observeEvent(input$hover, {
    g <-
      as.character(
        clickPointsVT(
          input$hover$x,
          input$hover$y,
          input$sample,
          input$tab,
          input$Study
        )
      )
    txt1 <- paste0("Effect size: ", g[1], ", Standard error: ", g[2])
    txt2 <- paste0("95% CI: (", g[3], ",", g[4], ")")
    txt3 <- presentDecision(
      input$hover$x,
      input$hover$y,
      inputalps,
      input$tab,
      matrix(15 * 0, nrow = 5, ncol = 3),
      input$LUX[1],
      input$LUX[2],
      as.integer(input$sample) - 2 + as.integer(input$Study),
      Levels[2,],
      Labels[2,]
    )

    showModal(modalDialog(
      size = "s",
      easyClose = TRUE,
      footer = NULL,
      div(style = "font-size: 90%;",
          p(txt1), br(), p(txt2), br(), p(txt3))
    ))
  }, ignoreInit = TRUE)

  # =====Pop-up info for design phase is the strongest test with required power at selected effect size and sample size ==========#

  observeEvent(input$hoverSS, {
    g <-
      as.character(
        clickPointsVT(
          input$hoverSS$x,
          input$hoverSS$y,
          input$var,
          input$tab,
          input$Study
        )
      )
    txt1 <- paste0("Effect size: ", g[1], ", Sample size: ", g[2])
    txt2 <- presentDecision(
      input$hoverSS$x,
      input$hoverSS$y,
      inputalps,
      input$tab,
      inputpower,
      input$LUX[1],
      input$LUX[2],
      as.integer(input$sample) - 2 + as.integer(input$Study),
      Levels[1,],
      Labels[1,]
    )
    showModal(modalDialog(
      size = "s",
      easyClose = TRUE,
      footer = NULL,
      div(style = "font-size: 90%;",
          p(txt1), br(), p(txt2))
    ))
  }, ignoreInit = TRUE)



}
