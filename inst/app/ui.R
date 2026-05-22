# -------------------------------------------------
# UI for 5x3 Tool
# -------------------------------------------------
ui <- page_fluid(

 #-------------------------------------------------
 # Formatting
 #-------------------------------------------------
  theme =  bs_theme(bootswatch = "flatly"),
  lang = "en",

  # Custom styles
  tags$head(
    tags$style(HTML("
      .card { box-shadow: 0 4px 6px -1px rgba(0,0,0,0.1); }
      .plotly .main-svg { border-radius: 0.75rem; }
      .value-box { font-weight: 600; }
      .matrix-input input { font-family: 'JetBrains Mono', monospace; font-size: 12px;}
      .matrix_input th {font-size: 6px;}
      .tooltip { font-size: 0.6rem; }
      .modal-dialog { top: 50%; left: 0%; transform: translateX(-50%);} /* Horizontal center */
      #blurb .accordion-button {font-size: 18px !important; color: black; background-color: lightgray;}
      .noUi-target {height: 6px !important; background: green;}
      
      .noUi-handle {
      opacity: 1 !important;
      width: 24px !important;      
      height: 10px !important;
      top: -13px !important;
      background: transparent !important;
      box-shadow: none !important;
      border: none !important;
      cursor: pointer;
    }
    
    .noUi-handle::before, .noUi-handle::after {
      display: none !important;
    }
    

    .noUi-tooltip {

      font-size: 12px !important;
      padding: 0px 0px !important;
      bottom: 50% !important;           
      background: #1E88E5;
      color: white;
      border: none;}
      /* Arrow pointing down */
    .noUi-tooltip::after {
      content: '';
      position: absolute;
      left: 50%;
      bottom: -6px;
      margin-left: -10px;
      border-left: 4px solid transparent;
      border-right: 4px solid transparent;
      border-top: 4px solid black;
    }
    }

       

  "))
 ),
 tags$style("
    .square-plot {
      width: 50vw;      /* 50% of viewport width */
      height: 50vw;
      max-width: 500px;
      max-height: 500px;

    }
  "),
  tags$style("input[type='text'],input[type='number']{font-size: 12px;}"),
  tags$style("input[type='switch']{font-size: 5px;}"),
  chooseSliderSkin("Modern"),


  #-------------------------------------------------
  # Header card with title and description
  #------------------------------------------------


    card(style = "width: 1215px;",

   
    card_body(
               
    shinyjs::useShinyjs(),
      accordion(id="blurb", open = TRUE,
       accordion_panel("5x3: A Visualization Tool to explore tests and sample sizes before a study, & help interpret findings after data collection.",
       h6("Given an interval [L, U] of effect sizes that are not materially significant (equivalent to zero), the tool considers 5 tests: whether the effect size E is
          at least L, is less than L, is at most U, is more than U, or is between L and U. Tests can be performed at up to 3 alpha levels. For details see Aisbett et al., '5x3: A Practical Approach to Encouraging Thoughtful Statistical Analyses'."),
      h6("Select the Design or Analysis tab depending on your study's stage. Enter desired tests, study design parameters and real or anticipated data in the sidebar, which can be hidden.
         Options to change display colors and legend terminology are below the chart. The legend is draggable. Click anywhere on the page to remove pop-ups."),
      h6("Draw a box in the chart and double click in it to zoom. Double click again to unzoom. Hover on a chart position to get the co-ordinates and the strongest test with desired power (design stage) or strongest rejected test hypothesis (analysis stage)."),
    )),
    actionButton("Manual", "Support documents", class = "btn-small",width=200,height=100), 

  )),
  #-------------------------------------------------
  # Main layout with sidebar
  #-------------------------------------------------
  layout_sidebar(
    border = FALSE,
    sidebar = sidebar(
      width = 400,
      title = tags$strong("5x3 Tool Controls"),
    #-------------------------------------------------
    # Sidebar with controls
    #-------------------------------------------------

      # === Panel to enter Study Description === #
    accordion(
      open = FALSE,
      accordion_panel(
        "Enter study description here.",
          numericInput("Study", h6("Number of groups."), 0.5, 0.5, 1, 0.1),
         helpText("(If performing t-tests on 2 groups, enter proportion in larger group. If a number greater than 1 is entered, F-tests will be applied assuming equal groups.)"),
         
        # Sample size planning inputs (conditional)
                            conditionalPanel("input.tab == 'Design'",
                                             textInput("ES", "Anticipated effect size(s)", anteES),
                                             numericInput("var", "Anticipated variance", anteVar, min = 0.01, step = 0.1),
                                             numericInput("sample", "Feasible or desirable total sample size", anteSS, 4, step = 10)
                            ),

                            # Data analysis inputs (conditional)
                            conditionalPanel("input.tab == 'Analysis'",
                                             h6("Enter summary data. Use comma-separated format if more than one. Data points are charted as 1, 2, 3... in order of entry."),
                                             textAreaInput("dataMean", "Effect size(s)", dataMean, rows = 1),
                                             layout_columns(
                                               textAreaInput("dataV", "SE or variance(s)", dataV, width = 350, height = 70,rows =1),
                                               checkboxInput("VES", "Check if entering variance", VES)
                                             ),
                                             numericInput("sample", "Total sample size", anteSS, 4, step = 10)
                            )
          )
      ),

    #== Panel to Nominate Tests ===#
    
    accordion(id="Alpha", open= FALSE,
            accordion_panel("View or set test directions and alpha levels.")),
    
   #== Panel to Nominate Power in Design stage ===#

    conditionalPanel("input.tab == 'Design'",
                     accordion(id="Power", open= FALSE,
                               accordion_panel("View or set power for nominated tests.")),
                     
    ),# end of test and power entry
   
   #==Panel to Change Displayed Chart ranges==#
   accordion(id="newRange",open=FALSE,
                    accordion_panel("Change chart ranges.",
                 
                        p("Effect size"),
                        layout_columns(
                          numericInput("xmin", "Min", xmin0),
                          numericInput("xmax", "Max", xmax0)),
          
                        conditionalPanel("input.tab == 'Analysis'",
                                         p("Standard error"),
                                         layout_columns(
                                           numericInput("ymin", "Min", ymin0),
                                           numericInput("ymax", "Max", ymax0)
                                         )
                        ),
                        conditionalPanel("input.tab == 'Design'",
                                         p("Total sample size"),
                                         layout_columns(
                                           numericInput("yminSS", "Min", ymin0SS),
                                           numericInput("ymaxSS", "Max", ymax0SS)
                                         )
                        )
                        
                      )
                    ), # end axis control
   
   #== Panel to Change Chart colors ==#
  
   # ====== on switching BW, display changes to monoschrome
   # ====== with other options, a panel will open up (see server code) for users to browse for files containing terms and colors)
   accordion(id="chartColors", open=FALSE,
             accordion_panel("Change to monochrome or other color palette.",
                             switchInput("BW", "Switch to monochrome.", size="mini", value=F,width="800px", labelWidth="200px"),
                             switchInput("chartColor", "Select new color palette.", size="mini", value=F,width="800px", labelWidth="200px"), # a panel will open to switch to B&W display, or to browse for file containing color palette
             ),
   ), # end of color change
   accordion(id="newTerminology", open=FALSE,
             accordion_panel("Use informal terminology.")),
   
    ), # end of control side panel
   
    #-------------------------------------------------
    # === Tabs for Main Panels for Design or Analysis === #
    #-------------------------------------------------
    card(style = "width: 800px;",

         navset_tab(
           id = "tab", selected = firstTab,

          # === Tab 1: Design stage ===
           nav_panel(value = "Design",
                     title = "Design", icon = icon("calculator"),
                     h6(strong("Estimate minimum sample size to achieve desired power over all nominated tests.")),
                     h6("If anticipated effects size(s) have been nominated, a vertical line is charted at each. The color behind a point on the line shows the strongest test with sufficient power at that sample size.
                      Use the slider to set interval [L, U] of values that are not materially significant."),
 
                     uiOutput("Slider"),
                     
                     # ===== Plot output with hover, double click and brush activated=====
                     div(style = "width:350px;color:blue;font-size: 8px;",
                         tags$head(tags$style("#'FigSS{cursor:default}")),
   
                         plotOutput("FigSS", height=500, width=500,

                                    hover = hoverOpts(id = "hoverSS",delay=600, delayType="debounce"), dblclick = "dclickSS",
                                    brush = brushOpts(id = "brushSS",  resetOnNew = TRUE)),

                          #=====  Draggable legend =========
                         absolutePanel(plotOutput("LegendSS"), draggable = TRUE, height=300,width=300,top = 320, right=0),

                     )), # end of Design panel

           # ========== Tab 2: Analysis =============
           nav_panel( value = "Analysis",
                      title = "Analysis", icon = icon("chart-line"),
                      h6(strong("Plot summary data against rejection regions of nominated tests and as confidence intervals.")),
                      h6("The color behind a numbered data point corresponds to the decision shown in the legend. Zoom in if needed. 
                      Use the slider to set interval [L, U] of values that are not materially significant. Select the option below the chart to see multi-level Confidence/Compatibility Intervals."),
                      uiOutput("SliderA"),

                      # ===== Analysis plot output with hover, double click and brush activated=====
                      tags$head(tags$style("#'Fig{cursor:default}")),
                      
                      
                      plotOutput("Fig", height=500, width=500,
                                 click = "click", dblclick = "dclick",hover = hoverOpts(id="hover",delay=600, delayType="debounce"),
                                 brush = brushOpts(id = "brush", resetOnNew = TRUE, delay=300, delayType="debounce")),

                      # ====== Draggable legend
                      absolutePanel(plotOutput("Legend"), draggable = TRUE, height=300,width=300,top = 320, right=0),

                      # =========Confidence intervals are displayed if accordion is opened======
                    br(),
                      accordion(open=FALSE,
                                accordion_panel("Confidence/Compatability Intervals for each alpha.",
                                            jqui_draggable(
                                              plotOutput("CIs"),
                                              options = list(cancel = ".shiny-input-container")
                                            ),
                                            helpText("Longest = strongest test")
                       
                        )
                      ) # end of CI display
                      
                      ), # end of Analysis panel
         )), #end of nav panels


  ))
#========= end of UI ============#
