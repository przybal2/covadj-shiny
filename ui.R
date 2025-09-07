
library(shiny)
library(shinyMatrix)

# Define UI
ui <- fluidPage(
  titlePanel("Covariate Adjustment in RCTs"),
  tags$head(
    tags$style(HTML("
    .left-mathjax .MathJax_Display {
      text-align: left !important;
      margin-left: 0 !important;
    }
  "))
  ),
  tags$p("Explore the impact of covariate adjustment on treatment effect estimation for a simple simulated trial. 
         Use the tabs below to analyse continuous and binary outcomes."),
  tags$div(
    style = "position: absolute; top: 10px; right: 20px; z-index: 1000;",
    actionButton("about_btn", "About", icon = icon("info-circle"), class = "btn-dark"),
    actionButton("learnings_btn", "Learnings Checklist", icon = icon("check-square"), class = "btn-success"),
  ),
  tabsetPanel(
    
    # Tab for Continuous Outcome
    
    tabPanel("Continuous Outcome",
             sidebarLayout(
               sidebarPanel(
                 
                 fluidRow(
                   column(10,
                          numericInput("sampleSize", 
                                       "Sample size:", 
                                       value = 100, min = 10, max=10000, step = 100)
                   ),
                   column(2, 
                          actionButton("infoSampleSize", "", icon=icon("circle-info"), class = "btn-dark btn-xs")
                   )
                 ),
                 
                 fluidRow(
                   column(10,
                          numericInput("treatmentEffect", 
                                       "True treatment effect:", 
                                       value = 0.5, step = 0.1)
                   ),
                   column(2,
                          actionButton("infoTrtEffect", "", icon=icon("circle-info"), class = "btn-dark btn-xs")
                   )
                 ),
                 
                 fluidRow(
                   column(10,
                          sliderInput("correlation", 
                                      "Baseline covariate prognostic strength:", 
                                      min = 0, max = 1, value = 0.6, step = 0.1)
                   ),
                   column(2,
                          actionButton("infoCorrelation", "", icon=icon("circle-info"), class = "btn-dark btn-xs")  # Info button
                   )
                 ),
                 
                 
                 tags$br(),
                 tags$script(HTML("
                    Shiny.addCustomMessageHandler('toggleNumeric', function(message) {
                      var el = document.getElementById(message.id);
                      if (el) {
                        el.disabled = message.disabled;
                      }
                    });
                  ")),
                 
                 fluidRow(
                   column(
                     width = 5,
                     checkboxInput("fix_seed", "fix seed?", value = FALSE),
                     numericInput("rseed", label = "Seed:", value = 123, width = "100%")
                   ),
                   column(
                     width = 5,
                     div(style = "height: 70px;"),
                     actionButton("simulate", "Simulate Data")
                   ),
                   column(
                     width = 2,
                     div(
                       style = "padding-top: 10px;",
                       actionButton("infoRseed", "", icon=icon("circle-info"), class = "btn-dark btn-xs")
                     )
                   )
                 )
                 
                 
               ),
               mainPanel(
                 uiOutput("cont_intro"),
                 
                 plotOutput("forestPlot"),
                 DT::DTOutput("effectTable"),
                 tags$br(),
                 tags$br(),
                 plotOutput("scatterPlot"),
                 tags$br(),
                 plotOutput("densityPlot")
                 
               )
             )
    ),
    
    # Tab for Binary Outcome
    
    tabPanel("Binary Outcome",
             sidebarLayout(
               sidebarPanel(
                 
                 fluidRow(
                   column(10,
                          numericInput("binarySampleSize", 
                                       "Sample size:", 
                                       value = 100, min = 10, max=10000, step = 100),
                   ),
                   column(2,
                          actionButton("infoBinarySampleSize", "", icon=icon("circle-info"), class = "btn-dark btn-xs")
                   )
                 ),
                 
                 
                 fluidRow(
                   column(10,
                          matrixInput(
                            inputId = "trt_matrix",
                            label = "Treatment effect (outcome response rates per level of binary covariate X)",
                            value = matrix(c(80, 25, 33.3, 4), nrow = 2, 
                                           dimnames = list(c("X=1", "X=0"),
                                                           c("P(Y=1 | treatment)", "P(Y=1 | placebo)"))),
                            rows = list(names = TRUE),
                            cols = list(names = TRUE),
                            class = "numeric"
                          )
                   ),
                   column(2,
                          actionButton("infoBinaryTrt", "", icon=icon("circle-info"), class = "btn-dark btn-xs")
                   )
                 ),
                 
                 
                 tags$br(),
                 tags$script(HTML("
                    Shiny.addCustomMessageHandler('toggleNumeric', function(message) {
                      var el = document.getElementById(message.id);
                      if (el) {
                        el.disabled = message.disabled;
                      }
                    });
                  ")),
                 fluidRow(
                   column(
                     width = 5,
                     checkboxInput("fix_seed_binary", "fix seed?", value = FALSE),
                     numericInput("rseed_binary", label = "Seed:", value = 123, width = "100%")
                   ),
                   column(
                     width = 5,
                     div(style = "height: 70px;"), # empty space 
                     actionButton("simulateBinary", "Simulate Data")
                   ),
                   column(
                     width = 2,
                     div(
                       style = "padding-top: 10px;",
                       actionButton("infoBinaryRseed", "", icon=icon("circle-info"), class = "btn-dark btn-xs")
                     )
                   )
                 )
                 
               ),
               mainPanel(
                 DT::DTOutput("binaryTableOne"),
                 tags$br(),
                 
                 uiOutput("binaryEstimandExplanation"),

                 uiOutput("binaryEffectExplanation"),
                 
                 uiOutput("binary_intro"),
                 
                 plotOutput("binaryForestPlot"),
                 # Toggle for OR/logOR scale
                 fluidRow(
                   column(12,
                          radioButtons(
                            inputId = "binary_forest_scale",
                            label = "X-axis scale:",
                            choices = c("Odds ratio" = "or", "Log odds ratio" = "logor"),
                            selected = "or",
                            inline = TRUE
                          )
                   )
                 ),
                 
                 tags$br(),
                 DT::DTOutput("binaryEffectTable"),
                 tags$br(),
                 
                 uiOutput("binaryCounterfactualSection"),
                 tags$br(),
                 
                 plotOutput("binaryBarPlotImbalance")
                 
               )
             )
    )
  )
)