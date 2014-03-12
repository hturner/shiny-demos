## user interface for linear regression app
shinyUI(pageWithSidebar(
    ## title, possibly also tab name (in browser)
    headerPanel("Linear Regression"),
    ## use Chrome to see up/down arrows on numeric input boxes
    sidebarPanel(selectInput("data",
                              "Data set",
                              list("anscombe 1",
                                   "anscombe 2",
                                   "anscombe 3",
                                   "anscombe 4",
                                   "cars",
                                   "cats",
                                   "faithful",
                                   "thuesen")),
                 br(), br(),
                 ## tab 1 inputs
                 conditionalPanel(
                    condition = "input.tab == 'tab1'",
                     ## use uiOutput to get numericInputs dependent on data
                     uiOutput("intercept"),
                     uiOutput("slope"),
                     br(),
                     checkboxInput("fitted", "Show fitted line", FALSE),
                     br(),
                     checkboxInput("residuals", "Show residuals", FALSE),
                     br(),
                     checkboxInput("squares", "Show squared residuals", FALSE),
                     br(),
                     radioButtons("loss",
                                  "Loss function",
                                  list("absolute", "quadratic")),
                     br()),
                 ## tab 2 inputs
                 conditionalPanel(
                     condition = "input.tab == 'tab2'",
                     checkboxInput("density2D", "Show fitted density in 2D",
                                   FALSE),
                     selectInput("limits",
                                 "Limits of density interval",
                                 list("99%", "95%", "90%")),
                     br(),
                     checkboxInput("density3D", "Show fitted density in 3D",
                                   FALSE),
                     numericInput("quantiles",
                                  "Number of density sections",
                                  value = 0, min = 0, max = 8))
                 ),
    mainPanel(
        tabsetPanel(
            tabPanel("Fitting Process", plotOutput("fitPlot"),
                     value = "tab1"),
            tabPanel("Fitted Distribution",
                     div(align = "center",
                         plotOutput("distPlot", width = "50%")),
                     value = "tab2"),
            id = "tab")
        )
    ))
