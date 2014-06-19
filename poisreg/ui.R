## user interface for linear regression app
shinyUI(pageWithSidebar(
    ## title, possibly also tab name (in browser)
    headerPanel("Poisson Regression"),
    ## use Chrome to see up/down arrows on numeric input boxes
    sidebarPanel(width = 3, # out of 12 units
                 selectInput("data",
                              "Data set",
                              list("cars")),
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
                     radioButtons("type",
                                  "Residual type",
                                  list("Pearson residuals" = "pearson",
                                       "Squared Pearson residuals" = "pearson2",
                                       "Deviance residuals" = "dev",
                                       "Squared deviance residuals" = "dev2")),
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
    mainPanel(width = 9,
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
