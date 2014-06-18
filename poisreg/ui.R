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
                     br())),
    mainPanel(width = 9,
        tabsetPanel(
            tabPanel("Fitting Process", plotOutput("fitPlot"),
                     value = "tab1"),
            id = "tab")
        )
    ))
