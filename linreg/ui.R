## user interface for linear regression app
shinyUI(pageWithSidebar(
    ## title, possibly also tab name (in browser)
    headerPanel("Linear Regression"),
    ## use Chrome to see up/down arrows on numeric input boxes
    sidebarPanel(## use uiOutput to get numericInputs dependent on data
                 uiOutput("intercept"),
                 br(),
                 uiOutput("slope"),
                 br(),
                 checkboxInput("fitted", "Show fitted line", FALSE),
                 br(),
                 checkboxInput("residuals", "Show residuals", FALSE),
                 br(),
                 radioButtons("loss",
                              "Loss function",
                              list("absolute", "quadratic")),
                 br()),
    mainPanel(plotOutput("distPlot"))
    ))
