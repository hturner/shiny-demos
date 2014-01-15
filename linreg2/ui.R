## user interface
shinyUI(pageWithSidebar(
    ## title, possibly also tab name (in browser)
    headerPanel("Linear Regression 2"),
    ## use Chrome to see up/down arrows on numeric input boxes
    ## just anscombe 1 for initial development
    sidebarPanel(selectInput("data",
                              "Data set",
                              list("anscombe 1")),
                 br(),
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
                              value = 0, min = 1, max = 8)
                 ),
    mainPanel(plotOutput("distPlot"))
    ))
