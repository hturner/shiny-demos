## user interface for linear regression app
shinyUI(fluidPage(
    ## title, possibly also tab name (in browser)
    titlePanel("Linear Regression"),

    sidebarLayout(
        ## sidebar
        ## use Chrome to see up/down arrows on numeric input boxes
        sidebarPanel(width = 3, # out of 12 units
                     selectInput("data",
                                 "Data set",
                                 list("anscombe 1",
                                      "anscombe 2",
                                      "anscombe 3",
                                      "anscombe 4",
                                      "cars",
                                      "cats",
                                      "faithful",
                                      "thuesen")),
                     ## tab 1 inputs
                     conditionalPanel(
                         condition = "input.tab == 'tab1'",
                         ## use uiOutput to get numericInputs dependent on data
                         conditionalPanel(
                             condition = "!output.intercept",
                              numericInput("a-holder",
                                           "Intercept (a)",
                                           value = NA,
                                           min = NA,
                                           max = NA,
                                           step = NA)
                             ),
                         uiOutput("intercept"),
                         conditionalPanel(
                             condition = "!output.slope",
                              numericInput("b-holder",
                                           "Slope (b)",
                                           value = NA,
                                           min = NA,
                                           max = NA,
                                           step = NA)
                             ),
                         uiOutput("slope"),
                         br(),
                         checkboxInput("fitted", "Show fitted line", FALSE),
                         checkboxInput("residuals", "Show residuals", FALSE),
                         checkboxInput("squares", "Show squared residuals",
                                       FALSE),
                         br(),
                         radioButtons("loss",
                                      "Loss function",
                                      list("absolute", "quadratic"))
                         ),
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
        ## main panel
        mainPanel(width = 9,
            tabsetPanel(
                tabPanel("Fitting Process", plotOutput("fitPlot"),
                         value = "tab1"),
                tabPanel("Fitted Distribution",
                         div(align = "center",
                             plotOutput("distPlot", width = "50%",
                                        height = "400px")),
                         value = "tab2"),
                id = "tab")
            )
        )
    )
)
