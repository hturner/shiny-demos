
shinyUI(pageWithSidebar(
  ## title, possibly also tab name (in browser)
headerPanel("Illustrating leave one out residuals"),
  ## use Chrome to see up/down arrows on numeric input boxes

sidebarPanel(
  selectInput("data",
                           "Data set",
                           list("Anscombe 1",
                                "Anscombe 2",
                                "Anscombe 3",
                                "Anscombe 4")),
               
sliderInput("i", "From", min=1, max=11, value=1, step = 1,  format = "####", ticks = TRUE, animate = TRUE)
  ),

  mainPanel(
    plotOutput("LOOplot")
    )
))
