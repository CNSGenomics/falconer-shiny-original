library(shiny)

shinyUI(fluidPage(
  titlePanel("Falconer additive-dominance model"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("sliderP", "p",
                  min = 0.0, max = 1.0, value = 0.3),
      numericInput("numericP", min = 0.0, max = 1.0, value = 0.3, ""),
      sliderInput("a", "a",
                  min = 0.0, max = 10.0, value = 4.0),
      sliderInput("d", "d",
                  min = 0.0, max = 10.0, value = 2.0)
    ),

    mainPanel(
      plotOutput("plot")
    )
  )
))
