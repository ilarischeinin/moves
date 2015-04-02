library(shiny)

shinyUI(fluidPage(
  titlePanel("Move Shares"),
  verticalLayout(
    plotOutput("plot", height="500px"),
    wellPanel(fluidRow(
      column(4, radioButtons("variable", label=NULL,
        choices=list("time", "distance"), selected="time")),
      column(4, radioButtons("share", label=NULL,
        choices=list(absolute="stack", relative="fill"), selected="stack")),
      column(4, checkboxInput("annotate", label="annotate", value=FALSE))
    ))
  )
))

# EOF
