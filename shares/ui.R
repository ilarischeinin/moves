library(shiny)

shinyUI(
  fillPage(title="Transportation Shares",
    plotOutput("plot", height="80%"),
    wellPanel(fluidRow(
      column(3, radioButtons("variable", label=NULL,
        choices=list("time", "distance"), selected="time")),
      column(3, radioButtons("share", label=NULL,
        choices=list(absolute="stack", relative="fill"), selected="stack")),
      column(3, checkboxInput("annotate", label="annotate", value=FALSE)),
      column(3, p("This is a visualization of the shares of different forms of",
        "transportation (excluding flying) based on my personal",
        a(href="https://www.moves-app.com", "Moves app"),
        "data. Source code is available on",
        a(href="https://github.com/ilarischeinin/moves", "GitHub.")))
    ))
  )
)

# EOF
