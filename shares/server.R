library(ggplot2)
library(RColorBrewer)

shares <- readRDS("shares.rds")

cols <- c(walking=brewer.pal(9, 'Paired')[4],
  cycling=brewer.pal(9, 'Paired')[3],
  tram=brewer.pal(9, 'Oranges')[3],
  underground=brewer.pal(9, 'Oranges')[4],
  train=brewer.pal(9, 'Oranges')[5],
  bus=brewer.pal(9, 'Paired')[5],
  car=brewer.pal(9, 'Paired')[6],
  ferry=brewer.pal(9, 'Paired')[1],
  boat=brewer.pal(9, 'Paired')[2])

places <- c("Amsterdam", "Amsterdam", "Finland", "Helsinki", "New York")
jobs <- c("commute", "work from home", "vacation", "work from home", "commute")
events <- unique(shares$month)[c(3, 8, 11, 16)] + 15
legends <- events[-length(events)] + diff(events)/2
legends <- c(min(shares$month), legends, max(shares$month))

shinyServer(function(input, output) {
  output$plot <- renderPlot({
    p <- ggplot(shares,
      aes(x=month, fill=activity, order=-as.numeric(activity))) +
      aes_string(y=input$variable) +
      geom_area(position=input$share) + scale_fill_manual(values=cols) +
      theme(legend.title=element_blank())
    if (input$share == "stack") {
      if (input$variable == "time") {
        p <- p + ylab("time (h)")
      } else if (input$variable == "distance") {
        p <- p + ylab("distance (km)")
      }
    } else if (input$share == "fill") {
      p <- p + scale_y_continuous(label=function(x) {paste(x*100, "%")})
    }
    if (input$annotate) {
      p <- p +
        geom_vline(xintercept=as.numeric(events), lty="dashed") +
        annotate("text", x=legends,
          y=ggplot_build(p)$panel$ranges[[1]]$y.range[2] * 0.95, label=places,
          vjust=-0.5, hjust=c(-0.1, rep(0.5, length(legends)-2), 1.1)) +
        annotate("text", x=legends, y=0, label=jobs,
        vjust=1.5, hjust=c(-0.1, rep(0.5, length(legends)-2), 1.1))
    }
    p
  })
})

# EOF
