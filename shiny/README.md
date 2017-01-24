![](https://raw.githubusercontent.com/yonicd/shinyProf/master/shinyProfExample.gif)

  - Run the reactive log as you ussually would (directions [here](https://shiny.rstudio.com/reference/shiny/latest/showReactLog.html)) 

  - Instead of opening the output in the browser, now you can navigate the output inside a shiny app with controls over
  
    - choosing among different sessions
    - selecting which input and output obects you see
    - selecting which reactive events to view
    - seek specific actions in the reactive event chosen using a slider.
    - animate changes to the reactive network with slider animation.

```
options(shiny.reactlog=TRUE)
options(scipen=100)
options(digits.secs=3)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
    ),
    mainPanel(plotOutput("distPlot"))
  )
)

shinyApp(ui = ui, server = server)

options(shiny.reactlog=F)

GS.net=convertStack(shiny:::.graphStack$as_list())
```