#library(shinyAce)
a<-vector('list',6)
shinyApp(
  ui = bootstrapPage(
    # a div named mydiv
    # tags$div(id="mydiv", style="width: 50px; height :50px;
    #        left: 100px; top: 100px;
    #        background-color: gray; position: absolute"),
    actionButton('mydiv','button'),
    sliderInput('sl','slider',min = 0,max = 100,value = 50),
    conditionalPanel('input.sl<50',
                     uiOutput("results")),
    tags$script('
document.getElementById("mydiv").onclick = function() {
      var number = Math.random();
      Shiny.onInputChange("mydata", number);
      var markup=document.documentElement.outerHTML;
      Shiny.onInputChange("outerHTML",markup);
};
  ')
    
  ),
  server = function(input, output,session) {
    output$results<-renderUI({
      aceEditor(outputId = "toace",
                value=input$mydata,
                mode = "r", 
                theme = "chrome", 
                height = "100px",
                fontSize = 12)      
    })

    
    observeEvent(input$mydiv,{
      a[[input$mydiv]]<<-input$outerHTML
    })
  }
)