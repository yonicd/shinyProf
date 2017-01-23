library(shinyAce)
library(shiny)

a<-vector('list',1)
shinyApp(
  ui = bootstrapPage(
    # a div named mydiv
    # tags$div(id="mydiv", style="width: 50px; height :50px;
    #        left: 100px; top: 100px;
    #        background-color: gray; position: absolute"),
    actionButton(inputId = 'mydiv',label = 'button'),
    sliderInput('sl','slider',min = 0,max = 100,value = 50),
    conditionalPanel('input.sl<50',
                     uiOutput("results")),
    tags$script('
document.getElementById("mydiv").onclick = function() {
      var number = Math.random();
      Shiny.onInputChange("mydata", number);
      var markup=document.body.outerHTML;
      Shiny.onInputChange("outerHTML",markup);
};
  ')
    
  ),
  server = function(input, output,session) {
    output$results<-renderUI({
      textInput('toace','val',input$mydata)
    })

    
    observeEvent(input$outerHTML,{
      a[[input$mydiv]]<<-input$outerHTML
    })
  }
)


system('node ./node_modules/himalaya/bin/himalaya test.html test.json')
b=jsonlite::read_json('test.json')
listviewer::jsonedit(b)