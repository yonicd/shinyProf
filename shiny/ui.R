shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        uiOutput('sessionId'),
        selectizeInput(inputId = 'inList',
                    label = 'input$objects',
                    choices = c(NULL,unique(grep('input\\$',GS.net$label,value = T))),
                    multiple = T),
        selectizeInput(inputId = 'outList',
                    label = 'output$objects',
                    choices = c(NULL,unique(grep('output\\$',GS.net$label,value = T))),
                    multiple = T),
        uiOutput('selCtx'),
        uiOutput('timeSlideUI'),
        p('action taken'),
        verbatimTextOutput('action')
      ),
      mainPanel(
        plotOutput('net')
      )
    )
  )
)
