shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        uiOutput('sessionId'),
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
