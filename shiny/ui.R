shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
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
