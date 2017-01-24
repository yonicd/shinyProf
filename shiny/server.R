shinyServer(function(input, output, session) {
  
  output$timeSlideUI=renderUI({
    ctxIdx=1
    if(!is.null(input$ctx)) ctxIdx=as.numeric(input$ctx)
    df=GS.net[GS.net$ctx==ctxIdx,]
    sliderInput(inputId = 'timeSlide',label = 'Time',
                                         min=min(df$timeid),
                                         max = max(df$timeid),
                                         value = c(min(df$timeid),min(df$timeid)+1),
                                         step = 1,
                                         dragRange = T,
                                         animate=TRUE
                                         #,timeFormat = '%T:%L'
                                         )
    })
  

  GS.mat=reactive({
    ctxIdx=1
    if(!is.null(input$ctx)) ctxIdx=input$ctx
    tSlide=range(GS.net$timeid)
    if(!is.null(input$timeSlide)) tSlide=input$timeSlide
    df=GS.net[GS.net$timeid>=tSlide[1]&GS.net$timeid<=tSlide[2],]
    df$timef<-NULL

    colrs <- brewer.pal(max(length(unique(df$action)),3), "Set3")
    
    nodes=bind_rows(
      df%>%filter(ctx==ctxIdx)%>%filter(!action%in%c('dep','depId'))%>%select(rowid,value=label)%>%left_join(nodesBase,by='value')%>%select(-rowid)%>%distinct(),
      df%>%filter(ctx==ctxIdx)%>%filter(action%in%c('dep','depId'))%>%select(rowid,label,dependsOn)%>%melt(.,'rowid')%>%left_join(nodesBase,by='value')%>%select(-c(variable,rowid))%>%distinct()
    )%>%distinct()%>%select(id,value)
    nodes$onAction=1
    nodes$onAction[nrow(nodes)]=2
    
    #nodes=nodes%>%left_join(df%>%select(value=label,action,valueNode=value),by='value')%>%distinct()
    
    links=df%>%filter(ctx==ctxIdx)%>%select(rowid,label,dependsOn)%>%melt(.,'rowid')%>%
      left_join(nodesBase,by='value')%>%dcast(rowid~variable,value.var='id')%>%rename(to=label,from=dependsOn)%>%
      do(.,filter(.,complete.cases(.)))%>%mutate(weight=1)%>%select(-rowid)%>%select(from,to,weight)
    
    net=graph.data.frame(links,nodes,directed = T)

    V(net)$color <- colrs[V(net)$onAction]
    
    net
    
  })
 
  output$selCtx<-renderUI({
    selectInput(inputId = 'ctx',
                label = 'Select event num',
                choices = unique(GS.net$ctx),
                selected = unique(GS.net$ctx)[1])
  })
  
  output$action<-renderText({
    ctxIdx=1
    if(!is.null(input$ctx)) ctxIdx=input$ctx
    tSlide=range(GS.net$timeid)
    if(!is.null(input$timeSlide)) tSlide=input$timeSlide
    df=GS.net[GS.net$timeid>=tSlide[1]&GS.net$timeid<=tSlide[2],]
    df$timef<-NULL
    paste0(df$action,collapse=',')
  })
  
    output$net<-renderPlot({
      net1=GS.mat()
      plot(net1,vertex.label=V(net1)$value)
    })

})