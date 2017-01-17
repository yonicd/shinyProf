devtools::install_github("briatte/ggnet")
library(igraph)
library(d3r)
library(GGally)
library(network)
library(sna)
library(ggnet)
library(shiny)

library(zoo)
library(reshape2)
library(dplyr)


options(shiny.reactlog=TRUE)
options(scipen=100)
options(digits.secs=3)

# plot(igraph::watts.strogatz.game(1, 50, 4, 0.05))
# net = rgraph(10, mode = "graph", tprob = 0.5)
# net = network(net, directed = T)

# vertex names
# network.vertex.names(net) = letters[1:10]
# ggnet2(net,)
# ggnet2(net, node.size = 6, node.color = "black", edge.size = 1, edge.color = "grey")
# ggnet2(net, size = 6, color = "black", edge.size = 1, edge.color = "grey")
# ggnet2(net, size = 6, color = rep(c("tomato", "steelblue"), 5))
# ggnet2(net, mode = "circle")
# ggnet2(net, mode = "kamadakawai")
# ggnet2(net, mode = "fruchtermanreingold", layout.par = list(cell.jitter = 0.75))
# ggnet2(net, mode = "target", layout.par = list(niter = 100))
# net %v% "phono" = ifelse(letters[1:10] %in% c("a", "e", "i"), "vowel", "consonant")
# ggnet2(net, color = "phono")

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

GS=shiny:::.graphStack$as_list()
GS.df=data.frame(rowid=1:length(GS))
items=c('session','label','id','action','value','prevId','dependsOn','type','time')
for(item in items) GS.df[[item]]=lapply(GS,function(x){
  if(!is.null(x[[item]])){
    x[[item]]
  }else{
    NA
  }
} )


GS.net=GS.df[!grepl('client',GS.df$id),]
for(i in which(sapply(GS.net,class)=='list')) GS.net[,i]=sapply(GS.net[[i]],'[[',1)
GS.net$timef=as.POSIXlt(GS.net$time,origin='1970-01-01')
GS.labels=GS.net[!is.na(GS.net$label),c('label','id')]



GS.net$label.new=GS.labels$label[match(GS.net$id,GS.labels$id)]
GS.net$label=ifelse(is.na(GS.net$label.new),GS.net$id,GS.net$label.new)

GS.net$dependsOn.new=GS.labels$label[match(GS.net$dependsOn,GS.labels$id)]
GS.net$dependsOn=ifelse(is.na(GS.net$dependsOn.new),GS.net$dependsOn,GS.net$dependsOn.new)

GS.net[,c('id','label.new','prevId','dependsOn.new')]<-NULL

GS.net$ctx=NA
GS.net$ctx[which(GS.net$action=='ctx')]=2:(length(which(GS.net$action=='ctx'))+1)
GS.net$ctx[1]=1


GS.net$ctx=na.locf(GS.net$ctx)
GS.net$o=1


GS.mat=GS.net[GS.net$ctx==5,]%>%dcast(ctx+label~dependsOn,value.var='o')
row.names(GS.mat)=GS.mat[,2]
GS.mat=GS.mat[,-c(1,2,8)]%>%as.matrix

network(GS.mat, directed = TRUE)%>%
  ggnet2(.,label=T,arrow.size = 12, arrow.gap = 0.025)

serverGS <- function(input, output) {
  output$timeSlide=renderUI({sliderInput(inputId = 'timeSlide',label = 'Time',
                                         min=min(GS.net$timef),
                                         max = max(GS.net$timef),
                                         value = range(GS.net$timef),
                                         dragRange = T,ticks = F,timeFormat = '%T:%L')})
  
  output$net<-renderPlot({
    ggnet2(net)
  })
}

uiGS <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      uiOutput('timeSlide')
    ),
    mainPanel(
      plotOutput('net')
    )
  )
)

shinyApp(ui = uiGS, server = serverGS)
