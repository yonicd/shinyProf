library(igraph)
library(zoo)
library(reshape2)
library(dplyr)
library(RColorBrewer)

load('GS.rda')

source('convertStack.R')
#to load from real run: GS.net=convertStack(shiny:::.graphStack$as_list())

nodesBase=GS.net%>%
  select(label,dependsOn)%>%
  distinct()%>%
  do(.,cbind(id=1:nrow(.),.))%>%
  melt(.,'id')%>%
  select(value)%>%
  distinct%>%
  filter(!is.na(value))%>%
  do(.,cbind(id=paste0('s',1:nrow(.)),.))
