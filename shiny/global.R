library(ggnet)
library(zoo)
library(reshape2)
library(dplyr)
library(RColorBrewer)

# GS2net=function(df,low,high,ctxf){
#   df.mat=df[df$timef>=low&df$timef<=high,]
#   if(ctxf!='') df.mat=df.mat[df.mat$ctx>=ctxf,]
#   df.mat=df.mat%>%dcast(ctx+label~dependsOn,value.var='o')
#   browser()
#   row.names(df.mat)=df.mat[,2]
#   df.mat[,-c(1,2,8)]%>%as.matrix
# }

load('../GS.rda')
GS.net$timeid=1:nrow(GS.net)