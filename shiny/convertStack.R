#Create GS.net object from graphStack ----
convertStack=function(obj){
  #read graphStack
  obj.df=data.frame(rowid=1:length(obj))
  items=c('session','label','id','action','value','prevId','dependsOn','type','time')
  for(item in items) obj.df[[item]]=lapply(obj,function(x){
    if(!is.null(x[[item]])){
      x[[item]]
    }else{
      NA
    }
  } )
  
  
  obj.net=obj.df[!grepl('client',obj.df$id),]
  #unlist objects
  for(i in which(sapply(obj.net,class)=='list')) obj.net[,i]=sapply(obj.net[[i]],'[[',1)
  #fix epoch times to R times
  obj.net$timef=as.POSIXlt(obj.net$time,origin='1970-01-01')
  #merge labels
  obj.labels=obj.net[!is.na(obj.net$label),c('label','id')]
  obj.net$label.new=obj.labels$label[match(obj.net$id,obj.labels$id)]
  obj.net$label=ifelse(is.na(obj.net$label.new),obj.net$id,obj.net$label.new)
  #merge depends
  obj.net$dependsOn.new=obj.labels$label[match(obj.net$dependsOn,obj.labels$id)]
  obj.net$dependsOn=ifelse(is.na(obj.net$dependsOn.new),obj.net$dependsOn,obj.net$dependsOn.new)
  #cleanup
  obj.net[,c('id','label.new','prevId','dependsOn.new')]<-NULL
  #build ctxIdx
  obj.net$ctx=NA
  obj.net$ctx[which(obj.net$action=='ctx')]=2:(length(which(obj.net$action=='ctx'))+1)
  obj.net$ctx[1]=1
  obj.net$ctx=na.locf(obj.net$ctx)
  obj.net$timeid=1:nrow(obj.net)
  
  obj.net
}