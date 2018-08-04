#Version 1 of Box Check list
#Created by Hannah Vincelette & Chuck Frost
#Creaion date: 8/1/2018


WhichBox=function(data.path, today=0){
  
  
  
  data=read.csv(data.path, header=T, stringsAsFactors = F)
  data<-data[ ,1:14]
  data<-data[data$sp1 != "UNAV",]
  
  if(today==0){
    
    warning("Today not provided. Using last recorded date.")
    
    today=max(as.numeric(data$date), na.rm=T)}

  box.list=unique(data$box)
  box.list=box.list[!is.na(box.list)]
  sp.list=c()
  since=c()
  to.pip=c()
  to.hatch=c()
  data$pip=(26-data$stage)+data$date
  data$hatch=(28-data$stage)+data$date



  for(b in 1:length(data$box)){
    if(data$sp1[b]=="COME"){data$pip[b]=(30-data$stage[b])+data$date[b]}
    if(data$sp1[b]=="COME"){data$hatch[b]=(32-data$stage[b])+data$date[b]}
  }

  
  for(a in 1:length(box.list)){
    
   
    since[a]=today-max(as.numeric(data$date[data$box==box.list[a]]), na.rm=T)
    last.obs=data[data$box==box.list[a],]
    last.obs=last.obs[which.max(last.obs$date),]
    to.pip[a]=last.obs$pip-today
    to.hatch[a]=last.obs$hatch-today
    sp.list[a]=last.obs$sp1
    
  
    
    if (any(data$done[data$box==box.list[a]])){to.hatch[a]=NA}
    if (any(data$done[data$box==box.list[a]])){to.pip[a]=NA}
   
  

    
    
  }
  
  
  
  
  
  check<-data.frame(box=box.list, sp1=sp.list, since=since, to.pip=to.pip, to.hatch=to.hatch)  
  
  
  check[order(check$since,decreasing=T), ]
  
  
  return(check[order(c(check$to.hatch),decreasing=F), ]) 
}




data=WhichBox("D:/incomplete_for_test.csv", today=168)
data=WhichBox("/Volumes/USB20FD/R7MBM_COGO/incomplete_for_test.csv", today=168)

data



inc=read.csv("incomplete_for_test.csv", header=T, stringsAsFactors = F)

