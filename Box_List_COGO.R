#Version 1 of Box Check list
#Created by Hannah Vincelette & Chuck Frost
#Creaion date: 8/1/2018


WhichBox=function(data.path, today=0){
  

  
  data=read.csv(data.path, header=T, stringsAsFactors = F)
data<-data[ ,1:13]

if(today==0){
  
  warning("Today not provided. Using last recorded date.")
  
  today=max(as.numeric(data$date), na.rm=T)}

  box.list=unique(data$box)
  box.list=box.list[!is.na(box.list)]
  since=c()
  to.pip=c()
  to.hatch=c()
  data$pip=(26-data$stage)+data$date
  data$hatch=(28-data$stage)+data$date
  

for(a in 1:length(box.list)){
  
  since[a]=today-max(as.numeric(data$date[data$box==box.list[a]]), na.rm=T)
  
  last.obs=data[data$box==box.list[a],]
  last.obs=last.obs[which.max(last.obs$date),]
  
  to.pip[a]=last.obs$pip-today
  to.hatch[a]=last.obs$hatch-today
                          
}  
  
  
  
check<-data.frame(box=box.list, since=since, to.pip=to.pip, to.hatch=to.hatch)  
  
  
check[order(check$since,decreasing=T), ]
  
  
 return(check[order(c(check$to.hatch),decreasing=F), ]) 
}


data=WhichBox("D:/incomplete_for_test.csv", today=149)
data


#considering COME incubation periods
WhichBox=function(data.path, today=0){
  
  
  
  data=read.csv(data.path, header=T, stringsAsFactors = F)
  data<-data[ ,1:13]
  
  if(today==0){
    
    warning("Today not provided. Using last recorded date.")
    
    today=max(as.numeric(data$date), na.rm=T)}
  
  box.list=unique(data$box)
  box.list=box.list[!is.na(box.list)]
  since=c()
  to.pip=c()
  to.hatch=c()
  data$pip=(28-data$stage)+data$date
  if [data$sp1=="COME"]{data$pip=(30-data$stage)+data$date}
  data$hatch=(32-data$stage)+data$date
  if [data$sp1=="COME"]{data$hatch=(32-data$stage)+data$date}
  
  
  for(a in 1:length(box.list)){
    
    since[a]=today-max(as.numeric(data$date[data$box==box.list[a]]), na.rm=T)
    
    last.obs=data[data$box==box.list[a],]
    last.obs=last.obs[which.max(last.obs$date),]
    
    to.pip[a]=last.obs$pip-today
    to.hatch[a]=last.obs$hatch-today
    
  }  
  
  
  
  check<-data.frame(box=box.list, since=since, to.pip=to.pip, to.hatch=to.hatch)  
  
  
  check[order(check$since,decreasing=T), ]
  
  
  return(check[order(c(check$to.hatch),decreasing=F), ]) 
}




data=WhichBox("D:/incomplete_for_test.csv", today=149)
data=WhichBox("/Volumes/USB20FD/R7MBM_COGO/incomplete_for_test.csv", today=149)

data
