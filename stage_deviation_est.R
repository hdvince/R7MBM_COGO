#Eric's deviation est.
#Created by Hannah Vincelette & Chuck Frost


accuracy=visits


for (b in 1:length(accuracy$stage)){
if(is.na(accuracy$stage[b])){accuracy$stage[b]=0}
}


accuracy<-accuracy[accuracy$stage>0,]
accuracy=accuracy[!is.na(accuracy$box),]

accuracy<-accuracy[accuracy$sp1=="COGO" | accuracy$sp1=="BUFF", ]

accuracy$hdate=NA
accuracy$pdate=NA


hatchers=unique(accuracy$box[accuracy$hatched==1])

accuracy=accuracy[accuracy$box %in% hatchers,]

for (a in 1:length(accuracy$stage)) {

  accuracy$hdate[a]=accuracy$date[accuracy$hatched==1 & accuracy$box==accuracy$box[a]]
  
  accuracy$pdate[a]=(28-accuracy$stage[a])+accuracy$date[a]
  
}

accuracy$diff=accuracy$pdate-accuracy$hdate

plot(accuracy$stage, accuracy$diff)
plot(accuracy$date, accuracy$diff)

