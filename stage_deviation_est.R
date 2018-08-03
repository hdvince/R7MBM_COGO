#Eric's deviation est.
#Created by Hannah Vincelette & Chuck Frost


stage=visits


for (b in 1:length(stage$stage)){
if(is.na(stage$stage[b]) & stage$hatched[b]==1){stage$stage[b]=28}
}


stage<-stage[stage$stage>0,]
stage=stage[!is.na(stage$box),]

stage<-stage[stage$sp1=="COGO" | stage$sp1=="BUFF", ]

stage$hdate=NA
stage$pdate=NA


hatchers=unique(stage$box[stage$hatched==1])

stage=stage[stage$box %in% hatchers,]

for (a in 1:length(stage$stage)) {

  stage$hdate[a]=stage$date[stage$hatched==1 & stage$box==stage$box[a]]
  
  stage$pdate[a]=(28-stage$stage[a])+stage$date[a]
  
}

stage$diff=stage$pdate-stage$hdate


