#Version 2 of goldeneye script
#Created by Hannah Vincelette and Chuck Frost
#Creation date: 8/3/2018



#read in the most current data file

visits<-read.csv("R_2018_COGO_Box_Checks.csv", header=T, stringsAsFactors = F)
visits<-visits[ ,1:14]
visits<-visits[visits$sp1 != "UNAV",]

boxes<-read.csv("R_2018_COGO_Box_Inventory.csv", header=T, stringsAsFactors = F)
boxes<-boxes[ ,1:11]
boxes<-boxes[boxes$sp1 != "UNAV",]

eggs<-read.csv("R_2018_COGO_Egg_Fate.csv", header=T, stringsAsFactors = F)
eggs<-eggs[,1:8]
eggs<-eggs[eggs$sp != "UNAV",]




#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx



#OCCUPANCY

# occupancy by sp
occupancy<-table(boxes$sp1)
occupancy

# number incubating COGO, BUFF, & COME hens (boxes w/ stage>3)

a.incub<-data.frame(
  box=visits$box[visits$sp1 =="COGO"|visits$sp1 =="BUFF"|visits$sp1 =="COME"],
  sp=visits$sp1[visits$sp1 =="COGO"|visits$sp1 =="BUFF"|visits$sp1 =="COME"],
  stage=visits$stage[visits$sp1 =="COGO"|visits$sp1 =="BUFF"|visits$sp1 =="COME"]
)
a.incub


for (g in 1:length(a.incub$box)) {
  a.incub$stage[g]=max(a.incub$stage[a.incub$box==a.incub$box[g]])
  if (is.na(a.incub$stage[g])){a.incub$stage[g]=0}
     
}

a.incub<-subset(a.incub, !(a.incub$stage==NA & a.incub$stage<3))
a.incub
incub.hen<-unique(a.incub)
incub.hen

incub.hen<-table(incub.hen$sp)

incub.hen.freq<-incub.hen/occupancy[c("BUFF", "COGO", "COME")]
incub.hen.freq

#EGGS

# total number eggs laid by sp

total.eggs<-table(eggs$sp[!(eggs$sp %in% c("UNOC", "RDSQ"))])
total.eggs


#clutch by sp
clutch<-data.frame(sp=unique(eggs$sp[!(eggs$sp %in% c("UNOC", "RDSQ"))]), small=0, large=0, range=0, mean=0, stringsAsFactors = F)
clutch

clutch.tot<-data.frame(sp=unique(eggs$sp[!(eggs$sp %in% c("UNOC", "RDSQ"))]), small=0, large=0, range=0, mean=0, stringsAsFactors = F)
clutch.tot

a.clutch<-data.frame(
  sp=visits$sp1[boxes$outcome=="missed"|boxes$outcome=="marked"|boxes$outcome=="n-abandon"|boxes$outcome=="r-abandon"|boxes$outcome=="u-abandon"], 
  date=visits$date[boxes$outcome=="missed"|boxes$outcome=="marked"|boxes$outcome=="n-abandon"|boxes$outcome=="r-abandon"|boxes$outcome=="u-abandon"],
  box=visits$box[boxes$outcome=="missed"|boxes$outcome=="marked"|boxes$outcome=="n-abandon"|boxes$outcome=="r-abandon"|boxes$outcome=="u-abandon"],
  primaryeggs=0,
  secondaryeggs=0,
  totaleggs=0, stringsAsFactors = F)

a.clutch

for (b in 1:length(a.clutch$box)) {
  a.clutch$primaryeggs[b]=length(eggs$eggnum[a.clutch$sp[b]==eggs$sp & eggs$box==a.clutch$box[b]])
  a.clutch$secondaryeggs[b]=length(eggs$eggnum[a.clutch$box[b]==eggs$box])-a.clutch$primaryeggs[b]
  a.clutch$totaleggs[b]=a.clutch$primaryeggs[b] + a.clutch$secondaryeggs[b]
  
}
a.clutch

for (k in 1:length(clutch$sp)){
  clutch$small[k]=min(a.clutch$primaryeggs[a.clutch$sp==clutch$sp[k]]) 
  clutch$large[k]=max(a.clutch$primaryeggs[a.clutch$sp==clutch$sp[k]]) 
  clutch$range[k]=clutch$large[k]-clutch$small[k]
  clutch$mean[k]=mean(a.clutch$primaryeggs[a.clutch$sp==clutch$sp[k]])
}

clutch

for (k in 1:length(clutch.tot$sp)){
  clutch.tot$small[k]=min(a.clutch$totaleggs[a.clutch$sp==clutch$sp[k]]) 
  clutch.tot$large[k]=max(a.clutch$totaleggs[a.clutch$sp==clutch$sp[k]]) 
  clutch.tot$range[k]=clutch.tot$large[k]-clutch.tot$small[k]
  clutch.tot$mean[k]=mean(a.clutch$totaleggs[a.clutch$sp==clutch$sp[k]])
}

clutch.tot

clutch
clutch.tot



#NEST SUCCESS

# outcomes by sp
outcome.sp<-table(boxes$outcome, boxes$sp1)
outcome.sp

outcome.sp.df<-as.data.frame(outcome.sp)
outcome.sp.df
names(outcome.sp.df)<-c("outcome", "sp", "count")
outcome.sp.df
total.sp<-aggregate(outcome.sp.df$count~outcome.sp.df$sp, FUN=sum)
names(total.sp)<-c("sp", "count")
total.sp
outcome.sp.df<-merge(outcome.sp.df, total.sp, by="sp")
outcome.sp.df
names(outcome.sp.df)<-c("sp", "outcome", "count", "total")
outcome.sp.df$pct<-outcome.sp.df$count/outcome.sp.df$total
outcome.sp.df
outcome.freq<-reshape(outcome.sp.df[,c("sp", "outcome", "pct")], idvar="outcome", timevar="sp", direction="wide")
names(outcome.freq)<-c("outcome", as.character(unique(outcome.sp.df$sp)))
outcome.freq

#EGG SUCCESS

# hatchlings marked by sp (1==marked)
marked.sp<-table(eggs$sp[eggs$marked==1])
marked.sp



#HEN CAPTURE

# hen status by sp
hen.df<-data.frame(
  sp=boxes$sp1[!(boxes$sp1 %in% c("UNOC", "RDSQ"))], 
  box=boxes$box[!(boxes$sp1 %in% c("UNOC", "RDSQ"))], 
  recap=boxes$recap[!(boxes$sp1 %in% c("UNOC", "RDSQ"))], 
  new=boxes$new[!(boxes$sp1 %in% c("UNOC", "RDSQ"))], 
  uncap=0
)
hen.df

for(j in 1:length(hen.df$box)) {
  if (is.na(hen.df$recap[j])){hen.df$uncap[j]=1}
}
hen.df

hen.status<-aggregate(cbind(recap, new,uncap)~sp, data=hen.df, FUN=sum)
hen.status<-aggregate(cbind(recap,new,uncap)~sp, data=hen.df, FUN=sum, na.rm=T, na.action=na.pass)
hen.status

