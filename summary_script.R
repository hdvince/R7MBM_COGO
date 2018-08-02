
#Version 1 of goldeneye script
#Created by Hannah Vincelette and Chuck Frost
#Creation date: 7/27/2018



#read in the most current data file

visits<-read.csv("R_2018_COGO_Box_Checks.csv", header=T, stringsAsFactors = F)
visits<-visits[ ,1:13]

boxes<-read.csv("R_2018_COGO_Box_Inventory.csv", header=T, stringsAsFactors = F)
boxes<-boxes[ ,1:11]

eggs<-read.csv("R_2018_COGO_Egg_Fate.csv", header=T, stringsAsFactors = F)
eggs<-eggs[,1:8]

#omit one sp
  #[boxes$sp1 != "UNOC"]

#omit two sp
  #occupancy.table<-table(boxes$sp1)
  #occupancy.birds<-occupancy.table[!(names(occupancy.table) %in% c("UNOC", "RDSQ"))]
  ......
  #missed<-table(boxes$sp1[boxes$sp1 != "UNOC" & boxes$sp1 != "RDSQ" & boxes$outcome=="missed"])
  #missed<-table(boxes$sp1[!(boxes$sp1 %in% c("UNOC", "RDSQ")) & boxes$outcome=="missed"])

#select out two sp
#names(occupancy.freq) %in% c("UNOC", "RDSQ")

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#OCCUPANCY
  #occupancy count
all.occupancy<-length(boxes$box[boxes$sp1 != "UNOC"])

all.occupancy<-length(boxes[boxes$sp1 != "UNOC", 1])
all.occupancy

  #occupancy by sp
occupancy<-table(boxes$sp1)
occupancy

  #occupancy by sp w/o UNOC
occupancy.sp<-table(boxes$sp1[boxes$sp1 != "UNOC"])
occupancy.sp

  #occupancy by sp w/o UNOC or RDSQ
occupancy.birds<-occupancy[!(names(occupancy) %in% c("UNOC", "RDSQ"))]
occupancy.birds

  #occupancy count w/o RDSQ
occupancy.total <- length(boxes$box[boxes$sp1 != "UNOC" & boxes$sp1 != "RDSQ"])
occupancy.total<-sum(occupancy[!(names(occupancy) %in% c("UNOC", "RDSQ"))])
occupancy.total

  #occupancy count COGO
occupancy.COGO<-length(boxes$box[boxes$sp1=="COGO"])
occupancy.COGO

  #occupancy plot by sp
occupancy.plot<-plot(table(boxes$sp1), xlab="species", ylab="# boxes", ylim=c(0,100))
occupancy.plot

  #occupancy plot w/o UNOC
occupancy.plot2<-plot(table(boxes$sp1[(boxes$sp1 != "UNOC")]), xlab="species", ylab="# boxes", ylim=c(0,100))
occupancy.plot2


  #occupancy plot w/o UNOC or RDSQ
occupancy.plot3<-plot(table(boxes$sp1[!(boxes$sp1 %in% c("UNOC", "RDSQ"))]), xlab="species", ylab="# boxes", ylim=c(0,100))
occupancy.plot3

  #occupancy frequencies by sp (prop 150 boxes)
occupancy.freq.150<-prop.table(occupancy)
occupancy.freq.150


  #occupancy frequency BOOW (prop 150 boxes)
occupancy.freq.150.BOOW <- prop.table(occupancy)[1]
occupancy.freq.150.BOOW

  #occupancy frequencies w/o UNOC
occupancy.freq.sp.150<-prop.table(occupancy)[(names(occupancy) != "UNOC")]
occupancy.freq.sp.150

  #occupancy frequencies w/o UNOC or RDSQ (prop 150 boxes)
occupancy.freq.birds.150<-prop.table(occupancy)[!(names(occupancy) %in% c("UNOC", "RDSQ"))]
occupancy.freq.birds.150


  #total occupancy frequency w/o UNOC or RDSQ
total.occupancy.freq<-sum(prop.table(occupancy)[1:4])
total.occupancy.freq<-occupancy.total/length(boxes$box)
total.occupancy.freq


  #unoccupied box frequency; total RDSQ and UNOC occupancy frequencies
unoccupied<-sum(prop.table(occupancy)[c("UNOC", "RDSQ")])
unoccupied

  #occupancy freq by sp w/ RDSQ (prop occupied boxes)
occupancy.freq.sp.100<-prop.table(occupancy.sp)
occupancy.freq.sp.100

  #occupancy frequencies by sp w/o RDSQ (prop occupied boxes)
occupancy.freq.birds.97<-prop.table(occupancy.birds)
occupancy.freq.birds.97



#HEN CAPTURE

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



 #sum new hens
new.hen<-sum(boxes$new, na.rm=T)
new.hen

  #new hens by sp w/o RDSQ or UNOC
new.hen.sp<-table(boxes$sp1[!(boxes$sp1 %in% c("UNOC", "RDSQ")) & boxes$new==1])
new.hen.sp

  #recap hens by sp w/o RDSQ or UNOC
recap.hen.sp<-table(boxes$sp1[!(boxes$sp1 %in% c("UNOC", "RDSQ")) & boxes$new==0])
recap.hen.sp

  #new hens frequencies by sp w/o RDSQ or UNOC (i.e. % of COGO hens that are new)
new.hen.freq<-new.hen.sp/occupancy.birds[names(occupancy.birds) %in% names(new.hen.sp)]
new.hen.freq

  #recap hens frequencies by sp w/o RDSQ or UNOC
recap.freq<-recap.hen.sp/occupancy.birds[names(occupancy.birds) %in% names(recap.hen.sp)]
recap.freq


  #uncaptured hens by sp 
uncap<-table(boxes$sp1[!(boxes$sp1 %in% c("UNOC", "RDSQ")) & boxes$band==""])
uncap

  #uncaptured hens freq by sp (prop of each sp occupancy)
uncap.freq<-uncap/occupancy.birds[names(occupancy.birds) %in% names(uncap)]
uncap.freq

#total uncaptured hens
total.uncap<-sum(uncap)
total.uncap

#uncaptured hen sp out of all occupied boxes
uncap.freq2 <- uncap/occupancy.total
uncap.freq2

  #uncaptured hens one sp
uncap.COGO<-uncap["COGO"]
uncap.COGO

#OUTCOME
  #outcome counts
outcome<-table(boxes$outcome)
outcome

  #outcome counts by sp
outcome.sp<-table(boxes$outcome, boxes$sp1)
outcome.sp

  #outcome frequencies
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

  

  #marked count
marked.total<-length(boxes$outcome[boxes$outcome=="marked"])
marked.total

  #marked COGO count
marked.COGO<-length(boxes$outcome[boxes$outcome=="marked" & boxes$sp1=="COGO"])
marked.COGO

  #marked counts by sp
marked<-table(boxes$sp1[boxes$outcome=="marked"])
marked


  #missed count
missed.total<- length(boxes$outcome[boxes$outcome=="missed"])
missed.total

 #missed counts by sp
missed<-table(boxes$sp1[boxes$outcome=="missed"])
missed


  #dump count 
dump.total<- length(boxes$outcome[boxes$outcome=="dump"])
dump.total

  #dump counts by sp
dump<-table(boxes$sp1[boxes$outcome=="dump"])
dump


  #n-abandon count 
n.abandon.total<- length(boxes$outcome[boxes$outcome=="n-abandon"])
n.abandon.total

  #n-abandon counts by sp
n.abandon<-table(boxes$sp1[boxes$outcome=="n-abandon"])
n.abandon

  #r-abandon count 
r.abandon.total<- length(boxes$outcome[boxes$outcome=="r-abandon"])
r.abandon.total

  #r-abandon counts by sp
r.abandon<-table(boxes$sp1[boxes$outcome=="r-abandon"])
r.abandon

  #u-abandon count 
u.abandon.total<- length(boxes$outcome[boxes$outcome=="u-abandon"])
u.abandon.total

  #u-abandon counts by sp
u.abandon<-table(boxes$sp1[boxes$outcome=="u-abandon"])
u.abandon


  #total abandon by sp
outcome.sp<-table(boxes$outcome[!(boxes$sp1 %in% c("UNOC", "RDSQ"))], boxes$sp1[!(boxes$sp1 %in% c("UNOC", "RDSQ"))])
x.outcome.sp<-outcome.sp[c("r-abandon", "n-abandon", "u-abandon"),]
colSums(x.outcome.sp)

  #unknown count 
unknown.total<- length(boxes$outcome[boxes$outcome=="unknown"])
unknown.total

  #unknown counts by sp
unknown<-table(boxes$sp1[boxes$outcome=="unknown"])
unknown



#EGG-LAYING & INCUBATION


  #nest initiation date stats by sp

a.initiation<-data.frame(
  sp=visits$sp1[visits$fate==1 & visits$hatched==1 & visits$sp1!="BOOW"], 
  date=visits$date[visits$fate==1 & visits$hatched==1 & visits$sp1!="BOOW"],
  box=visits$box[visits$fate==1 & visits$hatched==1 & visits$sp1!="BOOW"],
  eggs=0,
  adj.eggs=0)
  a.initiation
 
  for (b in 1:length(a.initiation$box)) {
    egg.list=visits$sp1eggs[visits$box == a.initiation$box[b]]
    a.initiation$eggs[b]=egg.list[which.max(egg.list)]
    a.initiation$adj.eggs[b]=a.initiation$eggs[b] 
    if(a.initiation$adj.eggs[b]>8){a.initiation$adj.eggs[b]=8}
    }
  
  a.initiation$adj.date=a.initiation$date-28
a.initiation$init.date=a.initiation$adj.date-(a.initiation$adj.eggs*2)
  head(a.initiation)
  
a.initiation

init<-data.frame(sp=unique(a.initiation$sp), early=0, late=0, range=0, mean=0)
init

for (c in 1:length(init$sp)){ 
  
  date.list=a.initiation$init.date[a.initiation$sp==init$sp[c]]
  init$early[c]=date.list[which.min(date.list)]
  init$late[c]=date.list[which.max(date.list)]
  init$range[c]=init$late[c]-init$early[c]
  init$mean[c]=mean(date.list)

}

init

#nest initiation date stats by sp

a.initiationC<-data.frame(
  sp=visits$sp1[visits$fate==1 & visits$hatched==1 & visits$sp1=="COME"], 
  date=visits$date[visits$fate==1 & visits$hatched==1 & visits$sp1=="COME"],
  box=visits$box[visits$fate==1 & visits$hatched==1 & visits$sp1=="COME"],
  eggs=0,
  adj.eggs=0)
a.initiationC

for (b in 1:length(a.initiationC$box)) {
  egg.list=visits$sp1eggs[visits$box == a.initiationC$box[b]]
  a.initiationC$eggs[b]=egg.list[which.max(egg.list)]
  a.initiationC$adj.eggs[b]=a.initiationC$eggs[b] 
  if(a.initiationC$adj.eggs[b]>8){a.initiationC$adj.eggs[b]=8}
}

a.initiationC$adj.date=a.initiationC$date-32
a.initiationC$init.date=a.initiationC$adj.date-(a.initiationC$adj.eggs*2)
head(a.initiationC)

a.initiationC

init.COME<-data.frame(sp=unique(a.initiationC$sp), early=0, late=0, range=0, mean=0)
init.COME

for (c in 1:length(init$sp)){ 
  
  date.list=a.initiationC$init.date[a.initiationC$sp==init.COME$sp[c]]
  init.COME$early[c]=date.list[which.min(date.list)]
  init.COME$late[c]=date.list[which.max(date.list)]
  init.COME$range[c]=init.COME$late[c]-init.COME$early[c]
  init.COME$mean[c]=mean(date.list)
  
}

init.COME

  #incubation stats by COGO & BUFF 
a.initiation2<-data.frame(
  sp=visits$sp1[visits$fate==1 & visits$hatched==1 & visits$sp1!="BOOW"& visits$sp1!="COME"], 
  date=visits$date[visits$fate==1 & visits$hatched==1 & visits$sp1!="BOOW"& visits$sp1!="COME"],
  box=visits$box[visits$fate==1 & visits$hatched==1 & visits$sp1!="BOOW"& visits$sp1!="COME"])
a.initiation2

a.initiation2$adj.date=a.initiation2$date-28
head(a.initiation2)

a.initiation2

incub<-data.frame(sp=unique(a.initiation2$sp), early=0, late=0, range=0, mean=0)
incub
                  

for (g in 1:length(incub$sp)){ 
  date.list=a.initiation2$adj.date[a.initiation2$sp==incub$sp[g]]
  incub$early[g]=date.list[which.min(date.list)]
  incub$late[g]=date.list[which.max(date.list)]
  incub$range[g]=incub$late[g]-incub$early[g]
  incub$mean[g]=mean(date.list)
  
}

incub

#incubation stats by COME
a.initiation3<-data.frame(
  sp=visits$sp1[visits$fate==1 & visits$hatched==1 & visits$sp1=="COME"], 
  date=visits$date[visits$fate==1 & visits$hatched==1 & visits$sp1=="COME"],
  box=visits$box[visits$fate==1 & visits$hatched==1 & visits$sp1=="COME"])
a.initiation3

a.initiation3$adj.date=a.initiation3$date-32
head(a.initiation3)

a.initiation3

incub.COME<-data.frame(sp=unique(a.initiation3$sp), early=0, late=0, range=0, mean=0)
incub.COME


for (g in 1:length(incub.COME$sp)){ 
  date.list=a.initiation3$adj.date[a.initiation3$sp==incub.COME$sp[g]]
  incub.COME$early[g]=date.list[which.min(date.list)]
  incub.COME$late[g]=date.list[which.max(date.list)]
  incub.COME$range[g]=incub.COME$late[g]-incub.COME$early[g]
  incub.COME$mean[g]=mean(date.list)
  
}

incub.COME


  #hatch date stats by sp
hatch<-data.frame(sp=unique(a.initiation$sp), early=0, late=0, range=0, mean=0 )
hatch

for (c in 1:length(hatch$sp)){ 
  
  date.list=a.initiation$date[a.initiation$sp==hatch$sp[c]]
  hatch$early[c]=date.list[which.min(date.list)]
  hatch$late[c]=date.list[which.max(date.list)]
  hatch$range[c]=hatch$late[c]-hatch$early[c]
  hatch$mean[c]=mean(date.list)
}
hatch

  #clutch by sp
clutch<-data.frame(sp=unique(a.initiation$sp), small=0, large=0, range=0, mean=0)


for (k in 1:length(clutch$sp)){
  clutch$small[k]=min(a.initiation$eggs[a.initiation$sp==clutch$sp[k]]) 
  clutch$large[k]=max(a.initiation$eggs[a.initiation$sp==clutch$sp[k]]) 
  clutch$range[k]=clutch$large[k]-clutch$small[k]
  clutch$mean[k]=mean(a.initiation$eggs[a.initiation$sp==clutch$sp[k]])
}

clutch


#count nests with other sp dump eggs (by sp)
dump.eggs<-table(boxes$sp2, boxes$sp1)
rownames(dump.eggs)[1]<-"no o/sp dump"

dump.eggs

#NEST SUCCESS

  #hatch success by sp
hatches<-table(boxes$sp1[boxes$outcome==c("marked", "missed")])
hatches

  #hatch success freq by sp (i.e. what % of COGO boxes hatched?)
hatches.freq<-hatches/occupancy.birds
hatches.freq

  #sum hatched eggs
hatched<-sum(eggs$hatched, na.rm=T)
hatched

  #sum hatched eggs by sp (1=hatched)
hatched.sp<-table(eggs$sp[eggs$hatched==1])
hatched.sp


#freq hatched eggs by sp
all.eggs<-table(eggs$sp[!(eggs$sp %in% c("UNOC", "RDSQ"))])
all.eggs
hatched.freq<- hatched.sp/all.eggs
hatched.freq

#HATCHLINGS

  #sum web-tagged ducklings
webtagged<-length(eggs$webtag[eggs$webtag != ""])
webtagged

  #sum banded owls
banded.owlets<-length(eggs$band[eggs$band !=""])
banded.owlets

  #sum marked hatchlings by sp (1=marked)
marked.sp<-table(eggs$sp[eggs$marked==1])
marked.sp

  #frequency of marked ducklings
marked.freq<-marked.sp/hatched.sp
marked.freq

