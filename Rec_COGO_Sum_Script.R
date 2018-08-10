#Rec Goldeneye Summary Script
#Created by Hannah Vincelette and Chuck Frost
#Creation date: 8/3/2018



#read in the most current data file

##to run stats on the rec

visits<-read.csv("2018_COGO_Box_Checks.csv", header=T, stringsAsFactors = F, na.strings = "")
visits<-visits[ ,1:14]
visits<-visits[visits$sp1 != "UNAV",]
visits<-visits[!is.na(visits$box), ]
visits<-visits[visits$site!="C",]

boxes<-read.csv("2018_COGO_Box_Inventory.csv", header=T, stringsAsFactors = F)
boxes<-boxes[ ,1:11]
boxes<-boxes[boxes$sp1 != "UNAV",]
boxes<-boxes[!is.na(boxes$box), ]
boxes<-boxes[boxes$site!="C",]

eggs<-read.csv("2018_COGO_Egg_Fate.csv", header=T, stringsAsFactors = F)
eggs<-eggs[,1:8]
eggs<-eggs[eggs$sp != "UNAV",]
eggs<-eggs[!is.na(eggs$box), ]
eggs<-eggs[eggs$site!="C",]

library(ggplot2)



# to run stats on the corps  

# visits<-visits[visits$site=="C",]


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx



#OCCUPANCY

# other occupancy by sp
o.occupancy =table(boxes$sp1[boxes$sp1 %in% c("UNOC", "RDSQ")])
#o.occupancy


# bird occupancy by sp (defined by laying an egg)
occupancy =table(boxes$sp1[!(boxes$sp1 %in% c("UNOC", "RDSQ"))])
#occupancy


# number incubating COGO, BUFF, & COME hens (boxes w/ stage>3)

a.incub<-data.frame(
  box=visits$box[visits$sp1 =="COGO"|visits$sp1 =="BUFF"|visits$sp1 =="COME" ],
  sp=visits$sp1[visits$sp1 =="COGO"|visits$sp1 =="BUFF"|visits$sp1 =="COME" ],
  stage=visits$stage[visits$sp1 =="COGO"|visits$sp1 =="BUFF"|visits$sp1 =="COME"], 
  stringsAsFactors = FALSE
)
#a.incub

a.incub1<-a.incub[!is.na(a.incub$stage),]

for (g in 1:length(a.incub1$box)) {
  a.incub1$stage[g]=max(a.incub1$stage[a.incub1$box==a.incub1$box[g]])
}

#a.incub1

a.incub2<-a.incub1[a.incub1$stage>3,]

incub.hen<-aggregate(a.incub2$sp~a.incub2$box, FUN=max)
#incub.hen

names(incub.hen)<- c("box", "sp")

incub.hen.tab<- table(incub.hen$sp)

incub.hen.freq<-incub.hen.tab/occupancy[names(occupancy) %in% names(incub.hen.tab)]
#incub.hen.freq
# number boxes with squirrel kittens
squirrels<- length(boxes$sp1[boxes$sp1=="RDSQ"])

# number of boxes unavailable during first sweep
# does not work unless data files are read in without subsetting out UNAV rows
# unavailable<- length(visits$visit[visits$visit==1 & visits$sp1=="UNAV"])
#unavailable


# sp combos occupying boxes (defined by laying an egg)
# omits boxes with 3 sp
# will not account for AMKE & OTHER species

a.combo<-data.frame(
  box=boxes$box[boxes$sp1 != "UNOC" & boxes$sp1 != "RDSQ"],
  sp1=boxes$sp1[boxes$sp1 != "UNOC" & boxes$sp1 != "RDSQ"],
  sp2=boxes$sp2[boxes$sp1 != "UNOC" & boxes$sp1 != "RDSQ"],
  sp3=boxes$sp3[boxes$sp1 != "UNOC" & boxes$sp1 != "RDSQ"]
)
#a.combo


combos<-data.frame(
  COGO=length(a.combo$sp1[a.combo$sp1=="COGO" & a.combo$sp2==""]),
  BUFF=length(a.combo$sp1[a.combo$sp1=="BUFF" & a.combo$sp2==""]),
  COME=length(a.combo$sp1[a.combo$sp1=="COME" & a.combo$sp2==""]),
  BOOW=length(a.combo$sp1[a.combo$sp1=="BOOW" & a.combo$sp2==""]),
  COGO.BUFF=length(a.combo$sp1[(a.combo$sp1=="COGO" | a.combo$sp1=="BUFF") & (a.combo$sp2=="COGO"| a.combo$sp2=="BUFF")]),
  COGO.COME=length(a.combo$sp1[(a.combo$sp1=="COGO"| a.combo$sp1=="COME") & (a.combo$sp2=="COGO"|a.combo$sp2=="COME")]),
  COGO.BOOW=length(a.combo$sp1[(a.combo$sp1=="COGO"|a.combo$sp1=="BOOW") & (a.combo$sp2=="COGO"|a.combo$sp2=="BOOW")]), 
  BUFF.BOOW=length(a.combo$sp1[(a.combo$sp1=="BUFF"|a.combo$sp1=="BOOW") & (a.combo$sp2=="BUFF"|a.combo$sp2=="BOOW")]), 
  COME.BOOW=length(a.combo$sp1[(a.combo$sp1=="COME"|a.combo$sp1=="BOOW") & (a.combo$sp2=="COME" | a.combo$sp2=="BOOW")]), 
  COME.BUFF=length(a.combo$sp1[(a.combo$sp1=="BUFF"|a.combo$sp1=="BOOW") & (a.combo$sp2=="COME"|a.combo$sp2=="BUFF")])
  
)
#combos
  



#EGGS

# total number eggs laid by sp

total.eggs<-table(eggs$sp[!(eggs$sp %in% c("UNOC", "RDSQ"))])
#total.eggs


#clutch by sp
clutch<-data.frame(sp=unique(eggs$sp[!(eggs$sp %in% c("UNOC", "RDSQ"))]), small=0, large=0, range=0, mean=0, stringsAsFactors = F)
#clutch

clutch.tot<-data.frame(sp=unique(eggs$sp[!(eggs$sp %in% c("UNOC", "RDSQ"))]), small=0, large=0, range=0, mean=0, stringsAsFactors = F)
#clutch.tot

a.clutch<-data.frame(
  sp=visits$sp1[boxes$outcome=="missed"|boxes$outcome=="marked"|boxes$outcome=="n-abandon"|boxes$outcome=="r-abandon"|boxes$outcome=="u-abandon"], 
  date=visits$date[boxes$outcome=="missed"|boxes$outcome=="marked"|boxes$outcome=="n-abandon"|boxes$outcome=="r-abandon"|boxes$outcome=="u-abandon"],
  box=visits$box[boxes$outcome=="missed"|boxes$outcome=="marked"|boxes$outcome=="n-abandon"|boxes$outcome=="r-abandon"|boxes$outcome=="u-abandon"],
  primaryeggs=0,
  secondaryeggs=0,
  totaleggs=0, stringsAsFactors = F)

#a.clutch

for (b in 1:length(a.clutch$box)) {
  a.clutch$primaryeggs[b]=length(eggs$eggnum[a.clutch$sp[b]==eggs$sp & eggs$box==a.clutch$box[b]])
  a.clutch$secondaryeggs[b]=length(eggs$eggnum[a.clutch$box[b]==eggs$box])-a.clutch$primaryeggs[b]
  a.clutch$totaleggs[b]=a.clutch$primaryeggs[b] + a.clutch$secondaryeggs[b]
  
}
#a.clutch

for (k in 1:length(clutch$sp)){
  clutch$small[k]=min(a.clutch$primaryeggs[a.clutch$sp==clutch$sp[k]]) 
  clutch$large[k]=max(a.clutch$primaryeggs[a.clutch$sp==clutch$sp[k]]) 
  clutch$range[k]=clutch$large[k]-clutch$small[k]
  clutch$mean[k]=mean(a.clutch$primaryeggs[a.clutch$sp==clutch$sp[k]])
}

#clutch

for (k in 1:length(clutch.tot$sp)){
  clutch.tot$small[k]=min(a.clutch$totaleggs[a.clutch$sp==clutch$sp[k]]) 
  clutch.tot$large[k]=max(a.clutch$totaleggs[a.clutch$sp==clutch$sp[k]]) 
  clutch.tot$range[k]=clutch.tot$large[k]-clutch.tot$small[k]
  clutch.tot$mean[k]=mean(a.clutch$totaleggs[a.clutch$sp==clutch$sp[k]])
}

#clutch.tot

#clutch
#clutch.tot

# number boxes with mixed sp eggs 
# doesn't take into account if sp1 hen switches
boxes.o.eggs<-length(boxes$sp2[boxes$sp2 !=""])
#boxes.o.eggs

# number of eggs predated (an egg went missing)
# does not count ducklings found dead (documented in excel sheet as hatched, unmarked egg)
# does not include owls
predation.eggs<-length(eggs$box[is.na(eggs$hatched) & !(eggs$sp %in% c("BOOW", "UNOC", "RDSQ"))])
#predation.eggs

# number of eggs predated over time
# not calculated; need dates of individual egg predation

# number of boxes predated by box initiation date
# not calculated; only have initiation dates for marked boxes (with known JD hatch) and most predated boxes are not marked

# number of boxes predated (an egg went missing)
# does not count ducklings found dead (documented in excel sheet as hatched, unmarked egg)
predation.boxes<-unique(eggs$box[is.na(eggs$hatched)])
predation.boxes<-length(predation.boxes)
#predation.boxes


#NESTING (init, incub, hatch)

#nest initiation COGO & BUFF (min, max, range, mean)

a.initiation<-data.frame(
  sp=visits$sp1[visits$fate==1 & visits$hatched==1 & visits$sp1 %in% c("COGO","BUFF")], 
  date=visits$date[visits$fate==1 & visits$hatched==1 & visits$sp1 %in% c("COGO","BUFF")],
  box=visits$box[visits$fate==1 & visits$hatched==1 & visits$sp1 %in% c("COGO","BUFF")],
  eggs=0,
  adj.eggs=0)
#a.initiation

for (b in 1:length(a.initiation$box)) {
  egg.list=visits$sp1eggs[visits$box == a.initiation$box[b]]
  a.initiation$eggs[b]=egg.list[which.max(egg.list)]
  a.initiation$adj.eggs[b]=a.initiation$eggs[b] 
  if(a.initiation$adj.eggs[b]>8){a.initiation$adj.eggs[b]=8}
}

a.initiation$adj.date=a.initiation$date-28
a.initiation$init.date=a.initiation$adj.date-(a.initiation$adj.eggs*2)

#a.initiation

init<-data.frame(sp=unique(a.initiation$sp), early=0, late=0, range=0, mean=0)
#init

for (c in 1:length(init$sp)){ 
  
  date.list=a.initiation$init.date[a.initiation$sp==init$sp[c]]
  init$early[c]=date.list[which.min(date.list)]
  init$late[c]=date.list[which.max(date.list)]
  init$range[c]=init$late[c]-init$early[c]
  init$mean[c]=mean(date.list)
  
}

#init


#nest initiation COME (min, max, range, mean)

a.initiationC<-data.frame(
  sp=visits$sp1[visits$fate==1 & visits$hatched==1 & visits$sp1=="COME"], 
  date=visits$date[visits$fate==1 & visits$hatched==1 & visits$sp1=="COME"],
  box=visits$box[visits$fate==1 & visits$hatched==1 & visits$sp1=="COME"],
  eggs=0,
  adj.eggs=0)
#a.initiationC

for (b in 1:length(a.initiationC$box)) {
  egg.list=visits$sp1eggs[visits$box == a.initiationC$box[b]]
  a.initiationC$eggs[b]=egg.list[which.max(egg.list)]
  a.initiationC$adj.eggs[b]=a.initiationC$eggs[b] 
  if(a.initiationC$adj.eggs[b]>8){a.initiationC$adj.eggs[b]=8}
}
#a.initiationC

a.initiationC$adj.date=a.initiationC$date-32
a.initiationC$init.date=a.initiationC$adj.date-(a.initiationC$adj.eggs*2)

#a.initiationC

init.COME<-data.frame(sp=unique(a.initiationC$sp), early=0, late=0, range=0, mean=0)
#init.COME

for (c in 1:length(init.COME$sp)){ 
  
  date.list=a.initiationC$init.date[a.initiationC$sp==init.COME$sp[c]]
  init.COME$early[c]=date.list[which.min(date.list)]
  init.COME$late[c]=date.list[which.max(date.list)]
  init.COME$range[c]=init.COME$late[c]-init.COME$early[c]
  init.COME$mean[c]=mean(date.list)
  
}

#init.COME



#incubation COGO & BUFF (min, max, range, mean)
a.initiation2<-data.frame(
  sp=visits$sp1[visits$fate==1 & visits$hatched==1 & visits$sp1!="BOOW"& visits$sp1!="COME"], 
  date=visits$date[visits$fate==1 & visits$hatched==1 & visits$sp1!="BOOW"& visits$sp1!="COME"],
  box=visits$box[visits$fate==1 & visits$hatched==1 & visits$sp1!="BOOW"& visits$sp1!="COME"])
#a.initiation2

a.initiation2$adj.date=a.initiation2$date-28
head(a.initiation2)

#a.initiation2

incub<-data.frame(sp=unique(a.initiation2$sp), early=0, late=0, range=0, mean=0)
#incub


for (g in 1:length(incub$sp)){ 
  date.list=a.initiation2$adj.date[a.initiation2$sp==incub$sp[g]]
  incub$early[g]=date.list[which.min(date.list)]
  incub$late[g]=date.list[which.max(date.list)]
  incub$range[g]=incub$late[g]-incub$early[g]
  incub$mean[g]=mean(date.list)
  
}

#incub

#incubation COME (min, max, range, mean)
a.initiation3<-data.frame(
  sp=visits$sp1[visits$fate==1 & visits$hatched==1 & visits$sp1=="COME"], 
  date=visits$date[visits$fate==1 & visits$hatched==1 & visits$sp1=="COME"],
  box=visits$box[visits$fate==1 & visits$hatched==1 & visits$sp1=="COME"])
#a.initiation3

a.initiation3$adj.date=a.initiation3$date-32
head(a.initiation3)

#a.initiation3

incub.COME<-data.frame(sp=unique(a.initiation3$sp), early=0, late=0, range=0, mean=0)
#incub.COME


for (g in 1:length(incub.COME$sp)){ 
  date.list=a.initiation3$adj.date[a.initiation3$sp==incub.COME$sp[g]]
  incub.COME$early[g]=date.list[which.min(date.list)]
  incub.COME$late[g]=date.list[which.max(date.list)]
  incub.COME$range[g]=incub.COME$late[g]-incub.COME$early[g]
  incub.COME$mean[g]=mean(date.list)
  
}

#incub.COME


#hatch date by sp (min, max, range, mean)

hatch<-data.frame(
  sp=unique(visits$sp1[visits$fate==1 & visits$hatched==1 & visits$sp1 != "BOOW"]),
  early=0, late=0, range=0, mean=0 )

a.hatch<-data.frame(
  sp=visits$sp1[visits$fate==1 & visits$hatched==1 & visits$sp1 != "BOOW"], 
  box=visits$box[visits$fate==1 & visits$hatched==1& visits$sp1 != "BOOW"],
  h.date=visits$date[visits$fate==1 & visits$hatched==1& visits$sp1 != "BOOW"]
)

for (c in 1:length(hatch$sp)){ 
  hatch$early[c]=min(a.hatch$h.date[a.hatch$sp==hatch$sp[c]])
  hatch$late[c]=max(a.hatch$h.date[a.hatch$sp==hatch$sp[c]])
  hatch$range[c]=hatch$late[c]-hatch$early[c]
  hatch$mean[c]=mean(a.hatch$h.date[hatch$sp[c]==a.hatch$sp])
}

#hatch



#NEST SUCCESS

# outcomes by sp (number and freq)
outcome.sp<-table(boxes$outcome, boxes$sp1)
#outcome.sp

outcome.sp.df<-as.data.frame(outcome.sp)
#outcome.sp.df
names(outcome.sp.df)<-c("outcome", "sp", "count")
#outcome.sp.df
total.sp<-aggregate(outcome.sp.df$count~outcome.sp.df$sp, FUN=sum)
names(total.sp)<-c("sp", "count")
#total.sp
outcome.sp.df<-merge(outcome.sp.df, total.sp, by="sp")
#outcome.sp.df
names(outcome.sp.df)<-c("sp", "outcome", "count", "total")
outcome.sp.df$pct<-outcome.sp.df$count/outcome.sp.df$total
#outcome.sp.df
outcome.freq<-reshape(outcome.sp.df[,c("sp", "outcome", "pct")], idvar="outcome", timevar="sp", direction="wide")
names(outcome.freq)<-c("outcome", as.character(unique(outcome.sp.df$sp)))
#outcome.freq

# hatched boxes by sp (number and freq)
hatched<-table(boxes$sp1[boxes$outcome=="missed" | boxes$outcome=="marked"])
#hatched

freq.hatched<- hatched/occupancy[names(occupancy) %in% names(hatched)]
#freq.hatched

# hatched boxes over time
# only considers marked COGO, BUFF, & COME boxes

hatched.time<- data.frame(
  box=visits$box[visits$fate==1 & visits$hatched==1 & visits$sp1 != "BOOW"],
  hatch.day=visits$date[visits$fate==1 & visits$hatched==1 & visits$sp1 != "BOOW"]
)


#hatched.time
hatch.time<-aggregate(hatched.time$hatch.day~hatched.time$box, FUN=min)

#hatched.time

hatch.days<-hatched.time$hatch.day
hatch.days<-table(hatch.days)
#hatch.days
plot(hatch.days, xlab="Julian Date", ylab="number of hatched boxes")



# number hatched COGO boxes by # eggs

b.clutch<-data.frame(
  sp=visits$sp1[visits$sp1=="COGO"], 
  date=visits$date[visits$sp1=="COGO"],
  box=visits$box[visits$sp1=="COGO"],
  primaryeggs=0,
  secondaryeggs=0,
  totaleggs=0, stringsAsFactors = F)

#b.clutch

for (b in 1:length(b.clutch$box)) {
  b.clutch$primaryeggs[b]=length(eggs$eggnum[b.clutch$sp[b]==eggs$sp & b.clutch$box[b]==eggs$box])
  b.clutch$secondaryeggs[b]=length(eggs$eggnum[b.clutch$box[b]==eggs$box])-b.clutch$primaryeggs[b]
  b.clutch$totaleggs[b]=b.clutch$primaryeggs[b] + b.clutch$secondaryeggs[b]
  
}
#b.clutch


c.clutch<-data.frame(
  box=boxes$box[boxes$sp1=="COGO"],
  outcome=0,
  sp.clutch=0,
  total.clutch=0
  
)



for (m in 1:length(c.clutch$box)){
  c.clutch$outcome[m]=boxes$outcome[c.clutch$box[m]==boxes$box]
  c.clutch$sp.clutch[m]=max(b.clutch$primaryeggs[b.clutch$box==c.clutch$box[m]]) 
  c.clutch$total.clutch[m]=max(b.clutch$totaleggs[b.clutch$box==c.clutch$box[m]]) 
}


####remove rows
c.clutch<-c.clutch[c.clutch$outcome == "missed"|c.clutch$outcome=="marked",]

#c.clutch

sp.clutch.tab<-table(c.clutch$sp.clutch)
total.clutch.tab<-table(c.clutch$total.clutch)

#sp.clutch.tab
#total.clutch.tab

plot(sp.clutch.tab, ylab="number hatches", xlab ="sp clutch size")
plot(total.clutch.tab, ylab= "number hatches", xlab ="total clutch size")


# hatched COGO boxes by init date 

p.initiation<-data.frame(
  sp=visits$sp1[visits$fate==1 & visits$hatched==1 & visits$sp1!="BOOW"], 
  date=visits$date[visits$fate==1 & visits$hatched==1 & visits$sp1!="BOOW"],
  box=visits$box[visits$fate==1 & visits$hatched==1 & visits$sp1!="BOOW"],
  outcome=0,
  eggs=0,
  adj.eggs=0)
#p.initiation

for (b in 1:length(p.initiation$box)) {
  p.initiation$outcome[b]=boxes$outcome[p.initiation$box[b]==boxes$box]
  egg.list=visits$sp1eggs[visits$box == p.initiation$box[b]]
  p.initiation$eggs[b]=egg.list[which.max(egg.list)]
  p.initiation$adj.eggs[b]=p.initiation$eggs[b] 
  if(p.initiation$adj.eggs[b]>8){p.initiation$adj.eggs[b]=8}

}

#p.initiation
p.initiation$adj.date=p.initiation$date-28
p.initiation$init.date=p.initiation$adj.date-(p.initiation$adj.eggs*2)

#p.initiation

init.hatch.COGO<-p.initiation[p.initiation$sp=="COGO",]
#init.hatch.COGO

init.hatch.COGO<-table(init.hatch.COGO$init.date)
#init.hatch.COGO
plot(init.hatch.COGO, xlab="initiation date", ylab="number hatched boxes")



#EGG SUCCESS

# number and freq COGO eggs hatched in COGO boxes
a.COGO.eggs<-data.frame(
  box=eggs$box[eggs$hatched==1 & eggs$sp=="COGO" & !is.na(eggs$hatched)],
  sp1=0
)

for (t in 1:length(a.COGO.eggs$box)){
  a.COGO.eggs$sp1[t]=boxes$sp1[a.COGO.eggs$box[t]==boxes$box]
  
}

COGO.eggs<-table(a.COGO.eggs$sp1)
COGO.eggs.prop<-prop.table(COGO.eggs)


# hatched eggs by sp
hatched.sp<-table(eggs$sp[eggs$hatched==1])
#hatched.sp

all.eggs<-table(eggs$sp[!(eggs$sp %in% c("UNOC", "RDSQ"))])
#all.eggs
hatched.freq<- hatched.sp/all.eggs
#hatched.freq



#HATCHLINGS

# hatchlings marked by sp (1==marked)
marked.sp<-table(eggs$sp[eggs$marked==1])
#marked.sp

# freq hatchlings marked
hatched.sp<-table(eggs$sp[eggs$hatched==1])
hatched.sp.freq<-marked.sp/hatched.sp[names(hatched.sp) %in% names(marked.sp)]



#HEN CAPTURE

# hen status by sp
hen.df<-data.frame(
  sp=boxes$sp1[!(boxes$sp1 %in% c("UNOC", "RDSQ"))], 
  box=boxes$box[!(boxes$sp1 %in% c("UNOC", "RDSQ"))], 
  recap=boxes$recap[!(boxes$sp1 %in% c("UNOC", "RDSQ"))], 
  new=boxes$new[!(boxes$sp1 %in% c("UNOC", "RDSQ"))], 
  uncap=0
)
#hen.df

for(j in 1:length(hen.df$box)) {
  if (is.na(hen.df$recap[j])){hen.df$uncap[j]=1}
}
#hen.df

hen.status<-aggregate(cbind(recap, new,uncap)~sp, data=hen.df, FUN=sum)
hen.status<-aggregate(cbind(recap,new,uncap)~sp, data=hen.df, FUN=sum, na.rm=T, na.action=na.pass)
#hen.status

# number of hens with other markers, by sp
hen.mark<-data.frame(
  box=boxes$box[boxes$sp1 %in% c("COGO", "BUFF", "COME", "OTHER")],
  sp=boxes$sp1[boxes$sp1 %in% c("COGO", "BUFF", "COME", "OTHER")],
  webtag=boxes$webtag[boxes$sp1 %in% c("COGO", "BUFF", "COME", "OTHER")],
  plast=boxes$plast[boxes$sp1 %in% c("COGO", "BUFF", "COME", "OTHER")]
)

hen.mark<-hen.mark[hen.mark$webtag!="" | hen.mark$plast !="",]
#hen.mark



# COGO hen capture status by nest initiation date   
#cap.init<-data.frame(box=boxes$box[boxes$outcome=="marked" & boxes$sp1=="COGO"],
  #sp=boxes$sp1[boxes$outcome=="marked"& boxes$sp1=="COGO"],
  #recap=boxes$recap[boxes$outcome=="marked"& boxes$sp1=="COGO"],
  #new=boxes$new[boxes$outcome=="marked"& boxes$sp1=="COGO"],
  #init.date=0)

#cap.init

#a.initiation

#for(g in 1:length(cap.init$box)){cap.init$init.date[g]=a.initiation$init.date[cap.init$box[g]==a.initiation$box]}


#cap.init

#cap.init.ag<-aggregate(cbind(recap, new)~init.date, data=cap.init, FUN=sum)


#hen.status.plot<-ggplot(data=cap.init, aes(x=init.date, fill=factor(recap, labels=c("new", "recap")), group=recap)) + 
  #geom_bar(position = position_dodge2(preserve = "single"), width = 2) +
  #scale_fill_discrete(name = "hen status")



