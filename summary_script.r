
#Version 1 of goldeneye script
#Created by Hannah Vincelette and Chuck Frost
#Creation date: 7/27/2018



#read in the most current data file

visits<-read.csv("R_2018_COGO_Box_Checks.csv", header=T, stringsAsFactors = F)
visits<-visits[ ,1:13]

boxes<-read.csv("R_2018_COGO_Box_Inventory.csv", header=T, stringsAsFactors = F)

eggs<-read.csv("R_2018_COGO_Egg_Fate.csv", header=T, stringsAsFactors = F)
eggs<-eggs[,1:7]

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
occupancy.birds<-occupancy.table[!(names(occupancy.table) %in% c("UNOC", "RDSQ"))]
occupancy.birds

  #occupancy count w/o RDSQ
x.occupancy.total <- length(boxes$box[boxes$sp1 != "UNOC" & boxes$sp1 != "RDSQ"])
occupancy.total<-sum(occupancy.table[!(names(occupancy.table) %in% c("UNOC", "RDSQ"))])
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
occupancy.freq.150<-prop.table(occupancy.table)
occupancy.freq.150


  #occupancy frequency BOOW (prop 150 boxes)
occupancy.freq.150.BOOW <- prop.table(occupancy.table)[1]
occupancy.freq.150.BOOW

  #occupancy frequencies w/o UNOC
occupancy.freq.sp.150<-prop.table(occupancy.table)[(names(occupancy) != "UNOC")]
occupancy.freq.sp.150

  #occupancy frequencies w/o UNOC or RDSQ (prop 150 boxes)
occupancy.freq.birds.150<-prop.table(occupancy.table)[!(names(occupancy.table) %in% c("UNOC", "RDSQ"))]
occupancy.freq.birds.150


  #total occupancy frequency w/o UNOC or RDSQ
total.occupancy.freq<-sum(prop.table(occupancy.table)[1:4])
total.occupancy.freq<-total.occupancy/length(boxes$box)
total.occupancy.freq


  #unoccupied box frequency; total RDSQ and UNOC occupancy frequencies
unoccupied<-sum(prop.table(occupancy.table)[c("UNOC", "RDSQ")])
unoccupied

  #occupancy freq by sp w/ RDSQ (prop occupied boxes)
occupancy.freq.sp.100<-prop.table(occupancy.sp)
occupancy.freq.sp.100

  #occupancy frequencies by sp w/o RDSQ (prop occupied boxes)
occupancy.freq.birds.97<-prop.table(occupancy.birds)
occupancy.freq.birds.97



#HEN CAPTURE
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
new.hen.sp.freq<-new.hen.sp/occupancy.birds
new.hen.sp.freq

  #recap hens frequencies by sp w/o RDSQ or UNOC
recap.hen.sp.freq<-recap.hen.sp/occupancy.birds
recap.hen.sp.freq


  #uncaptured hens by sp 
uncap<-table(boxes$sp1[!(boxes$sp1 %in% c("UNOC", "RDSQ")) & boxes$band==""])
uncap

#total uncaptured hens
total.uncap<-sum(uncap)
total.uncap

  #uncaptured hens frequencies by sp (prop of each sp occupancy)
uncap.freq<-uncap/occupancy.birds
uncap.freq<- uncap/occupancy.birds[c("COGO", "COME")]
uncap.freq


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
outcome.freq.sp<-prop.table(outcome.sp)
outcome.freq.sp
  

  #marked count
marked.total<-length(boxes$outcome[boxes$outcome=="marked"])
marked.total

  #marked COGO count
marked.COGO<-length(boxes$outcome[boxes$outcome=="marked" & boxes$sp1=="COGO"])
marked.COGO

  #marked counts by sp
marked<-table(boxes$sp1[boxes$outcome=="marked"])
marked

  #marked freq by sp
marked.freq<-marked/occupancy.birds
marked.freq


  #missed count
missed.total<- length(boxes$outcome[boxes$outcome=="missed"])
missed.total

 #missed counts by sp
missed<-table(boxes$sp1[boxes$outcome=="missed"])
missed

  #missed frequency only
missed.freq<-outcome.freq["missed"]
missed.freq

  #missed frequencies by sp
missed.freq.sp<-missed/occupancy.birds
missed.freq.sp


  #dump count 
dump.total<- length(boxes$outcome[boxes$outcome=="dump"])
dump.total

  #dump counts by sp
dump<-table(boxes$sp1[boxes$outcome=="dump"])
dump

  #dump frequency only
dump.freq<-outcome.freq["dump"]
dump.freq

  #dump frequencies by sp
dump.freq.sp<-dump/occupancy.birds[, boxes$outcome=="dump"]
dump.freq.sp


  #n-abandon count 
n.abandon.total<- length(boxes$outcome[boxes$outcome=="n-abandon"])
n.abandon.total

  #n-abandon counts by sp
n.abandon<-table(boxes$sp1[boxes$outcome=="n-abandon"])
n.abandon

  #n-abandon frequency only
n.abandon.freq<-outcome.freq["n-abandon"]
n.abandon.freq

  #n-abandon frequencies by sp
n.abandon.freq.sp<-n.abandon/occupancy.birds[, boxes$outcome=="n-abandon"]
n.abandon.freq.sp


  #r-abandon count 
r.abandon.total<- length(boxes$outcome[boxes$outcome=="r-abandon"])
r.abandon.total

  #r-abandon counts by sp
r.abandon<-table(boxes$sp1[boxes$outcome=="r-abandon"])
r.abandon

  #r-abandon frequency only
r.abandon.freq<-outcome.freq["r-abandon"]
r.abandon.freq

  #r-abandon frequencies by sp
r.abandon.freq.sp<-r.abandon/occupancy.birds[, boxes$outcome=="r-abandon"]
r.abandon.freq.sp

  #u-abandon count 
u.abandon.total<- length(boxes$outcome[boxes$outcome=="u-abandon"])
u.abandon.total

  #u-abandon counts by sp
u.abandon<-table(boxes$sp1[boxes$outcome=="u-abandon"])
u.abandon

  #u-abandon frequency only
u.abandon.freq<-outcome.freq["u-abandon"]
u.abandon.freq

  #u-abandon frequencies by sp
u.abandon.freq.sp<-u.abandon/occupancy.birds[, boxes$outcome=="u-abandon"]
u.abandon.freq.sp

  #total abandon by sp
outcome.sp<-table(boxes$outcome[!(boxes$sp1 %in% c("UNOC", "RDSQ"))], boxes$sp1[!(boxes$sp1 %in% c("UNOC", "RDSQ"))])
x.outcome.sp<-outcome.sp[c("r-abandon", "n-abandon", "u-abandon"),]
colSums(x.outcome.sp)

  #unknown count 
unknown.total<- length(boxes$outcome[boxes$outcome=="unknown"])
unknown.total

  #unknown counts by sp
unknown<-table(boxes$sp1[boxes$outcome=="dunknown"])
unknown

  #unknown frequency only
unknown.freq<-outcome.freq["unknown"]
unknown.freq

  #unknown frequencies by sp
unknown.freq.sp<-unknown/occupancy.birds[, boxes$outcome=="unknown"]
unknown.freq.sp



#EGG-LAYING & INCUBATION


  #earliest nest initiation COGO
a.initiation<-aggregate(visits$date[visits$fate==1 & visits$hatched==1 & visits$sp1=="COGO"], by=list(Category=visits$box[visits$fate==1 & visits$hatched==1 & visits$sp1=="COGO"]), FUN=min)
a.initiation
b.initiation<-a.initiation[2]-28
b.initiation

c.initiation<-aggregate(visits$sp1eggs[visits$fate==1 & visits$hatched==1 & visits$sp1=="COGO"], by=list(Category=visits$box[visits$fate==1 & visits$hatched==1 & visits$sp1=="COGO"]), FUN=max)
c.initiation
#only includes egg counts from hatch date (lower than it should be)
d.initiation<-c.initiation[2]-0
d.initiation
d.initiation[d.initiation>8]<- 8
d.initiation
e.initiation<-d.initiation*2
e.initiation

f.initiation<-b.initiation-e.initiation
f.initiation
early.initiation<-min(f.initiation)
early.initiation



#latest nest initiation COGO
late.initiation<-max(f.initiation)
late.initiation


#earliest nest initiation by sp

#latest nest initiation by sp
 
#nest initiation range by sp



#mean nest initiation by sp


#earliest hatch date by sp

#latest hatch date by sp

#hatch date range by sp

#mean hatch date by sp


#smallest clutch by sp

#largest clutch by sp

#clutch size range by sp

#mean clutch size

#count nests with other sp dump eggs (by sp)

#freq nests with other sp dump eggs (by sp)



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
hatched.sp<-table(eggs$sp[eggs$hatched==1], eggs$hatched[eggs$hatched==1])
hatched.sp

#freq hatched eggs


#freq hatched eggs by sp



#HATCHLINGS

  #sum web-tagged ducklings
webtagged<-length(eggs$webtag[eggs$webtag != ""])
webtagged

  #sum banded owls
banded.owlets<-length(eggs$band[eggs$band !=""])
banded.owlets

  #sum marked hatchlings by sp (1=marked)
marked.sp<-table(eggs$sp[!(eggs$sp %in% c("UNOC", "RDSQ"))], eggs$marked[!(eggs$sp %in% c("UNOC", "RDSQ"))])
marked.sp<-table(eggs$sp[eggs$marked==1], eggs$marked[eggs$marked==1])
marked.sp

  #frequency of marked ducklings
marked.freq<-marked.sp/hatched.sp
marked.freq
