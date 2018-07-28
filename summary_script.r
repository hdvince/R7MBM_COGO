
# Version 1 of goldeneye script. Created by Hannah Vincelette and Chuck Frost. 
# Creation date: 7/27/2018




#read in the most current data file
visits<-read.csv("R_2018_COGO_Box_Checks.csv", header=T, stringsAsFactors = F)
visits<-visits[ ,1:13]


boxes<-read.csv("R_2018_COGO_Box_Inventory.csv", header=T, stringsAsFactors = F)

eggs<-read.csv("R_2018_COGO_Egg_Fate.csv", header=T, stringsAsFactors = F)
eggs<-eggs[,1:7]


#calculate box occupancy
occupancy<-length(boxes$box[boxes$sp1 != "UNOC"])

occupancy<-length(boxes[boxes$sp1 != "UNOC",1])


#occupancy table by sp
occupancy<-table(boxes$sp1)

#occupancy by sp, only birds
occupancy.birds<-occupancy[!(names(occupancy) %in% c("UNOC", "RDSQ"))]

#occupancy plot by sp
occupancy.plot plot(table(boxes$sp1), xlab="species", ylab="# boxes", ylim=c(0,100))

#occupancy table of frequencies by sp
occupancy.freq<-prop.table(occupancytable)


#occupancy table of frequencies, 1 sp
occupancy.freq.BOOW <- prop.table(occupancytable)[1]

#pull out two sp
#names(occupancy.freq) %in% c("UNOC", "RDSQ")

#reverse
#!(names(occupancy.freq) %in% c("UNOC", "RDSQ"))

#occupancy frequencies of some sp
occupancy.bird.sp<-occupancy.freq[!(names(occupancy.freq) %in% c("UNOC", "RDSQ"))]

#total occupancy (no RDSQ)
total.occupancy<-sum(prop.table(occupancytable)[1:4])
total.occupancy<-sum(occupancy[!(names(occupancy) %in% c("UNOC", "RDSQ"))])

#total RDSQ and UNOC occupancy frequencies
total.occupancy<-sum(prop.table(occupancytable)[c("UNOC", "RDSQ")])


#sum new hens
new.hen<-sum(boxes$new, na.rm=T)

#sum new hens by sp
new.hen.sp<-table(boxes$new, boxes$sp1)


#uncaptured hens by sp
uncap<-table(boxes$sp1[!(boxes$sp1 %in% c("UNOC", "RDSQ")) & boxes$band==""])

#total uncaptured hens
total.uncap<-sum(uncap)

#freq uncaptured hens out of occupied boxes
uncap.prop <- uncap/total.occupancy

#uncaptured hens one sp
uncap.COGO<-uncap["COGO"]


#hatch success by sp
hatches<-table(boxes$sp1[boxes$outcome==c("marked", "missed")]
               + )

#hatch success freq by sp
hatches/occupancy[!(names(occupancy) %in% c("UNOC", "RDSQ"))]






