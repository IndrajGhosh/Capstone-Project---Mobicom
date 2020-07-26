library("dplyr")
setwd("C:\\Users\\indra\\Desktop\\JIGSAW\\#9 Capstone Project")
dat<-read.csv("sampletelecomfinal.csv")
str(dat)
colSums(is.na(dat))

#Dropping NA columns
indx<-which(names(dat) %in% c("mailordr","occu1","numbcars","retdays","wrkwoman","solflag","proptype","mailresp","cartype","car_buy","children","div_type"))
dat2 <- dat[,-indx]
names(dat2)

dat <- dat2
str(dat)

DataType<-list()
NoOfRecords<-list()
UniqueRecords<-list()
DataAvailable<-list()
AvailablePercent<-list()
Missing<-list()
MissingPercent<-list()
Minimum<-list()
Maximum<-list()
Mean<-list()
`5th Percentile` <-list()
`10th Percentile`<-list()
`25th Percentile`<-list()
`50th Percentile`<-list()
`75th Percentile`<-list()
`90th Percentile`<-list()
`95th Percentile`<-list()


j<-1
#class(dat[,"mou_Mean"])
for (i in names(dat)){
  #DataType1[j]<-class(dat[,i])
  DataType[j]<-class(dat[[i]])    # dat$i, "dat$i" dat[[i]] dat[,i]
  NoOfRecords[j]<-length(dat[[i]])
  UniqueRecords[j]<-length(unique(dat[[i]]))
  DataAvailable[j]<-(length(dat[[i]])-sum(is.na(dat[[i]])))
  AvailablePercent[j]<-(as.numeric(DataAvailable[j])/as.numeric(NoOfRecords[j]))
  Missing[j]<-sum(is.na(dat[[i]]))
  MissingPercent[j]<-(as.numeric(Missing[j])/as.numeric(NoOfRecords[j]))
  if (class(dat[[i]])=="character"){
    Minimum[j]<-0
    Maximum[j]<-0
    Mean[j]<-0
    
    `5th Percentile`[j]<-0
    `10th Percentile`[j]<-0
    `25th Percentile`[j]<-0
    `50th Percentile`[j]<-0
    `75th Percentile`[j]<-0
    `90th Percentile`[j]<-0
    `95th Percentile`[j]<-0
    
    
  } else {
    Minimum[j]<-min(as.numeric(dat[[i]]), na.rm = TRUE)
    Maximum[j]<-max(as.numeric(dat[[i]]),na.rm = TRUE)
    Mean[j]<- mean(as.numeric(dat[[i]]),na.rm = TRUE)

    `5th Percentile`[j]<-quantile(as.numeric(dat[[i]]),c(0.05),na.rm = TRUE)
    `10th Percentile`[j]<- quantile(as.numeric(dat[[i]]),c(0.1),na.rm = TRUE)
    `25th Percentile`[j]<- quantile(as.numeric(dat[[i]]),c(0.25),na.rm = TRUE)
    `50th Percentile`[j]<-quantile(as.numeric(dat[[i]]),c(0.5),na.rm = TRUE)
    `75th Percentile`[j] <- quantile(as.numeric(dat[[i]]),c(0.75),na.rm = TRUE)
    `90th Percentile`[j]<-quantile(as.numeric(dat[[i]]),c(0.90),na.rm = TRUE)
    `95th Percentile`[j]<-quantile(as.numeric(dat[[i]]),c(0.95),na.rm = TRUE)
  }

  j<-j+1
}



DataType<-unlist(DataType)
NoOfRecords<-unlist(NoOfRecords)
UniqueRecords<- unlist(UniqueRecords)
DataAvailable<-unlist(DataAvailable)
AvailablePercent<-unlist(AvailablePercent)
Missing<-unlist(Missing)
MissingPercent<-unlist(MissingPercent)
Minimum<-unlist(Minimum)
Maximum<-unlist(Maximum)
Mean<-unlist(Mean)
`5th Percentile` <-unlist(`5th Percentile`)
`10th Percentile`<-unlist(`10th Percentile`)
`25th Percentile`<-unlist(`25th Percentile`)
`50th Percentile`<-unlist(`50th Percentile`)
`75th Percentile`<-unlist(`75th Percentile`)
`90th Percentile`<-unlist(`90th Percentile`)
`95th Percentile`<-unlist(`95th Percentile`)

cb <-cbind(DataType, NoOfRecords,UniqueRecords,DataAvailable,AvailablePercent,Missing,MissingPercent,Minimum,Maximum,Mean,`5th Percentile`,`10th Percentile`,`25th Percentile`,`50th Percentile`,`75th Percentile`,`90th Percentile`,`95th Percentile`)
class(as.data.frame(cbind(DataType, NoOfRecords)))

QualityReport <- as.data.frame(cb)
rownames(QualityReport)<-colnames(dat)
head(QualityReport)

write.csv(QualityReport,"C:\\Users\\indra\\Desktop\\JIGSAW\\#9 Capstone Project\\SampleQuality_Report (final).csv")

# Continuous Variable Profiling
str(dat2)
names(dat2)
#models,actvsub,uniqsubs,dwlltype,mtrcycle,truck,churn  --- yet to convert to categorical variables
index <- which(names(dat2) %in% c("income","crclscod","asl_flag","prizm_social_one","area","refurb_new","hnd_webcap","marital","ethnic","age1","age2","models","hnd_price","actvsubs","uniqsubs","forgntvl","dwlltype","dwllsize","mtrcycle","truck","churn","csa"))
dat2_cont <- dat2[,-index]
names(dat2_cont)
dat2_cat <- dat2[,index]
names(dat2_cat)

#Adults,car_buy,cartype,children,churn,creditcd,crtcount,div_type,dualband,educ1,hhstatin,infobase,(kid0_2,kid3_5,kid6_10,kid11_15,kid16_17),last_swap,lor,mailflag,mailordr,mailresp,new_cell,numbcars,occu1,ownrent,pcowner,phones,pre_hnd_price,proptype,ref_qty,rv,solflag,tot_acpt,tot_ret,wrkwoman

dat2_cont$churn <- dat2_cat$churn
unique(dat2_cont$churn)


names(dat2)
dat2%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat45
class(dat45)
dat45
dat45$N<-unclass(dat2%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
dat45$churn_perc<-dat45$n/dat45$N
dat45$GreaterThan<-unclass(dat2%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
dat45$LessThan<-unclass(dat2%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
dat45$varname<-rep("avgrev",nrow(dat45))
dat45

write.csv(dat45,"C:\\Users\\indra\\Desktop\\JIGSAW\\#9 Capstone Project\\churnpercentage.csv")
names(dat2_cont)

dat2_cont%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat45
dat45$N<-unclass(dat2%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat45$churn_perc<-dat45$n/dat45$N
dat45$GreaterThan<-unclass(dat2%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat45$LessThan<-unclass(dat2%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat45$varname<-rep("totrev",nrow(dat45))
dat45

cor(dat2_cont[,1:44])
#avgmou*months = totcalls, as we don't want redundant variable we are removing avgmou
#avgqty*months=totrev, as avgqty is redundant information as the variation is already captured in totrev variable we are removing avgqty
#avg3mou,avg3qty information is captured in avg6mou,avg6qty. So we are removing both of them
index <- which(names(dat2) %in% c("mou_Mean","totmrc_Mean","mou_Range","change_mou","drop_blk_Mean","drop_vce_Range","owylis_vce_Range","mou_opkv_Range","months","eqpdays","totcalls","iwylis_vce_Mean","rev_Mean","totrev","avg6mou","avg6qty","avg3mou","avg3qty","avgmou","avgqty","avgrev","rev_Mean","drop_dat_Mean","drop_vce_Mean","mou_pead_Mean","opk_dat_Mean","datovr_Mean","ovrmou_Mean","blck_dat_Mean","totrev"))
length(index)
dat2_cont_req <- dat2[,index]
names(dat2_cont_req)


cor(dat2_cont_req[,1:18])


#categorical variable profiling
dat2_cat%>%count(churn,levels=age2)%>%filter(churn==1)->datC1
datC1$N<-unclass(dat2_cat%>%filter(age2%in%datC1$levels)%>%count(age2))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N 
datC1$Var.Name<-rep("age2",nrow(datC1))
datC1
dat2_cat%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datC1
datC1$N<-unclass(dat2_cat%>%filter(actvsubs%in%datC1$levels)%>%count(actvsubs))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("actvsubs",nrow(datC1))
datC1


index <- which(names(dat2) %in% c("income","crclscod","prizm_social_one","area","marital","ethnic","age1","age2","models","hnd_price","actvsubs","asl_flag","uniqsubs","dwllsize","csa","churn","Customer_ID"))
dat2_cat_req <- dat2[,index]

names(dat2_cat_req)

#coverting class of categorical variables from character to factors
dat2_cat_req_factor<-list()
for (i in names(dat2_cat_req)){
  dat2_cat_req_factor[[i]]<-as.factor(dat2_cat_req[[i]])
}
dat2_cat_req_factor<-as.data.frame(dat2_cat_req_factor)
write.csv(dat2_cont_req,"C:\\Users\\indra\\Desktop\\JIGSAW\\#9 Capstone Project\\dat2_cont_req.csv")

dat2_req_final<-cbind(dat2_cont_req,dat2_cat_req_factor)
colSums(is.na(dat2_req_final))

hist(dat2_cont_req$mou_Mean)
quantile(dat2_cont_req$mou_Mean,p=c(1:100)/100, na.rm=TRUE)
indx<-which(is.na(dat2_req_final$mou_Mean))
dat2_req_final<-dat2_req_final[-indx,]

indx<-which(is.na(dat2_req_final$avg6mou))
dat2_req_final<-dat2_req_final[-indx,]

indx<-which(is.na(dat2_req_final$marital))
dat2_req_final<-dat2_req_final[-indx,]

indx<-which(is.na(dat2_req_final$area))
dat2_req_final<-dat2_req_final[-indx,]

indx<-which(is.na(dat2_req_final$hnd_price))
dat2_req_final<-dat2_req_final[-indx,]

colSums(is.na(dat2_req_final))
names(dat2_req_final)
#income is 29 and dwllsize is 42
dat2_req_final<-dat2_req_final[,-c(29,42)]
names(dat2_req_final)

summary(dat2_req_final$prizm_social_one)
indx<-which(is.na(dat2_req_final$prizm_social_one))
dat2_req_final[indx,"prizm_social_one"]<-"U" 

colSums(is.na(dat2_req_final))

names(dat2_req_final)
dat2_req_final <-dat2_req_final[,-c(42)] #removing csa

#totrev - NA's
summary(dat2_req_final$totrev)
indx<-which(is.na(dat2_req_final$totrev))
dat2_req_final[indx,"totrev"]<- mean(dat2_req_final$totrev,na.rm = TRUE)

colSums(is.na(dat2_req_final))

#Change_mou -NA's
summary(dat2_req_final$change_mou)
indx<-which(is.na(dat2_req_final$change_mou))
dat2_req_final[indx,"change_mou"]<-  -4.750  
# replaced with median

write.csv(dat2_req_final,"C:\\Users\\indra\\Desktop\\JIGSAW\\#9 Capstone Project\\dat2_req_final.csv")

#Outliers in continuous variables
bx<-boxplot(dat2_req_final$mou_Mean)
quantile(dat2_req_final$mou_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$mou_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out


indx<-which(dat2_req_final[,"mou_Mean"]>4000)
dat2_req_final[indx,"mou_Mean"]<- 4000 #above 4000 because after 99.9 percentile all values are around 4000


bx<-boxplot(dat2_req_final$totmrc_Mean)
bx
quantile(dat2_req_final$totmrc_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$totmrc_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
summary(dat2_req_final$totmrc_Mean)
hist(dat2_req_final$totmrc_Mean)


indx<-which(dat2_req_final[,"totmrc_Mean"]>300)
dat2_req_final[indx,"totmrc_Mean"]<- 233 #above 300 because 3rd largest value is 233 so imputing 399 and 309 with 233



bx<-boxplot(dat2_req_final$mou_Range)
bx
quantile(dat2_req_final$mou_Range,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$mou_Range,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$mou_Range)


indx<-which(dat2_req_final[,"mou_Range"]>4000)
dat2_req_final[indx,"mou_Range"]<- 3545.47 #before imputing 99.9 percentile is 3545.47

names(dat2_req_final)
bx<-boxplot(dat2_req_final$drop_blk_Mean)
bx
quantile(dat2_req_final$drop_blk_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$drop_blk_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$drop_blk_Mean)

indx<-which(dat2_req_final[,"drop_blk_Mean"]>200)
dat2_req_final[indx,"drop_blk_Mean"]<- 157.98533 #before imputing 99.9 percentile is 157.98533


names(dat2_req_final)
bx<-boxplot(dat2_req_final$drop_vce_Range)
bx
quantile(dat2_req_final$drop_vce_Range,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$drop_vce_Range,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$drop_vce_Range)

indx<-which(dat2_req_final[,"drop_vce_Range"]>103)
dat2_req_final[indx,"drop_vce_Range"]<- 82.99 #before imputing 99.9 percentile is 82.99


names(dat2_req_final)
bx<-boxplot(dat2_req_final$owylis_vce_Range)
bx
quantile(dat2_req_final$owylis_vce_Range,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$owylis_vce_Range,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$owylis_vce_Range)

indx<-which(dat2_req_final[,"owylis_vce_Range"]>250)
dat2_req_final[indx,"owylis_vce_Range"]<- 246.49 #before imputing 99.9 percentile is 246.49



names(dat2_req_final)
bx<-boxplot(dat2_req_final$mou_opkv_Range)
bx
quantile(dat2_req_final$mou_opkv_Range,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$mou_opkv_Range,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$mou_opkv_Range)

indx<-which(dat2_req_final[,"mou_opkv_Range"]>1800)
dat2_req_final[indx,"mou_opkv_Range"]<- 1637.56 #before imputing 99.9 percentile is 1637.56


names(dat2_req_final)
bx<-boxplot(dat2_req_final$totcalls)
bx
quantile(dat2_req_final$totcalls,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$totcalls,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$totcalls)

indx<-which(dat2_req_final[,"totcalls"]>55000)
dat2_req_final[indx,"totcalls"]<- 46307.92 #before imputing 99.9 percentile is 46307.92


names(dat2_req_final)
bx<-boxplot(dat2_req_final$eqpdays)
bx
quantile(dat2_req_final$eqpdays,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$eqpdays,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$eqpdays)

indx<-which(dat2_req_final[,"eqpdays"]>1500)
dat2_req_final[indx,"eqpdays"]<- 1500 #before imputing 99.9 percentile is 1500


names(dat2_req_final)
bx<-boxplot(dat2_req_final$iwylis_vce_Mean)
bx
quantile(dat2_req_final$iwylis_vce_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$iwylis_vce_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$iwylis_vce_Mean)
indx<-which(dat2_req_final[,"iwylis_vce_Mean"]>150)
dat2_req_final[indx,"iwylis_vce_Mean"]<- 150.32 #before imputing 99.9 percentile is 150.32


names(dat2_req_final)
bx<-boxplot(dat2_req_final$rev_Mean)
bx
quantile(dat2_req_final$rev_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$rev_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$rev_Mean)
indx<-which(dat2_req_final[,"rev_Mean"]>3000)
dat2_req_final[indx,"rev_Mean"]<- 428.29 #before imputing 99.9 percentile is 428.29


names(dat2_req_final)
bx<-boxplot(dat2_req_final$totrev)
bx
quantile(dat2_req_final$totrev,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$totrev,p=c(0:100)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$totrev)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$avg6mou)
bx
quantile(dat2_req_final$avg6mou,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$avg6mou,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$avg6mou)

indx<-which(dat2_req_final[,"avg6mou"]>3000)
dat2_req_final[indx,"avg6mou"]<- 2846.95 #before imputing 99.9 percentile is 2846.95


names(dat2_req_final)
bx<-boxplot(dat2_req_final$avg6qty)
bx
quantile(dat2_req_final$avg6qty,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$avg6qty,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$avg6qty)

indx<-which(dat2_req_final[,"avg6qty"]>2000)
dat2_req_final[indx,"avg6qty"]<- 1541.44 #before imputing 99.9 percentile is 1541.44


names(dat2_req_final)
bx<-boxplot(dat2_req_final$totrev)
bx
quantile(dat2_req_final$totrev,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$totrev,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$totrev)

indx<-which(dat2_req_final[,"totrev"]>8000)
dat2_req_final[indx,"totrev"]<- 7765.58 #before imputing 99.9 percentile is 7765.58


names(dat2_req_final)
bx<-boxplot(dat2_req_final$change_mou)
bx
quantile(dat2_req_final$change_mou,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$change_mou,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$change_mou)

indx<-which(dat2_req_final[,"change_mou"]>5000)
dat2_req_final[indx,"change_mou"]<- 1612.63 #before imputing 99.9 percentile is 1612.63

summary(dat2_req_final$change_mou)
str(dat2_req_final)
summary(dat2_req_final)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$avg3mou)
bx
quantile(dat2_req_final$avg3mou,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$avg3mou,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$avg3mou)

indx<-which(dat2_req_final[,"avg3mou"]>4000)
dat2_req_final[indx,"avg3mou"]<- 3673 #before imputing 99.9 percentile is 3673

summary(dat2_req_final$avg3mou)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$avg3qty)
bx
quantile(dat2_req_final$avg3qty,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$avg3qty,p=c(9990:10000)/10000, na.rm=TRUE)
bx$out
hist(dat2_req_final$avg3qty)

indx<-which(dat2_req_final[,"avg3qty"]>2100)
dat2_req_final[indx,"avg3qty"]<- 1547.95 #before imputing 99.9 percentile is 1547.95

summary(dat2_req_final$avg3qty)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$avgmou)
bx
quantile(dat2_req_final$avgmou,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$avgmou,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$avgmou)

indx<-which(dat2_req_final[,"avgmou"]>4000)
dat2_req_final[indx,"avgmou"]<- 2972.29 #before imputing 99.9 percentile is 2972.29

summary(dat2_req_final$avgmou)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$avgqty)
bx
quantile(dat2_req_final$avgqty,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$avgqty,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$avgqty)

indx<-which(dat2_req_final[,"avgqty"]>1500)
dat2_req_final[indx,"avgqty"]<- 1427.27 #before imputing 99.9 percentile is 1427.27

summary(dat2_req_final$avgqty)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$avgrev)
bx
quantile(dat2_req_final$avgrev,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$avgrev,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$avgrev)

indx<-which(dat2_req_final[,"avgrev"]>1500)
dat2_req_final[indx,"avgrev"]<- 1427.27 #before imputing 99.9 percentile is 1427.27

summary(dat2_req_final$avgqty)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$rev_Mean)
bx
quantile(dat2_req_final$rev_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$rev_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$rev_Mean)

indx<-which(dat2_req_final[,"rev_Mean"]>1500)
dat2_req_final[indx,"rev_Mean"]<- 1427.27 #before imputing 99.9 percentile is 1427.27

summary(dat2_req_final$rev_Mean)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$drop_dat_Mean)
bx
quantile(dat2_req_final$drop_dat_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$drop_dat_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$drop_dat_Mean)

indx<-which(dat2_req_final[,"drop_dat_Mean"]>50)
dat2_req_final[indx,"drop_dat_Mean"]<- 5.49 #before imputing 99.9 percentile is 5.49

summary(dat2_req_final$drop_dat_Mean)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$drop_vce_Mean)
bx
quantile(dat2_req_final$drop_vce_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$drop_vce_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$drop_vce_Mean)

indx<-which(dat2_req_final[,"drop_vce_Mean"]>100)
dat2_req_final[indx,"drop_vce_Mean"]<- 79.32 #before imputing 99.9 percentile is 79.32

summary(dat2_req_final$drop_vce_Mean)



names(dat2_req_final)
bx<-boxplot(dat2_req_final$mou_pead_Mean)
bx
quantile(dat2_req_final$mou_pead_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$mou_pead_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$mou_pead_Mean)

indx<-which(dat2_req_final[,"mou_pead_Mean"]>200)
dat2_req_final[indx,"mou_pead_Mean"]<- 122.95 #before imputing 99.9 percentile is 122.95

summary(dat2_req_final$mou_pead_Mean)



names(dat2_req_final)
bx<-boxplot(dat2_req_final$opk_dat_Mean)
bx
quantile(dat2_req_final$opk_dat_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$opk_dat_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$opk_dat_Mean)

indx<-which(dat2_req_final[,"opk_dat_Mean"]>70)
dat2_req_final[indx,"opk_dat_Mean"]<- 58.82 #before imputing 99.9 percentile is 58.82

summary(dat2_req_final$opk_dat_Mean)



names(dat2_req_final)
bx<-boxplot(dat2_req_final$datovr_Mean)
bx
quantile(dat2_req_final$datovr_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$datovr_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$datovr_Mean)

indx<-which(dat2_req_final[,"datovr_Mean"]>30)
dat2_req_final[indx,"datovr_Mean"]<- 24.46 #before imputing 99.9 percentile is 24.46

summary(dat2_req_final$datovr_Mean)



names(dat2_req_final)
bx<-boxplot(dat2_req_final$ovrmou_Mean)
bx
quantile(dat2_req_final$ovrmou_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$ovrmou_Mean,p=c(9990:10000)/10000, na.rm=TRUE)
bx$out
hist(dat2_req_final$ovrmou_Mean)

indx<-which(dat2_req_final[,"ovrmou_Mean"]>1000)
dat2_req_final[indx,"ovrmou_Mean"]<- 958.15 #before imputing 99.9 percentile is 958.15

summary(dat2_req_final$ovrmou_Mean)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$blck_dat_Mean)
bx
quantile(dat2_req_final$blck_dat_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$blck_dat_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$blck_dat_Mean)

indx<-which(dat2_req_final[,"blck_dat_Mean"]>20)
dat2_req_final[indx,"blck_dat_Mean"]<- 15.58 #before imputing 99.9 percentile is 15.58

summary(dat2_req_final$blck_dat_Mean)


#classification: method will be class
#Regression: method will be anova
set.seed(123)
indx<-sample(nrow(dat2_req_final),0.7*nrow(dat2_req_final))
train<-dat2_req_final[indx,]
test<-dat2_req_final[-indx,]


dat2_req_final%>%count(churn,levels=crclscod)%>%filter(churn==1)->datC1
datC1$N<-unclass(dat2_req_final%>%filter(crclscod%in%datC1$levels)%>%count(crclscod))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N 
datC1$Var.Name<-rep("crclscod",nrow(datC1))
datC1
datC1_DF<-as.data.frame(datC1)

hist(datC1_DF$ChurnPerc)

#0-0.2(level 1), 0.2-0.4(level 2), 0.4 (level 3) above: total 3 groups for crclscod

#C2   CY    D  D5   E   E4  EA    GA   I   U    U1   W   Z4    ZY

unique(dat2_req_final$crclscod)

j<-1
for (i in dat2_req_final$crclscod){
  if (i=="C2" || i=="CY" || i=="D" || i=="D5" || i=="E" || i=="E4" || i=="EA" || i=="GA" || i=="I" || i == "U" || i=="U1" || i=="W" || i=="Z4" || i=="ZY"){
    dat2_req_final[j,"crclscod_Level"]<-"I"
  } else if(i=="A3" || i=="B2" || i=="EM" || i=="J" || i=="TP" ){
    dat2_req_final[j,"crclscod_Level"]<-"III"
  } else {
    dat2_req_final[j,"crclscod_Level"]<-"II"
  }
  j=j+1
}
unique(dat2_req_final$crclscod_Level)

dat2_req_final%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC1
datC1$N<-unclass(dat2_req_final%>%filter(ethnic%in%datC1$levels)%>%count(ethnic))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N 
datC1$Var.Name<-rep("ethnic",nrow(datC1))
datC1
datC1_DF<-as.data.frame(datC1)

hist(datC1_DF$ChurnPerc)


#X  P   Z   M   C   Level 1
#G R D O Level 3
j<-1
for (i in dat2_req_final$ethnic){
  #print(i)
  if (i=="X" || i=="P" || i=="Z" || i=="M" || i=="C"){
    dat2_req_final[j,"ethnic_Level"]<-"I"
  } else if(i=="G" || i=="R" || i=="D" || i=="O"){
    dat2_req_final[j,"ethnic_Level"]<-"III"
  } else {
    dat2_req_final[j,"ethnic_Level"]<-"II"
  }
  j=j+1
}
unique(dat2_req_final$ethnic_Level)


dat2_req_final%>%count(churn,levels=hnd_price)%>%filter(churn==1)->datC1
datC1$N<-unclass(dat2_req_final%>%filter(hnd_price%in%datC1$levels)%>%count(hnd_price))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N 
datC1$Var.Name<-rep("hnd_price",nrow(datC1))
datC1
datC1_DF<-as.data.frame(datC1)

hist(datC1_DF$ChurnPerc)

#249.9899902     499.9899902      199.9899902       399.9899902      179.9899902      Level 1
#9.989997864      39.98999023      29.98999023       239.9899902     Level 3
j<-1
for (i in dat2_req_final$hnd_price){
  #print(i)
  if (i=="249.9899902" || i=="499.9899902" || i=="199.9899902" || i=="399.9899902" || i=="179.9899902"){
    dat2_req_final[j,"hnd_price_Level"]<-"I"
  } else if(i=="9.989997864" || i=="39.98999023" || i=="29.98999023" || i=="239.9899902"){
    dat2_req_final[j,"hnd_price_Level"]<-"III"
  } else {
    dat2_req_final[j,"hnd_price_Level"]<-"II"
  }
  j=j+1
}
unique(dat2_req_final$hnd_price_Level)

j<-1
for (i in dat2_req_final$models){
  if (i=="1" || i=="2" || i =="3") {
    dat2_req_final[j,"models_Level"]<-"I"
  } else if(i=="4" || i=="5" || i=="6") {
    dat2_req_final[j,"models_Level"]<-"II"
  } else {
    dat2_req_final[j,"models_Level"]<-"III"
  }
  j=j+1
}
unique(dat2_req_final$models_Level)

unique(dat2_req_final$uniqsubs)
j<-1
for (i in dat2_req_final$uniqsubs){
  if (i=="1" || i=="2" || i =="3" || i=="4" || i=="5") {
    dat2_req_final[j,"uniqsubs_Level"]<-"I"
  } else {
    dat2_req_final[j,"uniqsubs_Level"]<-"II"
  }
  j=j+1
}
unique(dat2_req_final$uniqsubs_Level)

unique(dat2_req_final$actvsubs)
j<-1
for (i in dat2_req_final$actvsubs){
  if (i=="0" || i=="1" || i =="2" || i=="3" ) {
    dat2_req_final[j,"actvsubs_Level"]<-"I"
  } else {
    dat2_req_final[j,"actvsubs_Level"]<-"II"
  }
  j=j+1
}
unique(dat2_req_final$actvsubs_Level)


names(dat2_req_final)
dat2_req_final<-dat2_req_final[,-c(29,34,38,37,40,39)] 
#here 29 is crclscod, 34 is ethnic, 38 is hnd_price, 37 is models, 40 is uniqsubs, 39 is actvsubs
names(dat2_req_final)

dat2_req_final$crclscod_Level<-as.factor(dat2_req_final$crclscod_Level)
dat2_req_final$ethnic_Level<-as.factor(dat2_req_final$ethnic_Level)
dat2_req_final$hnd_price_Level<-as.factor(dat2_req_final$hnd_price_Level)
dat2_req_final$models_Level<-as.factor(dat2_req_final$models_Level)
dat2_req_final$uniqsubs_Level<-as.factor(dat2_req_final$uniqsubs_Level)
dat2_req_final$actvsubs_Level<-as.factor(dat2_req_final$actvsubs_Level)

names(dat2_req_final)

library(caret)
dummy <- dummyVars("~.",data = dat2_req_final,fullRank = T)
dat2_req_final_dummy <- data.frame(predict(dummy,newdata=dat2_req_final))
names(dat2_req_final_dummy)

set.seed(123)
indx<-sort(sample(nrow(dat2_req_final_dummy),nrow(dat2_req_final_dummy)*0.7))
train<-dat2_req_final_dummy[indx,]
test<-dat2_req_final_dummy[-indx,]
str(train)

#Q1
#churn name changed by dummyVars function to churn.1
trans_sigmodel<-glm(formula = churn.1 ~ mou_Mean + totmrc_Mean + mou_Range + 
                       drop_vce_Range + owylis_vce_Range + mou_opkv_Range + months + 
                       totcalls + eqpdays + rev_Mean + avg6mou + avg6qty + totrev + 
                       asl_flag.Y + prizm_social_one.S + prizm_social_one.U + area.CALIFORNIA.NORTH.AREA + 
                       area.CHICAGO.AREA + area.DALLAS.AREA + area.DC.MARYLAND.VIRGINIA.AREA + 
                       area.MIDWEST.AREA + area.NEW.YORK.CITY.AREA + area.NORTH.FLORIDA.AREA + 
                       area.NORTHWEST.ROCKY.MOUNTAIN.AREA + area.PHILADELPHIA.AREA + 
                       area.SOUTH.FLORIDA.AREA + area.TENNESSEE.AREA + marital.M + 
                       marital.S + models_Level.II + models_Level.III + uniqsubs_Level.II +
                       crclscod_Level.II + crclscod_Level.III + ethnic_Level.II + ethnic_Level.III + 
                       hnd_price_Level.II + hnd_price_Level.III + actvsubs_Level.II, family = binomial, 
                       data = train)

summary(trans_sigmodel)
names(trans_sigmodel)
class(trans_sigmodel)
#code for first problem
sort(trans_sigmodel$coefficients,decreasing = TRUE)


library (car)
vif(trans_sigmodel)

library(ROCR)

pred<-predict(trans_sigmodel,newdata=train,type="response")
predf<-ifelse(pred>0.6,1,0)
t=table(predf,train$churn.1) #for confusion matrix of train result
t
sum(diag(t))/nrow(test)
pred0<-prediction(pred,train$churn.1)
roc=performance(pred0,"tpr","fpr")
plot(roc)
abline(0,1)
auctest<-performance(pred0,"auc") 
auctest=unlist(slot(auctest,"y.values"))
auctest


pred<-predict(trans_sigmodel,newdata=test,type="response")
predf<-ifelse(pred>0.6,1,0)
t=table(predf,test$churn.1) #for confusion matrix of test result
t
sum(diag(t))/nrow(test)

pred0<-prediction(pred,test$churn.1)
roc=performance(pred0,"tpr","fpr")
plot(roc)
abline(0,1)
auctest<-performance(pred0,"auc") 
auctest=unlist(slot(auctest,"y.values"))
auctest


#for creating targeted customers
library(dplyr)
test%>%mutate(dec=ntile(totrev,n=10))%>%count(churn.1,dec)%>%filter(churn.1==1)->dat45
dat45$N<-unclass(test%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat45$churn_perc<-dat45$n/dat45$N
dat45$GreaterThan<-unclass(test%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat45$LessThan<-unclass(test%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat45

test2<-test
names(test2)

j<-1
for (i in test2$totrev){
  #print(i)
  if (i>=0 && i<590){
    test2[j,"totrevBuckets"]<-"low"
  } else if(i>=590 && i<1190){
    test2[j,"totrevBuckets"]<-"medium"
  } else {
    test2[j,"totrevBuckets"]<-"high"
  }
  j=j+1
}
str(test2$totrev)
str(test2$totrevBuckets)



j<-1
for (i in pred){
  #print(i)
  if (i>=0 && i<0.2){
    test2[j,"churnpercent"]<-"low"
  } else if(i>=0.2 && i<0.4){
    test2[j,"churnpercent"]<-"medium"
  } else {
    test2[j,"churnpercent"]<-"high"
  }
  j=j+1
}
names(test2)

table(test2$totrevBuckets,test2$churnpercent)
table(test2$churnpercent)

j<-1
for (i in c(1:nrow(test2))){
  #print(i)
  if (test2[i,"churnpercent"]=="high" && test2[i,"totrevBuckets"]=="high"){
    test1[j,"TargetCust"]<-1
  } else if(test2[i,"churnpercent"]=="high" && test2[i,"totrevBuckets"]=="medium"){
    test2[j,"TargetCust"]<-1
  } else if(test2[i,"churnpercent"]=="medium" && test2[i,"totrevBuckets"]=="high"){
    test2[j,"TargetCust"]<-1
  } else {
    test2[j,"TargetCust"]<-0
  }
  j=j+1
}
test2$TargetCust
table(test2$TargetCust) #748

#59+640+49. Getting the same output

names(test2)
test2$Customer_ID <- test$Customer_ID
Targeted_Customers <- test2%>%filter(TargetCust==1)%>%select(Customer_ID,TargetCust)
Targeted_Customers

#Q2 & Q3
names(dat2)
index <- which(names(dat2) %in% c( "drop_blk_Mean", "mou_Range", "change_mou", "iwylis_vce_Mean", "roam_Mean", "drop_vce_Mean", "avgmou", "callwait_Mean", "mou_Mean", "churn", "totmrc_Mean", "ovrrev_Mean", "rev_Range"))
dat2_ <- dat2[,index]
names(dat2_)
colSums(is.na(dat2_))
dat2_
indx<-which(is.na(dat2_$mou_Mean))
dat2_<-dat2_[-indx,]
indx<-which(is.na(dat2_$change_mou))
dat2_<-dat2_[-indx,]
colSums(is.na(dat2_))
dat2_

set.seed(1234)
indx<-sort(sample(nrow(dat2_),nrow(dat2_)*0.7))
train1<-dat2_[indx,]
test1<-dat2_[-indx,]

trans_sigmodel2<-glm(formula = churn ~drop_blk_Mean + totmrc_Mean + mou_Range + change_mou + iwylis_vce_Mean + roam_Mean + drop_vce_Mean + avgmou + callwait_Mean + mou_Mean + ovrrev_Mean + rev_Range, family = binomial,data = train1)
summary(trans_sigmodel2)

vif(trans_sigmodel2)

pred<-predict(trans_sigmodel2,newdata=test1,type="response")
predf<-ifelse(pred>0.6,1,0)
t=table(predf,test1$churn) #for confusion matrix of test result
t
sum(diag(t))/nrow(test1)

pred0<-prediction(pred,test1$churn)
roc=performance(pred0,"tpr","fpr")
plot(roc)
abline(0,1)
auctest<-performance(pred0,"auc") 
auctest=unlist(slot(auctest,"y.values"))
auctest

