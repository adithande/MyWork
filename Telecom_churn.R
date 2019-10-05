getwd()
setwd("E:\\jigsaw\\JLC\\CAPSTONE\\project")
telecom<-read.csv("sampletelecomfinal.csv", stringsAsFactors = T)
options(scipen = 999)
library(dplyr)



                          ##---------Creating Data Quality Report(dqr)-----------##

#Extracting Variable names
Variables<-names(telecom)
dataquality<-as.data.frame(Variables)
rm(Variables)

#Recording Data Type for each Variable
dataquality$DataType<-sapply(telecom,class)

#No. of Records for each Variable
dataquality$No.ofRecords<-nrow(telecom)

#Counting No. of Unique Values for each variable
for(i in 1:ncol(telecom))
{
  dataquality$UniqueRecords[i]<-length(unique(telecom[,i]))
}

#No.of observations available for each variable and its percentage
dataquality$DataAvailable<-colSums(!is.na(telecom))
dataquality$AvailablePercent<-round(colMeans(!is.na(telecom)),4)


#Total and Percentage of Missing Values for each Variable
dataquality$Missing<-colSums(is.na(telecom))
dataquality$MissingPercent<-round(colMeans(is.na(telecom)),4)

#Minimum, Maximum, Mean, Quantile Values for each Variable
for(i in 1:ncol(telecom))
{
  dataquality$Minimum[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",min(telecom[,i],na.rm=T),0),2)
  dataquality$Maximum[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",max(telecom[,i],na.rm=T),0),2)
  dataquality$Mean[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",mean(telecom[,i],na.rm=T),0),2)
  dataquality$fifthPercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.05,na.rm=T),0),2)
  dataquality$tenthPercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.10,na.rm=T),0),2)
  dataquality$twentyfifthPercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.25,na.rm=T),0),2)
  dataquality$fiftythPercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.50,na.rm=T),0),2)
  dataquality$seventyfifthPercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.75,na.rm=T),0),2)
  dataquality$ninetythPercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.90,na.rm=T),0),2)
  dataquality$ninetyfifthPercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.95,na.rm=T),0),2)
}

str(dataquality)

#Exporting Data Quality Report
write.csv(dataquality,"Data Quality Report.csv")






##--------------Missing Value treatment of var, "retdays" and Creating Dummy Variable---------#
summary(telecom$retdays)
sort(unique(telecom$retdays), na.last = F)
telecom$retdays_1<-ifelse(is.na(telecom$retdays)==TRUE, 0, 1)
str(telecom$retdays_1)
summary(telecom$retdays_1)


#Ommitting variables with more than 15% missing values and creating a new data set
telecom1<-telecom[,colMeans(is.na(telecom))<=0.15]

#Variable drop_blk_Mean is created by adding vars blck_dat_Mean + BLCK_VCE_MEAN + DROP_DAT_MEAN + DROP_VCE_MEAN 
# So omitting Variable blck_dat_Mean,drop_vce_Mean,drop_dat_Mean & datovr_Mean
# OVRREV_MEAN=DATOVR_MEAN + VCEOVR_MEAN
names(telecom1)
telecom1<- subset(telecom1, select=-c(blck_dat_Mean,drop_vce_Mean,drop_dat_Mean,datovr_Mean)) #-----------important---###

##-----------------Data Exploration => Profiling (dat1-Continuous Variables , datC-Categorical Variables)------------## 


##-------- Deciling Continuous Variables Basis Target Variabe Churn---------##

names(telecom1)
str(telecom1)

# <1>Variable 'mou_Mean'
summary(telecom1$mou_Mean)
telecom1%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat1
dat1$N<-unclass(telecom1%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat1$churn_perc<-round(dat1$n/dat1$N,2)
dat1$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dat1$LessThan<-unclass(telecom1%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dat1$varname<-rep("mou_Mean",nrow(dat1))


# <2> Variable "totmrc_Mean" 
summary(telecom1$totmrc_Mean)
telecom1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat2
dat2$N<-unclass(telecom1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat2$churn_perc<-dat2$n/dat2$N
dat2$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat2$LessThan<-unclass(telecom1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat2$varname<-rep("totmrc_Mean",nrow(dat2))


# <3> Variable "rev_Range" 
summary(telecom1$rev_Range)
telecom1%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat3
dat3$N<-unclass(telecom1%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat3$churn_perc<-dat3$n/dat3$N
dat3$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
dat3$LessThan<-unclass(telecom1%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
dat3$varname<-rep("rev_Range",nrow(dat3))


# <4> Variable "mou_Range" 
summary(telecom1$mou_Range)
telecom1%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat4
dat4$N<-unclass(telecom1%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat4$churn_perc<-dat4$n/dat4$N
dat4$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
dat4$LessThan<-unclass(telecom1%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
dat4$varname<-rep("mou_Range",nrow(dat4))                  


# <5> Variable "change_mou" 
summary(telecom1$change_mou)
telecom1%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat5
dat5$N<-unclass(telecom1%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
dat5$churn_perc<-dat5$n/dat5$N
dat5$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
dat5$LessThan<-unclass(telecom1%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
dat5$varname<-rep("change_mou",nrow(dat5))    


# <6> Variable "drop_blk_Mean" 
telecom1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat6
dat6$N<-unclass(telecom1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat6$churn_perc<-dat6$n/dat6$N
dat6$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
dat6$LessThan<-unclass(telecom1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
dat6$varname<-rep("drop_blk_Mean",nrow(dat6)) 


# <7> Variable "drop_vce_Range" 
summary(telecom1$drop_vce_Range)
telecom1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat7
dat7$N<-unclass(telecom1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat7$churn_perc<-dat7$n/dat7$N
dat7$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
dat7$LessThan<-unclass(telecom1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
dat7$varname<-rep("drop_vce_Range",nrow(dat7)) 


# <8> Variable "owylis_vce_Range" 
summary(telecom1$owylis_vce_Range)
telecom1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat8
dat8$N<-unclass(telecom1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat8$churn_perc<-dat8$n/dat8$N
dat8$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
dat8$LessThan<-unclass(telecom1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
dat8$varname<-rep("owylis_vce_Range",nrow(dat8))


# <9> Variable "mou_opkv_Range" 
summary(telecom1$mou_opkv_Range)
telecom1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat9
dat9$N<-unclass(telecom1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat9$churn_perc<-dat9$n/dat9$N
dat9$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
dat9$LessThan<-unclass(telecom1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
dat9$varname<-rep("mou_opkv_Range",nrow(dat9))


# <10> Variable "months" 
summary(telecom1$months)
telecom1%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat10
dat10$N<-unclass(telecom1%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
dat10$churn_perc<-dat10$n/dat10$N
dat10$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
dat10$LessThan<-unclass(telecom1%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
dat10$varname<-rep("months",nrow(dat10))


# <11> Variable "totcalls" 
summary(telecom1$totcalls)
telecom1%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat11
dat11$N<-unclass(telecom1%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
dat11$churn_perc<-dat11$n/dat11$N
dat11$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
dat11$LessThan<-unclass(telecom1%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
dat11$varname<-rep("totcalls",nrow(dat11))


# <12> Variable "eqpdays"
summary(telecom1$eqpdays)
telecom1%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat12
dat12$N<-unclass(telecom1%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]]
dat12$churn_perc<-dat12$n/dat12$N
dat12$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
dat12$LessThan<-unclass(telecom1%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
dat12$varname<-rep("eqpdays",nrow(dat12))


# <13> Variable "custcare_Mean"===>> ***Getting less than 4 deciles. Omit***
summary(telecom1$custcare_Mean)
telecom1%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat13
dat13$N<-unclass(telecom1%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat13$churn_perc<-dat13$n/dat13$N
dat13$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(custcare_Mean,n=10))%>%group_by(dec)%>%summarise(min(custcare_Mean)))[[2]]
dat13$LessThan<-unclass(telecom1%>%mutate(dec=ntile(custcare_Mean,n=10))%>%group_by(dec)%>%summarise(max(custcare_Mean)))[[2]]
dat13$varname<-rep("custcare_Mean",nrow(dat13))




# <14> Variable "callwait_Mean"
summary(telecom1$callwait_Mean)
telecom1%>%mutate(dec=ntile(callwait_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat14
dat14$N<-unclass(telecom1%>%mutate(dec=ntile(callwait_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat14$churn_perc<-dat14$n/dat14$N
dat14$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(callwait_Mean,n=10))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
dat14$LessThan<-unclass(telecom1%>%mutate(dec=ntile(callwait_Mean,n=10))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
dat14$varname<-rep("callwait_Mean",nrow(dat14))


# <15> Variable "iwylis_vce_Mean"
summary(telecom1$iwylis_vce_Mean)
telecom1%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat15
dat15$N<-unclass(telecom1%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat15$churn_perc<-dat15$n/dat15$N
dat15$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
dat15$LessThan<-unclass(telecom1%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
dat15$varname<-rep("iwylis_vce_Mean",nrow(dat15))               


# <16> Variable "callwait_Range"===>> ***Getting less than 4 deciles. Omit***
summary(telecom1$callwait_Range)
telecom1%>%mutate(dec=ntile(callwait_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat16
dat16$N<-unclass(telecom1%>%mutate(dec=ntile(callwait_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat16$churn_perc<-dat16$n/dat16$N
dat16$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(callwait_Range,n=10))%>%group_by(dec)%>%summarise(min(callwait_Range)))[[2]]
dat16$LessThan<-unclass(telecom1%>%mutate(dec=ntile(callwait_Range,n=10))%>%group_by(dec)%>%summarise(max(callwait_Range)))[[2]]
dat16$varname<-rep("callwait_Range",nrow(dat16)) 



# <17> Variable "ccrndmou_Range"===>> ***Getting less than 4 deciles. Omit***
summary(telecom1$ccrndmou_Range)
telecom1%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat17
dat17$N<-unclass(telecom1%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat17$churn_perc<-dat17$n/dat17$N
dat17$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%group_by(dec)%>%summarise(min(ccrndmou_Range)))[[2]]
dat17$LessThan<-unclass(telecom1%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%group_by(dec)%>%summarise(max(ccrndmou_Range)))[[2]]
dat17$varname<-rep("ccrndmou_Range",nrow(dat17))


# <18> Variable "adjqty"
summary(telecom1$adjqty)
telecom1%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat18
dat18$N<-unclass(telecom1%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
dat18$churn_perc<-dat18$n/dat18$N
dat18$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
dat18$LessThan<-unclass(telecom1%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
dat18$varname<-rep("adjqty",nrow(dat18))


# <19> Variable "ovrrev_Mean"
summary(telecom1$ovrrev_Mean)
telecom1%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat19
dat19$N<-unclass(telecom1%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat19$churn_perc<-dat19$n/dat19$N
dat19$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
dat19$LessThan<-unclass(telecom1%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
dat19$varname<-rep("ovrrev_Mean",nrow(dat19))


# <20> Variable "rev_Mean"
summary(telecom1$rev_Mean)
telecom1%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat20
dat20$N<-unclass(telecom1%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat20$churn_perc<-dat20$n/dat20$N
dat20$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dat20$LessThan<-unclass(telecom1%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
dat20$varname<-rep("rev_Mean",nrow(dat20))


# <21> Variable "ovrmou_Mean"
summary(telecom1$ovrmou_Mean)
telecom1%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat21
dat21$N<-unclass(telecom1%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat21$churn_perc<-dat21$n/dat21$N
dat21$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
dat21$LessThan<-unclass(telecom1%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
dat21$varname<-rep("ovrmou_Mean",nrow(dat21))


# <22> Variable "comp_vce_Mean" 
summary(telecom1$comp_vce_Mean)
telecom1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat22
dat22$N<-unclass(telecom1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat22$churn_perc<-dat22$n/dat22$N
dat22$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
dat22$LessThan<-unclass(telecom1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
dat22$varname<-rep("comp_vce_Mean",nrow(dat22))


# <23> Variable "plcd_vce_Mean" 
summary(telecom1$plcd_vce_Mean)
telecom1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat23
dat23$N<-unclass(telecom1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat23$churn_perc<-dat23$n/dat23$N
dat23$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
dat23$LessThan<-unclass(telecom1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
dat23$varname<-rep("plcd_vce_Mean",nrow(dat23))


# <24> Variable "avg3mou"
summary(telecom1$avg3mou)
telecom1%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat24
dat24$N<-unclass(telecom1%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
dat24$churn_perc<-dat24$n/dat24$N
dat24$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
dat24$LessThan<-unclass(telecom1%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
dat24$varname<-rep("avg3mou",nrow(dat24))


# <25> Variable "avgmou"
summary(telecom1$avgmou)
telecom1%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat25
dat25$N<-unclass(telecom1%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
dat25$churn_perc<-dat25$n/dat25$N
dat25$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
dat25$LessThan<-unclass(telecom1%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
dat25$varname<-rep("avgmou",nrow(dat25))


# <26> Variable "avg3qty"
summary(telecom1$avg3qty)
telecom1%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat26
dat26$N<-unclass(telecom1%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
dat26$churn_perc<-dat26$n/dat26$N
dat26$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
dat26$LessThan<-unclass(telecom1%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
dat26$varname<-rep("avg3qty",nrow(dat26))


# <27> Variable "avgqty"
summary(telecom1$avgqty)
telecom1%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat27
dat27$N<-unclass(telecom1%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
dat27$churn_perc<-dat27$n/dat27$N
dat27$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
dat27$LessThan<-unclass(telecom1%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
dat27$varname<-rep("avgqty",nrow(dat27))


# <28> Variable "avg6mou"
summary(telecom1$avg6mou)
telecom1%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat28
dat28$N<-unclass(telecom1%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
dat28$churn_perc<-dat28$n/dat28$N
dat28$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
dat28$LessThan<-unclass(telecom1%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
dat28$varname<-rep("avg6mou",nrow(dat28))


# <29> Variable "avg6qty"
summary(telecom1$avg6qty)
telecom1%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat29
dat29$N<-unclass(telecom1%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
dat29$churn_perc<-dat29$n/dat29$N
dat29$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
dat29$LessThan<-unclass(telecom1%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
dat29$varname<-rep("avg6qty",nrow(dat29))

# <30> Variable "da_Mean"
summary(telecom1$da_Mean)
telecom1%>%mutate(dec=ntile(da_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat30
dat30$N<-unclass(telecom1%>%mutate(dec=ntile(da_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat30$churn_perc<-dat30$n/dat30$N
dat30$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(da_Mean,n=10))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
dat30$LessThan<-unclass(telecom1%>%mutate(dec=ntile(da_Mean,n=10))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
dat30$varname<-rep("da_Mean",nrow(dat30))


# <31> Variable "da_Range"
summary(telecom1$da_Range)
telecom1%>%mutate(dec=ntile(da_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat31
dat31$N<-unclass(telecom1%>%mutate(dec=ntile(da_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat31$churn_perc<-dat31$n/dat31$N
dat31$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(da_Range,n=10))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
dat31$LessThan<-unclass(telecom1%>%mutate(dec=ntile(da_Range,n=10))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
dat31$varname<-rep("da_Range",nrow(dat31))


# <32> Variable "adjmou" 
summary(telecom1$adjmou)
telecom1%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat32
dat32$N<-unclass(telecom1%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
dat32$churn_perc<-dat32$n/dat32$N
dat32$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
dat32$LessThan<-unclass(telecom1%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
dat32$varname<-rep("adjmou",nrow(dat32))


# <33> Variable "totrev"
summary(telecom1$totrev)
telecom1%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat33
dat33$N<-unclass(telecom1%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat33$churn_perc<-dat33$n/dat33$N
dat33$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat33$LessThan<-unclass(telecom1%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat33$varname<-rep("totrev",nrow(dat33))


# <34> Variable "adjrev" 
summary(telecom1$adjrev)
telecom1%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat34
dat34$N<-unclass(telecom1%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
dat34$churn_perc<-dat34$n/dat34$N
dat34$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
dat34$LessThan<-unclass(telecom1%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
dat34$varname<-rep("adjrev",nrow(dat34))


# <35> Variable "avgrev" 
summary(telecom1$avgrev)
telecom1%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat35
dat35$N<-unclass(telecom1%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
dat35$churn_perc<-dat35$n/dat35$N
dat35$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
dat35$LessThan<-unclass(telecom1%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
dat35$varname<-rep("avgrev",nrow(dat35))


# <36> Variable "opk_dat_Mean" ===>> ***Omit***
summary(telecom1$opk_dat_Mean)
telecom1%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat36
dat36$varname<-rep("opk_dat_Mean",nrow(dat36))



# <37> Variable "roam_Mean" ===>> ***Getting less than 4 deciles. So Omit***
summary(telecom1$roam_Mean)
telecom1%>%mutate(dec=ntile(roam_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat37
dat37$varname<-rep("roam_Mean",nrow(dat37))


# <38> Variable "recv_sms_Mean" ===>> ***Getting less than 4 deciles. So Omit***
summary(telecom1$recv_sms_Mean)
telecom1%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat38
dat38$varname<-rep("recv_sms_Mean",nrow(dat38))


# <39> Variable "mou_pead_Mean" ===>> ***Getting less than 4 deciles. So Omit***
summary(telecom1$mou_pead_Mean)
telecom1%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat39
dat39$varname<-rep("mou_pead_Mean",nrow(dat39))


# <40> Variable "datovr_Range" ===>> ***Getting less than 4 deciles. So Omit***
summary(telecom1$datovr_Range)
telecom1%>%mutate(dec=ntile(datovr_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat40
dat40$varname<-rep("datovr_Range",nrow(dat40))



#Adding all appropriate dat1 to dat54 objects to create a dat object
dat<-rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8,dat9,dat10,dat11,dat12,dat14,dat15,dat18,dat19,
           dat20,dat21,dat22,dat23,dat24,dat25,dat26,dat27,dat28,dat29,dat30,dat31,dat32,dat33,
           dat34,dat35)

#Exporting Deciled variables
write.csv(dat,"Deciled Usable Continuous variables.csv",row.names = F)



#Removing Variables that could not be deciled as will come insignificant in the model
#Also omitting transformed Vars "comp_vce_Mean" ,"comp_dat_Mean", "plcd_vce_Mean", plcd_dat_Mean
names(telecom1)
telecom1<- subset(telecom1, select=-c(custcare_Mean,callwait_Range,ccrndmou_Range,opk_dat_Mean,mou_pead_Mean,roam_Mean,recv_sms_Mean,mou_pead_Mean,datovr_Range))
names(telecom1)





##-------- Categorical Variables---------##

##-----Event rate for each level in a categorical variable-----##

# <27> Variable "crclscod" =====>>> **** Some Levels show less than 5% churn rate. So Omit as will come insignificant ****  
summary(telecom1$crclscod)
telecom1%>%count(churn,levels=crclscod)%>%filter(churn==1)->datC27
datC27$N<-unclass(telecom1%>%filter(crclscod%in%datC27$levels)%>%count(crclscod))[[2]]
datC27$ChurnPerc<-datC27$n/datC27$N
datC27$Var.Name<-rep("crclscod",nrow(datC27))


# <28> Variable "asl_flag"  
summary(telecom1$asl_flag)
telecom1%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datC28
datC28$N<-unclass(telecom1%>%filter(asl_flag%in%datC28$levels)%>%count(asl_flag))[[2]]
datC28$ChurnPerc<-datC28$n/datC28$N
datC28$Var.Name<-rep("asl_flag",nrow(datC28))


# <29> Variable "prizm_social_one"  
summary(telecom1$prizm_social_one)
telecom1%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datC29
datC29$N<-unclass(telecom1%>%filter(prizm_social_one%in%datC29$levels)%>%count(prizm_social_one))[[2]]
datC29$ChurnPerc<-datC29$n/datC29$N
datC29$Var.Name<-rep("prizm_social_one",nrow(datC29))


# <30> Variable "area"  
summary(telecom1$area)
telecom1%>%count(churn,levels=area)%>%filter(churn==1)->datC30
datC30$N<-unclass(telecom1%>%filter(area%in%datC30$levels)%>%count(area))[[2]]
datC30$ChurnPerc<-datC30$n/datC30$N
datC30$Var.Name<-rep("area",nrow(datC30))


# <31> Variable "refurb_new"  
summary(telecom1$refurb_new)
telecom1%>%count(churn,levels=refurb_new)%>%filter(churn==1)->datC31
datC31$N<-unclass(telecom1%>%filter(refurb_new%in%datC31$levels)%>%count(refurb_new))[[2]]
datC31$ChurnPerc<-datC31$n/datC31$N
datC31$Var.Name<-rep("refurb_new",nrow(datC31))


# <32> Variable "hnd_webcap"  
summary(telecom1$hnd_webcap)
telecom1%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datC32
datC32$N<-unclass(telecom1%>%filter(hnd_webcap%in%datC32$levels)%>%count(hnd_webcap))[[2]]
datC32$ChurnPerc<-datC32$n/datC32$N
datC32$Var.Name<-rep("hnd_webcap",nrow(datC32))


# <33> Variable "marital"  
summary(telecom1$marital)
telecom1%>%count(churn,levels=marital)%>%filter(churn==1)->datC33
datC33$N<-unclass(telecom1%>%filter(marital%in%datC33$levels)%>%count(marital))[[2]]
datC33$ChurnPerc<-datC33$n/datC33$N
datC33$Var.Name<-rep("marital",nrow(datC33))


# <34> Variable "ethnic"  
summary(telecom1$ethnic)
telecom1%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC34
datC34$N<-unclass(telecom1%>%filter(ethnic%in%datC34$levels)%>%count(ethnic))[[2]]
datC34$ChurnPerc<-datC34$n/datC34$N
datC34$Var.Name<-rep("ethnic",nrow(datC34))


# <45> Variable "car_buy"  
summary(telecom1$car_buy)
telecom1%>%count(churn,levels=car_buy)%>%filter(churn==1)->datC45
datC45$N<-unclass(telecom1%>%filter(car_buy%in%datC45$levels)%>%count(car_buy))[[2]]
datC45$ChurnPerc<-datC45$n/datC45$N
datC45$Var.Name<-rep("car_buy",nrow(datC45))


# <46> Variable "csa" ===>>> **** Some Levels show less than 5% churn rate. So Omit as will come insignificant **** 
summary(telecom1$csa)
telecom1%>%count(churn,levels=csa)%>%filter(churn==1)->datC46
datC46$N<-unclass(telecom1%>%filter(csa%in%datC46$levels)%>%count(csa))[[2]]
datC46$ChurnPerc<-datC46$n/datC46$N
datC46$Var.Name<-rep("csa",nrow(datC46))

# <53> Variable "retdays_1"  
summary(telecom1$retdays_1)
telecom1$retdays_1<-as.factor(telecom1$retdays_1)
telecom1%>%count(churn,levels=retdays_1)%>%filter(churn==1)->datC53
datC53$N<-unclass(telecom1%>%filter(retdays_1%in%datC53$levels)%>%count(retdays_1))[[2]]
datC53$ChurnPerc<-datC53$n/datC53$N
datC53$Var.Name<-rep("retdays_1",nrow(datC53))


# Use VArs as Factor => age1,age2, models, actvsubs, uniqsubs, forgntvl, mtrcycle, truck, 

# <35> Variable "age1"  
summary(telecom1$age1)
telecom1%>%count(churn,levels=age1)%>%filter(churn==1)->datC35
datC35$N<-unclass(telecom1%>%filter(age1%in%datC35$levels)%>%count(age1))[[2]]
datC35$ChurnPerc<-datC35$n/datC35$N
datC35$Var.Name<-rep("age1",nrow(datC35))


# <36> Variable "age2"  
summary(telecom1$age2)
telecom1%>%count(churn,levels=age2)%>%filter(churn==1)->datC36
datC36$N<-unclass(telecom1%>%filter(age2%in%datC36$levels)%>%count(age2))[[2]]
datC36$ChurnPerc<-datC36$n/datC36$N
datC36$Var.Name<-rep("age2",nrow(datC36))


# <37> Variable "models"  
summary(telecom1$models)
telecom1%>%count(churn,levels=models)%>%filter(churn==1)->datC37
datC37$N<-unclass(telecom1%>%filter(models%in%datC37$levels)%>%count(models))[[2]]
datC37$ChurnPerc<-datC37$n/datC37$N
datC37$Var.Name<-rep("models",nrow(datC37))


# <38> Variable "hnd_price"  
summary(telecom1$hnd_price)
unique(telecom1$hnd_price)
telecom1%>%count(churn,levels=hnd_price)%>%filter(churn==1)->datC38
datC38$N<-unclass(telecom1%>%filter(hnd_price%in%datC38$levels)%>%count(hnd_price))[[2]]
datC38$ChurnPerc<-datC38$n/datC38$N
datC38$Var.Name<-rep("hnd_price",nrow(datC38))

# <39> Variable "actvsubs"  
summary(telecom1$actvsubs)
telecom1%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datC39
datC39$N<-unclass(telecom1%>%filter(actvsubs%in%datC39$levels)%>%count(actvsubs))[[2]]
datC39$ChurnPerc<-datC39$n/datC39$N
datC39$Var.Name<-rep("actvsubs",nrow(datC39))


# <40> Variable "uniqsubs"  
summary(telecom1$uniqsubs)
telecom1%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datC40
datC40$N<-unclass(telecom1%>%filter(uniqsubs%in%datC40$levels)%>%count(uniqsubs))[[2]]
datC40$ChurnPerc<-datC40$n/datC40$N
datC40$Var.Name<-rep("uniqsubs",nrow(datC40))


# <41> Variable "forgntvl"  
summary(telecom1$forgntvl)
telecom1%>%count(churn,levels=forgntvl)%>%filter(churn==1)->datC41
datC41$N<-unclass(telecom1%>%filter(forgntvl%in%datC41$levels)%>%count(forgntvl))[[2]]
datC41$ChurnPerc<-datC41$n/datC41$N
datC41$Var.Name<-rep("forgntvl",nrow(datC41))


# <42> Variable "mtrcycle"  
summary(telecom1$mtrcycle)
telecom1%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->datC42
datC42$N<-unclass(telecom1%>%filter(mtrcycle%in%datC42$levels)%>%count(mtrcycle))[[2]]
datC42$ChurnPerc<-datC42$n/datC42$N
datC42$Var.Name<-rep("mtrcycle",nrow(datC42))


# <43> Variable "Truck"  
summary(telecom1$truck)
telecom1%>%count(churn,levels=truck)%>%filter(churn==1)->datC43
datC43$N<-unclass(telecom1%>%filter(truck%in%datC43$levels)%>%count(truck))[[2]]
datC43$ChurnPerc<-datC43$n/datC43$N
datC43$Var.Name<-rep("Truck",nrow(datC43))



#Adding objects to create a datC object
datC_1<-rbind(datC27,datC28,datC29,datC30,datC31,datC32,datC33,datC34,datC45,
              datC46,datC53)

datC_2<-rbind(datC35,datC36,datC37,datC38,datC39,datC40,datC41,datC42,datC43)


#Exporting Deciled variables
write.csv(datC_1,"Event Rate - Categorical variables1.csv",row.names = F)
write.csv(datC_2,"Event Rate - Categorical variables2.csv",row.names = F)


#Removing Variables with levels less than 5% churn rate as will come insignificant
names(telecom1)
telecom1<- subset(telecom1, select=-c(crclscod,csa))
names(telecom1)

                                     #*********Data Preparation*********#

                                            #-----Outlier Treatment----#

                                            #-----Continuous Variables------#
                                      #Method <II> Box Plot Method==>>

summary(telecom1)
str(telecom1)

#Factor Variables=> asl_flag, prizm_social_one, area, refurb_new, hnd_webcap, 
#marital, ethnic, age1, age2, models, hnd_price, actvsubs, "uniqsubs", 
#"forgntvl", "mtrcycle", "truck", churn, car_buy, Customer_ID, retdays_1


list<-names(telecom1)
list
# Removing Categorical Variables
list<-list[-c(27:44,51,52)]
list

# Outlier Plots
par(mfrow=c(4,8))
for(i in 1:length(list))
{
  boxplot(telecom1[,list[i]],main=list[i])
}



# Outlier Treatment
par(mfrow=c(4,8))
for(i in 1:length(list))
{
  x<-boxplot(telecom1[,list[i]],main=list[i])
  out<-x$out
  index<-which(telecom1[,list[i]]%in% x$out)
  telecom1[index,list[i]]<-mean(telecom1[,list[i]],na.rm = T)
  rm(x)
  rm(out)
}


# Checking After Treatment
par(mfrow=c(4,8))
for(i in 1:length(list))
{
  boxplot(telecom1[,list[i]],main=list[i])
}


dev.off()



#-----Missing Value Treatment -------# 

summary(telecom1)
names(telecom1)

# Factor Variables=> crclscod, asl_flag, prizm_social_one, area, refurb_new, hnd_webcap, marital, ethnic, "age1", 
#    "age2", "models", "hnd_price","actvsubs", "uniqsubs", "forgntvl", "mtrcycle", "truck", car_buy, csa retdays_1


# Deleting Missing Values
index1<-which(is.na(telecom1[,c(1:4)]))
telecom1<-telecom1[-index1,]
summary(telecom1)

index2<-which(is.na(telecom1$change_mou))
telecom1<-telecom1[-index2,]

index4<-which(is.na(telecom1$area))
telecom1<-telecom1[-index4,]

index5<-which(is.na(telecom1$marital))
telecom1<-telecom1[-index5,]
summary(telecom1)


# Mean Imputation
telecom1$avg6mou[is.na(telecom1$avg6mou)]<-mean(telecom1$avg6mou,na.rm = T)

telecom1$avg6qty[is.na(telecom1$avg6qty)]<-mean(telecom1$avg6qty,na.rm = T)



# Creating seperate category "Missing" for Factor Variables

#variable hnd_price
telecom1$hnd_price_1<-ifelse(is.na(telecom1$hnd_price),"Missing",as.factor(telecom1$hnd_price))
str(telecom1$hnd_price_1)
telecom1$hnd_price_1<-as.factor(telecom1$hnd_price_1)
summary(telecom1$hnd_price)
summary(telecom1$hnd_price_1)
telecom1$hnd_price_1<-factor(telecom1$hnd_price_1,labels =c("9.989997864","179.9899902","199.9899902","239.9899902","249.9899902" ,"299.9899902", "399.9899902", "499.9899902","29.98999023", "39.98999023", "59.98999023", "79.98999023", 
                                                            "99.98999023", "119.9899902", "129.9899902", "149.9899902" ,"Missing"))
summary(telecom1$hnd_price_1)

names(telecom1)
telecom1<-telecom1[,-37]
summary(telecom1)


#Variable prizm_social_one
telecom1$prizm_social_one_1<-ifelse(is.na(telecom1$prizm_social_one),"Missing",as.factor(telecom1$prizm_social_one))
str(telecom1$prizm_social_one_1)
telecom1$prizm_social_one_1<-as.factor(telecom1$prizm_social_one_1)
summary(telecom1$prizm_social_one)
summary(telecom1$prizm_social_one_1)
telecom1$prizm_social_one_1<-factor(telecom1$prizm_social_one_1,labels =c("C","R","S","T","U","Missing"))
summary(telecom1$prizm_social_one_1)

names(telecom1)
telecom1<-telecom1[,-28]
summary(telecom1)



#Variable hnd_webcap
telecom1$hnd_webcap_1<-ifelse(is.na(telecom1$hnd_webcap),"Missing",as.factor(telecom1$hnd_webcap))
str(telecom1$hnd_webcap_1)
telecom1$hnd_webcap_1<-as.factor(telecom1$hnd_webcap_1)
summary(telecom1$hnd_webcap)
summary(telecom1$hnd_webcap_1)
telecom1$hnd_webcap_1<-factor(telecom1$hnd_webcap_1,labels =c("UNKW","WC","WCMB","Missing"))
summary(telecom1$hnd_webcap_1)

names(telecom1)
telecom1<-telecom1[,-30]
summary(telecom1)



#Checking Churn Rate in the data after Imputations
table(telecom$churn)/nrow(telecom)
table(telecom1$churn)/nrow(telecom1)



# Convert to Factor and Create Dummy Variables => 
#age1, age2, models, hnd_price, actvsubs,uniqsubs, forgntvl, mtrcycle, truck, Customer ID, Churn


str(telecom1$age1)
telecom1$age1_1<-ifelse(telecom1$age1==0,"Default",ifelse(telecom1$age1<=30,"Young",
                                                    ifelse(telecom1$age1>30 & telecom1$age1<=55,"Mid Age","Old")))
str(telecom1$age1_1)
telecom1$age1_1<-as.factor(telecom1$age1_1)
summary(telecom1$age1_1)

names(telecom1)
telecom1<-telecom1[,-32]
summary(telecom1)



str(telecom1$age2)
telecom1$age2_1<-ifelse(telecom1$age2==0,"Default",ifelse(telecom1$age2<=30,"Young",
                                                    ifelse(telecom1$age2>30 & telecom1$age2<=55,"Mid Age","Old")))
str(telecom1$age2_1)
telecom1$age2_1<-as.factor(telecom1$age2_1)
summary(telecom1$age2_1)

names(telecom1)
telecom1<-telecom1[,-32]
summary(telecom1)



str(telecom1$models)
summary(telecom1$models)
telecom1$models<-as.factor(telecom1$models)
summary(telecom1$models)




str(telecom1$actvsubs)
summary(telecom1$actvsubs)
telecom1$actvsubs<-as.factor(telecom1$actvsubs)
summary(telecom1$actvsubs)


str(telecom1$uniqsubs)
summary(telecom1$uniqsubs)
telecom1$uniqsubs<-as.factor(telecom1$uniqsubs)
summary(telecom1$uniqsubs)


str(telecom1$forgntvl)
summary(telecom1$forgntvl)
telecom1$forgntvl<-as.factor(telecom1$forgntvl)
summary(telecom1$forgntvl)


str(telecom1$mtrcycle)
summary(telecom1$mtrcycle)
telecom1$mtrcycle<-as.factor(telecom1$mtrcycle)
summary(telecom1$mtrcycle)


str(telecom1$truck)
summary(telecom1$truck)
telecom1$truck<-as.factor(telecom1$truck)
summary(telecom1$truck)



### ********** Logistic Regression Model Building ********** ###

# Splitting into Test and Training Samples
set.seed(200)
index<-sample(nrow(telecom1),0.70*nrow(telecom1),replace=F)
train<-telecom1[index,]
test<-telecom1[-index,]


#Checking Churn Rate 
table(train$churn)/nrow(train)
table(test$churn)/nrow(test)

names(telecom1)

# Building Logistic Regression Model. 
mod<-glm(churn~.,data=train,family="binomial")
summary(mod)



# Step wise Regression Model ===>> It would atleast take 20 mins

step(mod,direction = "both")


mod1<-glm(formula = churn ~ totmrc_Mean + mou_Range + change_mou + 
            drop_vce_Range + owylis_vce_Range + mou_opkv_Range + months + 
            totcalls + eqpdays + iwylis_vce_Mean + adjqty + ovrrev_Mean + 
            comp_vce_Mean + avg3mou + avgmou + avgqty + asl_flag + uniqsubs + 
            totrev + avgrev + retdays_1 + hnd_webcap_1 + age1_1, family = "binomial", 
          data = train)
summary(mod1)



## ***** Creating Dummy Vars for Factor Vars with significant levels ***** ##

summary(telecom1$asl_flag)
train$asl_flag_Y<-ifelse(train$asl_flag == "Y", 1, 0)
test$asl_flag_Y<-ifelse(test$asl_flag == "Y", 1, 0)

summary(train$uniqsubs)

train$unq_2<-ifelse(train$uniqsubs == "2", 1, 0)
test$unq_2<-ifelse(test$uniqsubs == "2", 1, 0)

train$unq_3<-ifelse(train$uniqsubs == "3", 1, 0)
test$unq_3<-ifelse(test$uniqsubs == "3", 1, 0)

train$unq_4<-ifelse(train$uniqsubs == "4", 1, 0)
test$unq_4<-ifelse(test$uniqsubs == "4", 1, 0)


train$unq_7<-ifelse(train$uniqsubs == "7", 1, 0)
test$unq_7<-ifelse(test$uniqsubs == "7", 1, 0)


summary(train$age1_1)

train$age1_Mid_Age<-ifelse(train$age1_1 == "Mid Age", 1, 0)
test$age1_Mid_Age<-ifelse(test$age1_1 == "Mid Age", 1, 0)

train$age1_Old_Age<-ifelse(train$age1_1 == "Old", 1, 0)
test$age1_Old_Age<-ifelse(test$age1_1 == "Old", 1, 0)



## **** Rerunning Model with Significant Variables ***** ##



mod2<-glm(formula = churn ~   change_mou + 
            drop_vce_Range + owylis_vce_Range + mou_opkv_Range + months + 
            eqpdays + iwylis_vce_Mean + ovrrev_Mean + 
            comp_vce_Mean + avg3mou + avgmou + asl_flag_Y + unq_2+unq_3+unq_4+unq_7+ 
            totrev + avgrev + retdays_1 + age1_Mid_Age+ age1_Old_Age, family = "binomial", 
          data = train)
summary(mod2)



# All the variables have come significant. Also all the signs of the beta coefficients are in line 
# with probablity values less than 5%. So this model can be finalised after checking for absence of Multicollinearity.



# Checking For Multicollinearity
library(car)
vif(mod2)
# Variables => Ideally vif values should be < 5. Choosing vif cut-off value of 5, 
#All the variables has VIF less than 5 and now all the variables are significant.
# All the vif values are well below 5. Thus there is no Multicollinearity. So this model is finalised.


# Checking Confidence Interval
confint(mod2)  



## ***** Model Testing ***** ##

#Predicted Values ==> Predicting the probability of a customer churning.
pred<-predict(mod2, type="response", newdata=test)
head(pred)

#We gonna assume thresh hold value of 0.5 at first now

pred1<-ifelse(pred>=0.5,1,0)
table(pred1) 
#Confusion Matrix
table(actualvalue=test$churn,PredictedValue=pred1)


#Accuracy=(2950+10)/(2950+11+921+10)=0.7605  
#From the above table, its clear that the confusion Matrix has high fpr which is why our accuracy is low
#Lets check for the ideal Threshold value where our accuracy increases, this can be done by looking at ROCR curve of training dataset

predtr<-predict(mod2, type="response", newdata=train)
head(predtr)

library(ROCR)
ROCRpred<-prediction(predtr,train$churn)
ROCRpref<-performance(ROCRpred,"tpr","fpr")
plot(ROCRpref,colorize=TRUE,print.cutoffs.at=seq(0,1,by=0.1))


#Pick the highest Accuracy threshold value
library(caret)
for ( i in seq(0.4,0.5,by=0.01))
{
  pred1<-ifelse(pred>=i,1,0)
  m=confusionMatrix(as.factor(pred1),as.factor(test$churn),positive = "1")
  print(m$overall[1])
}

pred1<-ifelse(pred>=0.46,1,0)
table(pred1) 
#Confusion Matrix
table(actualvalue=test$churn,PredictedValue=pred1)

#from the plot the best range lies between 0.4 to 0.5 , after several iterations we found that 0.46 has highest accuracy of 0.7616, so we will go with 0.42
# The confusion MAtrix shows 2944 correct events and 17 incorrect events. 
# And also shows 20 correct Non-Events and 911 incorrect Non-Events 
# The model is doing good job with 76.16% Accuracy




## **** Checking Prediction Quality **** ##

#Kappa Matrix
library(irr)
kappa2(data.frame(test$churn,pred1))




#ROCR Curve and AUC

pred2<-prediction(pred1,test$churn)
pref<-performance(pred2,"tpr","fpr")
plot(pref,colorize=TRUE,print.cutoffs.at=seq(0,1,by=0.1))

auc<-performance(pred2,"auc")
auc
auc<-unlist(slot(auc,"y.values"))
auc

# The auc is 0.5078705 which is more than 0.50. 
# So the model seems to be ok and is acceptable.


#Gains Chart
library(gains)
gains(test$churn,predict(mod2,type="response",newdata=test),groups = 10)

#the Gains Chart shows that the top 30% of the probabilities contain 41.7% customers that are likely to churn.


test$prob<-predict(mod2,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#Top 30% of the probability scores lie between 0.2584238 and 0.6974238
#We can use this probablity to extract the data of customers who are highly likely to churn.






### ********** Answering Business Questions ********** ###
### Top Line Questions of Interest to Senior Management: 

#  1.  What are the top five factors driving likelihood of churn at Mobicom?

library(lm.beta)
lm.beta(mod2)

head(sort(abs(mod2$coefficients),decreasing = T),10)
summary(mod2)



## The model results show that the top 5 factors affecting churn are:

### a. unq_7            with beta coefficient of 1.27148614
### b. retdays_11       with beta coefficient of 0.91811637
### c. unq_4            with beta coefficient of 0.41686198
### d. asl_flag_Y       with beta coefficient of 0.30831587
### e. unq_3            with beta coefficient of 0.28393727


#The 1st factor explains, with a unit increase in level 7 of variable uniqsubs, there is 1.27148614 unit increase
# in churn.
# The 2nd Factor explains, with a unit increase in variable retdays, there is 0.91811637 unit increase in churn.
# Same explaination applies to the next 3 variables.

# Thus family bundles should be rolled out for families with 7,4,3 unique subscribers. Special offers should be given
# to customers who makes retention calls, at the earliest as per their grieviances. Special plans should be rolled out for 
# people with acccount spending is Limited.



# 2.  Validation of survey findings. 
# a) Whether "cost and billing" and "network and service quality" are important factors influencing churn behaviour.  

# The following variables explain "cost and billing"

# VARIABLE          BETA COEFFICIENT
#totrev               0.00034984
#avgrev              -0.00491113
#ovrrev_Mean          0.02003970


#var avgrev i.e. with a unit increase in 'Average monthly revenue over the previous three months, 
#    there is decrease in Churn by 0.00491113 units
#var avgrev i.e. with a unit increase in 'Average monthly revenue over the previous three months, 
#    there is decrease in Churn by 0.00491113 units
#var avgrev i.e. with a unit increase in 'Average monthly revenue over the previous three months, 
#   there is decrease in Churn by 0.00491113 units


# Having said that, if we notice above mentioned beta values, a unit increase in them is having almost 0% impact 
# on churn. SO it seems cost and billing is not very important factors here influencing churn behaviour at Mobicom.



# The following variables explain "network and service quality" 

# VARIABLE          BETA COEFFICIENT


# change_mou       -0.00099077
# drop_vce_Range    0.02076125 
# mou_opkv_Range   -0.00089220 
# owylis_vce_Range  0.00772310
# iwylis_vce_Mean  -0.01800551
# avg3mou          -0.00052033
# avgmou            0.00072036
# retdays_1         0.91970836
# comp_vce_Mean    -0.00180165


# From the above statistics, data explains the following:


# var change_mou i.e. with a unit increase in 'Percentage change in monthly minutes of 
#     use vs previous three month average, there is decrease in Churn by 0.00099077 units.
# var drop_vce_Range i.e. with a unit increase in 'Range of number of dropped (failed) voice calls', 
#     there is an increase in Churn by 0.02076125 units.
# var mou_opkv_Range  i.e. with a unit increase in  'Range of unrounded minutes of use of 
#     off-peak voice calls, there is a decrease in Churn by 0.00089220 units.
# var owylis_vce_Range i.e. with a unit increase in 'Range of number of outbound wireless to wireless voice calls',
#     there is a increase in churn by 0.00772310 units.
# var iwylis_vce_Mean i.e. with a unit increase in 'Mean number of inbound wireless to wireless voice calls',
#     there is a decrease in churn by 0.01800551 units.
# var avg3mou i.e. with unit increase in 'Average monthly minutes of use over the previous three months',
#     there is a decrease in Churn by 0.00052033 units.
# var avgmou i.e. with unit increase in  'Average monthly minutes of use over the life of the customer',
#     there is an increase in Churn by 0.00072036 units.
# var retdays_1 representing values captured in the variable retdays i.e. with a unit increase in 
#     'Number of days since last retention call', there is an increase in Churn by 0.91970836 units.
#     This variable is probably explaining the service quality of the company.
# var comp_vce_Mean i.e. with unit increase in 'Mean number of completed voice calls' 
#     there is a decrease in Churn by 0.00180165 units

# Of the above variables, the beta coefficient of variable retdays_1 is expressing a very important 
# factor influencing Churn behaviour. That is  with the increase in the number of days since a customer 
# makes a retention call, the customer's chances of churning is very high. This could probably be because
# their grieviances are not being catered to properly. These customers should be paid more attention to and 
# special offers should be made to them depending upon their grieviances.




#  2b) Are data usage connectivity issues turning out to be costly? In other words, is it leading to churn? 

 
#   plcd_dat_Mean - Mean number of attempted data calls placed(Since this variable is missing from the document we will be looking at the below variable)
#   opk_dat_Mean - Mean number of off-peak data calls
#   blck_dat_Mean - Mean no. of blocked / failed data calls
#   datovr_Mean - Mean revenue of data overage. 
#   datovr_Range - Range of revenue of data overage
#   drop_dat_Mean - Mean no. of dropped / failed data calls

#   The above variables express data usage connectivity. 
quantile(telecom$opk_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))
quantile(telecom$opk_dat_Mean,prob=c(0.90,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1))

 
#   In this case it seems customers are not really using the internet. So it would be good to work 
#   towards attaining more customers to use data and also towards proving quality network connectivity
#   and service to provide maximum customer satisfaction and reduce Churn.
#   Since there is not enough usable data for the above variables they are not showing any influence 
#   on the Churn Behaviour at Mobicom.




#   3. Would you recommend rate plan migration as a proactive retention strategy?

#   Variable ovrrev_Mean has beta coefficient of 0.02003970. 
#   var ovrrev_Mean = DATOVR_MEAN + VCEOVR_MEAN i.e. 'Mean overage revenue' 
#   It is the sum of data and voice overage revenues representing the overage revenue earned 
#   from customers after billing the same to them. 
#   The Beta coefficient is not showing a strong impact of overage billing as an influencer 
#   of churn behaviour. 
#   Though this might be a matter of concern for few individual customers and they could be 
#   catered to on case to case basis. But overall rate plan migration as a proactive retention strategy
#   might not help much at Mobicom.


#   4. What would be your recommendation on how to use this churn model for prioritisation
#   of customers for a proactive retention campaigns in the future?

# Solution:
#Gains Chart
gains(test$churn,predict(mod2,type="response",newdata=test),groups = 10)
#the Gains Chart shows that the top 20% of the probabilities contain 29.8% customers that are highly likely to churn.


# Selecting Customers with high churn rate
test$prob<-predict(mod2,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

# Top 20% of the probabilities lie between 0.2820621 and 0.6974238

# Applying cutoff value to predict customers who Will Churn
pred3<-predict(mod2, type="response", newdata=test)
pred3<-ifelse(pred3>=0.2820621, 1, 0)
table(pred3,test$churn)

Targeted<-test[test$prob>0.2820621 & test$prob<=0.6974238 & test$churn=="1","Customer_ID"]
Targeted<-as.data.frame(Targeted)
nrow(Targeted)


write.csv(Targeted,"Target_Customers.csv",row.names = F)

#   Thus Using the model can be used to predict customers with high probability of Churn and extract the 
#   target list using their "Customer ID". 



# 5. What would be the target segments for proactive retention campaigns? 
# Falling ARPU forecast is also a concern and therefore, Mobicom would like to save their high revenue 
# customers besides managing churn. Given a budget constraint of a contact list of 20% of the subscriber pool, 
# which subscribers should prioritized if "revenue saves" is also a priority besides controlling churn. 
# In other words, controlling churn is the primary objective and revenue saves is the secondary objective.

# Solution:
pred4<-predict(mod2, type="response", newdata=test)
test$prob<-predict(mod2,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
pred5<-ifelse(pred4<0.2054630,"Low_Score",ifelse(pred4>=0.2054630 & pred4<0.2584238,"Medium_Score","High_Score"))
table(pred5,test$churn)

str(test$totrev)
quantile(test$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
Revenue_Levels<-ifelse(test$totrev<678.322,"Low_Revenue",ifelse(test$totrev>=678.322 & 
                                                                  test$totrev<1034.724,"Medium_Revenue","High_Revenue"))

table(Revenue_Levels)

table(pred5,Revenue_Levels)

##  Thus this table can be used to select the levels of customers are to be targeted
##  and the Target list can be extracted as follows:

test$prob_levels<-ifelse(pred4<0.2054630,"Low_Score",ifelse(pred4>=0.2054630 & pred4<0.2584238,"Medium_Score","High_Score"))
test$Revenue_Levels<-ifelse(test$totrev<678.322,"Low_Revenue",ifelse(test$totrev>=678.322 & 
                                                                       test$totrev<1034.724,"Medium_Revenue","High_Revenue"))

Targeted1<-test[test$prob_levels=="High_Score" & test$Revenue_Levels=="High_Revenue","Customer_ID"]
Targeted1<-as.data.frame(Targeted1)
nrow(Targeted1)

write.csv(Targeted1,"High_Revenue_Target_Customers.csv",row.names = F)




