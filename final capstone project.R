#=================================================================================================================================#
#=======================================Analysis on Telecom Churn Dataset=========================================================#

#----------------------------------------------------------------------------------------------------------------------------------
#step 1 -> Loading dataset and save it as data
#----------------------------------------------------------------------------------------------------------------------------------

library(dplyr)
getwd()
data<-read.csv("telecomfinal.csv", header = TRUE, sep = ',')
options(scipen = 999)
names(data)
str(data)
summary(data) 


#-----------------------------------------------------------------------------------------------------------------------------------
#step 2 -> Creating Data Quality Report
#-----------------------------------------------------------------------------------------------------------------------------------
#Extracting Variable names
var <- names(data)
data_report<-as.data.frame(var)
rm(var)

#Recording Data Type for each Variable
data_report$DataType<-sapply(data,class)
data_report$NoOfRecords<-nrow(data)

#Counting No. of Unique Values for each variable
for(i in 1:ncol(data))
{
  data_report$UniqueRecords[i]<-length(unique(data[,i]))
}

#No.of observations available for each variable and its percentage
data_report$DataAvailable<-colSums(!is.na(data))
data_report$AvailablePercentage<-round(colMeans(!is.na(data)),4)
data_report$Missing<-colSums(is.na(data))
data_report$MissingPercentage<-round(colMeans(is.na(data)),4)

#Minimum, Maximum, Mean, Quantile Values for each Variable
for(i in 1:ncol(data))
{
  data_report$Minimum[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",min(data[,i],na.rm=T),0),2)
  data_report$Maximum[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",max(data[,i],na.rm=T),0),2)
  data_report$Mean[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",mean(data[,i],na.rm=T),0),2)
  data_report$"5th Percentile"[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.05,na.rm=T),0),2)
  data_report$"10th Percentile"[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.10,na.rm=T),0),2)
  data_report$"25th Percentile"[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.25,na.rm=T),0),2)
  data_report$"50th Percentile"[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.50,na.rm=T),0),2)
  data_report$"75th Percentile"[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.75,na.rm=T),0),2)
  data_report$"90th Percentile"[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.90,na.rm=T),0),2)
  data_report$"95thPercentile"[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.95,na.rm=T),0),2)
}

str(data_report)

#Exporting Data Quality Report
write.csv(data_report,"Data Quality Report.csv",row.names = T)
#------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------


#Missing Value treatment of var, "retdays" and Creating Dummy Variable
summary(data$retdays)
sort(unique(data$retdays), na.last = F)
data$retdays_1<-ifelse(is.na(data$retdays)==TRUE, 0, 1)
str(data$retdays_1)
summary(data$retdays_1)

#Omitting variables with more than 20% missing values and creating a new data set
data1<-data[,colMeans(is.na(tele))<=0.20]


##-----------------------Data Exploration - Profiling (dat-Continuous Variables , datC-Categorical Variables)---------------------##

#---------------------------------------------------------------------------------------------------------------------------------------
# Step 3 -> Variable Profiling: Continuous Variables
#--------------------------------------------------------------------------------------------------------------------------------
names(data1)
str(data1)

# <1>Variable 'mou_Mean'
summary(data1$mou_Mean)
data1%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat1
dat1$N<-unclass(data1%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat1$churn_perc<-dat1$n/dat1$N
dat1$GreaterThan<-unclass(data1%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dat1$LessThan<-unclass(data1%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dat1$varname<-rep("mou_Mean",nrow(dat1))

# <2> Variable "totmrc_Mean" 
summary(data1$totmrc_Mean)
data1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat2
dat2$N<-unclass(data1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat2$churn_perc<-dat2$n/dat2$N
dat2$GreaterThan<-unclass(data1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat2$LessThan<-unclass(data1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat2$varname<-rep("totmrc_Mean",nrow(dat2))

# <3> Variable "rev_Range" 
summary(data1$rev_Range)
data1%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat3
dat3$N<-unclass(data1%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat3$churn_perc<-dat3$n/dat3$N
dat3$GreaterThan<-unclass(data1%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
dat3$LessThan<-unclass(data1%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
dat3$varname<-rep("rev_Range",nrow(dat3))

# <4> Variable "mou_Range" 
summary(data1$mou_Range)
data1%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat4
dat4$N<-unclass(data1%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat4$churn_perc<-dat4$n/dat4$N
dat4$GreaterThan<-unclass(data1%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
dat4$LessThan<-unclass(data1%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
dat4$varname<-rep("mou_Range",nrow(dat4))                  

# <5> Variable "change_mou" 
summary(data1$change_mou)
data1%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat5
dat5$N<-unclass(data1%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
dat5$churn_perc<-dat5$n/dat5$N
dat5$GreaterThan<-unclass(data1%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
dat5$LessThan<-unclass(data1%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
dat5$varname<-rep("change_mou",nrow(dat5))    

# <6> Variable "drop_blk_Mean" 
data1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat6
dat6$N<-unclass(data1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat6$churn_perc<-dat6$n/dat6$N
dat6$GreaterThan<-unclass(data1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
dat6$LessThan<-unclass(data1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
dat6$varname<-rep("drop_blk_Mean",nrow(dat6)) 

# <7> Variable "drop_vce_Range" 
summary(data1$drop_vce_Range)
data1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat7
dat7$N<-unclass(data1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat7$churn_perc<-dat7$n/dat7$N
dat7$GreaterThan<-unclass(data1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
dat7$LessThan<-unclass(data1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
dat7$varname<-rep("drop_vce_Range",nrow(dat7)) 

# <8> Variable "owylis_vce_Range" 
summary(data1$owylis_vce_Range)
data1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat8
dat8$N<-unclass(data1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat8$churn_perc<-dat8$n/dat8$N
dat8$GreaterThan<-unclass(data1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
dat8$LessThan<-unclass(data1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
dat8$varname<-rep("owylis_vce_Range",nrow(dat8))

# <9> Variable "mou_opkv_Range" 
summary(data1$mou_opkv_Range)
data1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat9
dat9$N<-unclass(data1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat9$churn_perc<-dat9$n/dat9$N
dat9$GreaterThan<-unclass(data1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
dat9$LessThan<-unclass(data1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
dat9$varname<-rep("mou_opkv_Range",nrow(dat9))

# <10> Variable "months" 
summary(data1$months)
data1%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat10
dat10$N<-unclass(data1%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
dat10$churn_perc<-dat10$n/dat10$N
dat10$GreaterThan<-unclass(data1%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
dat10$LessThan<-unclass(data1%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
dat10$varname<-rep("months",nrow(dat10))

# <11> Variable "totcalls" 
summary(data1$totcalls)
data1%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat11
dat11$N<-unclass(data1%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
dat11$churn_perc<-dat11$n/dat11$N
dat11$GreaterThan<-unclass(data1%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
dat11$LessThan<-unclass(data1%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
dat11$varname<-rep("totcalls",nrow(dat11))

# <12> Variable "eqpdays"
summary(data1$eqpdays)
#Missing Value Treatment - Since there is just 1 missing observation, will remove the same.
index<-which(is.na(data1$eqpdays))
data1<-data1[-index,]

#Deciling basis Variable churn 
data1%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat12
dat12$N<-unclass(data1%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]]
dat12$churn_perc<-dat12$n/dat12$N
dat12$GreaterThan<-unclass(data1%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
dat12$LessThan<-unclass(data1%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
dat12$varname<-rep("eqpdays",nrow(dat12))


# <13> Variable "custcare_Mean"===>> Less than 4 decile-----omit
summary(data1$custcare_Mean)
data1%>%mutate(dec=ntile(custcare_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat13
dat13$varname<-rep("custcare_Mean",nrow(dat13))
plot(data1$churn,data1$custcare_Mean, col="red")

# <14> Variable "callwait_Mean"
summary(data1$callwait_Mean)
data1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat14
dat14$N<-unclass(data1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat14$churn_perc<-dat14$n/dat14$N
dat14$GreaterThan<-unclass(data1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
dat14$LessThan<-unclass(data1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
dat14$varname<-rep("callwait_Mean",nrow(dat14))

# <15> Variable "iwylis_vce_Mean"
summary(data1$iwylis_vce_Mean)
data1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat15
dat15$N<-unclass(data1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(dec)%>%unname())[[2]]
dat15$churn_perc<-dat15$n/dat15$N
dat15$GreaterThan<-unclass(data1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
dat15$LessThan<-unclass(data1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
dat15$varname<-rep("iwylis_vce_Mean",nrow(dat15))


# <16> Variable "callwait_Range"===>>Less than 4 decile------Omit
summary(data1$callwait_Range)
data1%>%mutate(dec=ntile(callwait_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat16
dat16$varname<-rep("callwait_Range",nrow(dat16))
plot(data1$churn,data1$callwait_Range, col="red")

# <17> Variable "ccrndmou_Range"===>> Less than 4 decile-------Omit
summary(data1$ccrndmou_Range)
data1%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat17
dat17$varname<-rep("ccrndmou_Range",nrow(dat17))
plot(data1$churn,data1$ccrndmou_Range, col="red")

# <18> Variable "adjqty"
summary(data1$adjqty)
data1%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat18
dat18$N<-unclass(data1%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
dat18$churn_perc<-dat18$n/dat18$N
dat18$GreaterThan<-unclass(data1%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
dat18$LessThan<-unclass(data1%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
dat18$varname<-rep("adjqty",nrow(dat18))


# <19> Variable "ovrrev_Mean"
summary(data1$ovrrev_Mean)
data1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat19
dat19$N<-unclass(data1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat19$churn_perc<-dat19$n/dat19$N
dat19$GreaterThan<-unclass(data1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
dat19$LessThan<-unclass(data1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
dat19$varname<-rep("ovrrev_Mean",nrow(dat19))


# <20> Variable "rev_Mean"
summary(data1$rev_Mean)
data1%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat20
dat20$N<-unclass(data1%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat20$churn_perc<-dat20$n/dat20$N
dat20$GreaterThan<-unclass(data1%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dat20$LessThan<-unclass(data1%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
dat20$varname<-rep("rev_Mean",nrow(dat20))


# <21> Variable "ovrmou_Mean"
summary(data1$ovrmou_Mean)
data1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat21
dat21$N<-unclass(data1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat21$churn_perc<-dat21$n/dat21$N
dat21$GreaterThan<-unclass(data1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
dat21$LessThan<-unclass(data1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
dat21$varname<-rep("ovrmou_Mean",nrow(dat21))


# <22> Variable "comp_vce_Mean" ===>>Data Transformation then Delete 
summary(data1$comp_vce_Mean)
data1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat22
dat22$N<-unclass(data1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat22$churn_perc<-dat22$n/dat22$N
dat22$GreaterThan<-unclass(data1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
dat22$LessThan<-unclass(data1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
dat22$varname<-rep("comp_vce_Mean",nrow(dat22))


# <23> Variable "plcd_vce_Mean" ===>>Data Transformation then Delete
summary(data1$plcd_vce_Mean)
data1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat23
dat23$N<-unclass(data1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat23$churn_perc<-dat23$n/dat23$N
dat23$GreaterThan<-unclass(data1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
dat23$LessThan<-unclass(data1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
dat23$varname<-rep("plcd_vce_Mean",nrow(dat23))


# <24> Variable "avg3mou"
summary(data1$avg3mou)
data1%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat24
dat24$N<-unclass(data1%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
dat24$churn_perc<-dat24$n/dat24$N
dat24$GreaterThan<-unclass(data1%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
dat24$LessThan<-unclass(data1%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
dat24$varname<-rep("avg3mou",nrow(dat24))


# <25> Variable "avgmou"
summary(data1$avgmou)
data1%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat25
dat25$N<-unclass(data1%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
dat25$churn_perc<-dat25$n/dat25$N
dat25$GreaterThan<-unclass(data1%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
dat25$LessThan<-unclass(data1%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
dat25$varname<-rep("avgmou",nrow(dat25))


# <26> Variable "avg3qty"
summary(data1$avg3qty)
data1%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat26
dat26$N<-unclass(data1%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
dat26$churn_perc<-dat26$n/dat26$N
dat26$GreaterThan<-unclass(data1%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
dat26$LessThan<-unclass(data1%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
dat26$varname<-rep("avg3qty",nrow(dat26))


# <27> Variable "avgqty"
summary(data1$avgqty)
data1%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat27
dat27$N<-unclass(data1%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
dat27$churn_perc<-dat27$n/dat27$N
dat27$GreaterThan<-unclass(data1%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
dat27$LessThan<-unclass(data1%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
dat27$varname<-rep("avgqty",nrow(dat27))


# <28> Variable "avg6mou"
summary(data1$avg6mou)
data1%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat28
dat28$N<-unclass(data1%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
dat28$churn_perc<-dat28$n/dat28$N
dat28$GreaterThan<-unclass(data1%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
dat28$LessThan<-unclass(data1%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
dat28$varname<-rep("avg6mou",nrow(dat28))


# <29> Variable "avg6qty"
summary(data1$avg6qty)
data1%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat29
dat29$N<-unclass(data1%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
dat29$churn_perc<-dat29$n/dat29$N
dat29$GreaterThan<-unclass(data1%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
dat29$LessThan<-unclass(data1%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
dat29$varname<-rep("avg6qty",nrow(dat29))


# <30> Variable "age1" =====>>Use As Factor Variable
summary(data1$age1)
data1%>%mutate(dec=ntile(age1,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat30
dat30$N<-unclass(data1%>%mutate(dec=ntile(age1,n=6))%>%count(dec)%>%unname())[[2]]
dat30$churn_perc<-dat30$n/dat30$N
dat30$GreaterThan<-unclass(data1%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(min(age1)))[[2]]
dat30$LessThan<-unclass(data1%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(max(age1)))[[2]]
dat30$varname<-rep("age1",nrow(dat30))


# <31> Variable "age2"===>> Less than 4 decile.....Use As Factor variable
summary(data1$age2)
data1%>%mutate(dec=ntile(age2,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat31
dat31$varname<-rep("age2",nrow(dat31))


# <32> Variable "models" ===>>Less than 4 decile.... Factor Variable
summary(data1$models)
data1%>%mutate(dec=ntile(models,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat32
dat32$varname<-rep("models",nrow(dat32))


# <33> Variable "hnd_price" =====>> Use as Factor Variable variable
summary(data1$hnd_price)
data1%>%mutate(dec=ntile(hnd_price,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat33
dat33$N<-unclass(data1%>%mutate(dec=ntile(hnd_price,n=10))%>%count(dec)%>%unname())[[2]]
dat33$churn_perc<-dat33$n/dat33$N
dat33$GreaterThan<-unclass(data1%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]
dat33$LessThan<-unclass(data1%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]
dat33$varname<-rep("hnd_price",nrow(dat33))

# <34> Variable "actvsubs" ===>>Factor Variable
summary(data1$actvsubs)
data1%>%mutate(dec=ntile(actvsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat34
dat34$varname<-rep("actvsubs",nrow(dat34))


# <35> Variable "uniqsubs" ===>>Factor Variable
summary(data1$uniqsubs)
data1%>%mutate(dec=ntile(uniqsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat35
dat35$varname<-rep("uniqsubs",nrow(dat35))


# <36> Variable "forgntvl" ===>>Factor Variable
summary(data1$forgntvl)
data1%>%mutate(dec=ntile(forgntvl,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat36
dat36$varname<-rep("forgntvl",nrow(dat36))


# <37> Variable "opk_dat_Mean" ===>>less than 4 decile....omit
summary(data1$opk_dat_Mean)
data1%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat37
dat37$varname<-rep("opk_dat_Mean",nrow(dat37))


# <38> Variable "mtrcycle" ===>>Factor variable
summary(data1$mtrcycle)
data1%>%mutate(dec=ntile(mtrcycle,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat38
dat38$varname<-rep("mtrcycle",nrow(dat38))


# <39> Variable "truck" ===>> Factor variable
summary(data1$truck)
data1%>%mutate(dec=ntile(truck,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat39
dat39$varname<-rep("truck",nrow(dat39))


# <40> Variable "roam_Mean" ===>>Less than 4 decile.....Omit
summary(data1$roam_Mean)
data1%>%mutate(dec=ntile(roam_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat40
dat40$varname<-rep("roam_Mean",nrow(dat40))


# <41> Variable "recv_sms_Mean" ===>>less than 4 deciles.........Omit
summary(data1$recv_sms_Mean)
data1%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat41
dat41$varname<-rep("recv_sms_Mean",nrow(dat41))

# <42> var "blck_dat_Mean" =======> less than 4 decile .....omit
summary(data1$blck_dat_Mean)
data1%>%mutate(dec = ntile(blck_dat_Mean, n= 4))%>%count(churn, dec)%>%filter(churn==1)-> dat42
dat42$varname<- rep("blck_dat_Mean", nrow(dat42))
plot(data1$churn,data1$blck_dat_Mean, col = "red")


# <43> Variable "mou_pead_Mean" ===>>less than 4 deciles..... Omit
summary(data1$mou_pead_Mean)
data1%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat43
dat43$varname<-rep("mou_pead_Mean",nrow(dat43))
plot(data1$churn,data1$mou_pead_Mean, col = "red")


# <44> Variable "da_Mean"
summary(data1$da_Mean)
data1%>%mutate(dec=ntile(da_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat44
dat44$N<-unclass(data1%>%mutate(dec=ntile(da_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat44$churn_perc<-dat44$n/dat44$N
dat44$GreaterThan<-unclass(data1%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
dat44$LessThan<-unclass(data1%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
dat44$varname<-rep("da_Mean",nrow(dat44))


# <45> Variable "da_Range"
summary(data1$da_Range)
data1%>%mutate(dec=ntile(da_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat45
dat45$N<-unclass(data1%>%mutate(dec=ntile(da_Range,n=4))%>%count(dec)%>%unname())[[2]]
dat45$churn_perc<-dat45$n/dat45$N
dat45$GreaterThan<-unclass(data1%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
dat45$LessThan<-unclass(data1%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
dat45$varname<-rep("da_Range",nrow(dat45))


# <46> Variable "datovr_Mean" ===>>less than 4 deciles.......Omit
summary(data1$datovr_Mean)
data1%>%mutate(dec=ntile(datovr_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat46
dat46$varname<-rep("datovr_Mean",nrow(dat46))
plot(data1$churn,data1$datovr_Mean, col = "red")

# <47> Variable "datovr_Range" ===>>less than 4 deciles......Omit
summary(data1$datovr_Range)
data1%>%mutate(dec=ntile(datovr_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat47
dat47$varname<-rep("datovr_Range",nrow(dat47))
plot(data1$churn,data1$datovr_Range, col = "red")

# <48> Variable "drop_dat_Mean" ===>> ess than 4 deciles......omit 
summary(data1$drop_dat_Mean)
data1%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat48
dat48$varname<-rep("drop_dat_Mean",nrow(dat48))
plot(data1$churn,data1$drop_dat_Mean, col = "red")


# <49> Variable "drop_vce_Mean" 
summary(data1$drop_vce_Mean)
data1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat49
dat49$N<-unclass(data1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat49$churn_perc<-dat49$n/dat49$N
dat49$GreaterThan<-unclass(data1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
dat49$LessThan<-unclass(data1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
dat49$varname<-rep("drop_vce_Mean",nrow(dat49))


# <50> Variable "adjmou" 
summary(data1$adjmou)
data1%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat50
dat50$N<-unclass(data1%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
dat50$churn_perc<-dat50$n/dat50$N
dat50$GreaterThan<-unclass(data1%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
dat50$LessThan<-unclass(data1%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
dat50$varname<-rep("adjmou",nrow(dat50))


# <51> Variable "totrev"
summary(data1$totrev)
data1%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat51
dat51$N<-unclass(data1%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat51$churn_perc<-dat51$n/dat51$N
dat51$GreaterThan<-unclass(data1%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat51$LessThan<-unclass(data1%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat51$varname<-rep("totrev",nrow(dat51))


# <52> Variable "adjrev" 
summary(data1$adjrev)
data1%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat52
dat52$N<-unclass(data1%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
dat52$churn_perc<-dat52$n/dat52$N
dat52$GreaterThan<-unclass(data1%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
dat52$LessThan<-unclass(data1%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
dat52$varname<-rep("adjrev",nrow(dat52))


# <53> Variable "avgrev" 
summary(data1$avgrev)
data1%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat53
dat53$N<-unclass(data1%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
dat53$churn_perc<-dat53$n/dat53$N
dat53$GreaterThan<-unclass(data1%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
dat53$LessThan<-unclass(data1%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
dat53$varname<-rep("avgrev",nrow(dat53))


# <54> Variable "comp_dat_Mean" ===>> ***Data Transformation then omit***
summary(data1$comp_dat_Mean)
data1%>%mutate(dec=ntile(comp_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat54
dat54$varname<-rep("comp_dat_Mean",nrow(dat54))


# <55> Variable "plcd_dat_Mean" ====>> ****Data Transformation then omit****
summary(data1$plcd_dat_Mean)
data1%>%mutate(dec=ntile(plcd_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat55
dat55$varname<-rep("plcd_dat_Mean",nrow(dat55))



##---------Data Transformation- Creating Dummy Variables--------##

#<56> Create Dummy Variable plcd_Atempt_Mean and Deciling
data1$plcd_attempt_mean<-data1$plcd_vce_Mean+data1$plcd_dat_Mean

summary(data1$plcd_attempt_mean)
data1%>%mutate(dec=ntile(plcd_attempt_mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat56
dat56$N<-unclass(data1%>%mutate(dec=ntile(plcd_attempt_mean,n=10))%>%count(dec)%>%unname())[[2]]
dat56$churn_perc<-dat56$n/dat56$N
dat56$GreaterThan<-unclass(data1%>%mutate(dec=ntile(plcd_attempt_mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_attempt_mean)))[[2]]
dat56$LessThan<-unclass(data1%>%mutate(dec=ntile(plcd_attempt_mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_attempt_mean)))[[2]]
dat56$varname<-rep("plcd_attempt_mean",nrow(dat56))


#<57> Create Dummy Variable complete_Mean and Deciling
data1$complete_mean<-data1$comp_vce_Mean+data1$comp_dat_Mean
summary(data1$complete_mean)
data1%>%mutate(dec=ntile(complete_mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat57
dat57$N<-unclass(data1%>%mutate(dec=ntile(complete_mean,n=10))%>%count(dec)%>%unname())[[2]]
dat57$churn_perc<-dat57$n/dat57$N
dat57$GreaterThan<-unclass(data1%>%mutate(dec=ntile(complete_mean,n=10))%>%group_by(dec)%>%summarise(min(complete_mean)))[[2]]
dat57$LessThan<-unclass(data1%>%mutate(dec=ntile(complete_mean,n=10))%>%group_by(dec)%>%summarise(max(complete_mean)))[[2]]
dat57$varname<-rep("complete_mean",nrow(dat57))


#Adding all appropriate dat1 to dat54 objects to create a dat object
dat<-rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8,dat9,dat10,dat11,dat12,dat14,dat15,dat18,dat19,
           dat20,dat21,dat22,dat23,dat24,dat25,dat26,dat27,dat28,dat29,dat30,dat33,dat44,
           dat45,dat49,dat50,dat51,dat52,dat53,dat56,dat57)

#Exporting Deciled variables
write.csv(dat,"Deciled Continuous variables.csv",row.names = F)


# Removing variable that could not decile and also omit transformed variable
as.data.frame((colnames(data1)))
str(data1)
data1<-data1[,-c(13,16,17,22,23,45,48,49,50,51,57,58,59,66,67)]

#------------------------------------------------------------------------------------------------------------------------------------------
# Step 4 -> variable profiling - Categorical Variables
#------------------------------------------------------------------------------------------------------------------------------------

str(data1)
as.data.frame(colnames(data1))

# Variable "crclscod" =====>>>Some Levels show less than 5% churn rate. So Omit as will come insignificant 
summary(data1$crclscod)
data1%>%count(churn,levels=crclscod)%>%filter(churn==1)->datC1
datC1$N<-unclass(data1%>%filter(crclscod%in%datC1$levels)%>%count(crclscod))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("crclscod",nrow(datC1))


# Variable "asl_flag"  
summary(data1$asl_flag)
data1%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datC2
datC2$N<-unclass(data1%>%filter(asl_flag%in%datC2$levels)%>%count(asl_flag))[[2]]
datC2$ChurnPerc<-datC2$n/datC2$N
datC2$Var.Name<-rep("asl_flag",nrow(datC2))


# Variable "prizm_social_one"  
summary(data1$prizm_social_one)
data1%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datC3
datC3$N<-unclass(data1%>%filter(prizm_social_one%in%datC3$levels)%>%count(prizm_social_one))[[2]]
datC3$ChurnPerc<-datC3$n/datC3$N
datC3$Var.Name<-rep("prizm_social_one",nrow(datC3))


# Variable "area"  
summary(data1$area)
data1%>%count(churn,levels=area)%>%filter(churn==1)->datC4
datC4$N<-unclass(data1%>%filter(area%in%datC4$levels)%>%count(area))[[2]]
datC4$ChurnPerc<-datC4$n/datC4$N
datC4$Var.Name<-rep("area",nrow(datC4))


# Variable "refurb_new"  
summary(data1$refurb_new)
data1%>%count(churn,levels=refurb_new)%>%filter(churn==1)->datC5
datC5$N<-unclass(data1%>%filter(refurb_new%in%datC5$levels)%>%count(refurb_new))[[2]]
datC5$ChurnPerc<-datC5$n/datC5$N
datC5$Var.Name<-rep("refurb_new",nrow(datC5))


# Variable "hnd_webcap"  
summary(data1$hnd_webcap)
data1%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datC6
datC6$N<-unclass(data1%>%filter(hnd_webcap%in%datC6$levels)%>%count(hnd_webcap))[[2]]
datC6$ChurnPerc<-datC6$n/datC6$N
datC6$Var.Name<-rep("hnd_webcap",nrow(datC6))


# Variable "marital"  
summary(data1$marital)
data1%>%count(churn,levels=marital)%>%filter(churn==1)->datC7
datC7$N<-unclass(data1%>%filter(marital%in%datC7$levels)%>%count(marital))[[2]]
datC7$ChurnPerc<-datC7$n/datC7$N
datC7$Var.Name<-rep("marital",nrow(datC7))


# Variable "ethnic"  
summary(data1$ethnic)
data1%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC8
datC8$N<-unclass(data1%>%filter(ethnic%in%datC8$levels)%>%count(ethnic))[[2]]
datC8$ChurnPerc<-datC8$n/datC8$N
datC8$Var.Name<-rep("ethnic",nrow(datC8))



# Variable "car_buy"  
summary(data1$car_buy)
data1%>%count(churn,levels=car_buy)%>%filter(churn==1)->datC9
datC9$N<-unclass(data1%>%filter(car_buy%in%datC9$levels)%>%count(car_buy))[[2]]
datC9$ChurnPerc<-datC9$n/datC9$N
datC9$Var.Name<-rep("car_buy",nrow(datC9))



# Variable "csa" =====>>>Some Levels show less than 5% churn rate. So Omit as will come insignificant 
summary(data1$csa)
data1%>%count(churn,levels=csa)%>%filter(churn==1)->datC10
datC10$N<-unclass(data1%>%filter(csa%in%datC10$levels)%>%count(csa))[[2]]
datC10$ChurnPerc<-datC10$n/datC10$N
datC10$Var.Name<-rep("csa",nrow(datC10))

# Variable "retdays_1"  
summary(data1$retdays_1)
data1$retdays_1<-as.factor(data1$retdays_1)
data1%>%count(churn,levels=retdays_1)%>%filter(churn==1)->datC11
datC11$N<-unclass(data1%>%filter(retdays_1%in%datC11$levels)%>%count(retdays_1))[[2]]
datC11$ChurnPerc<-datC11$n/datC11$N
datC11$Var.Name<-rep("retdays_1",nrow(datC11))


# Use VArs as Factor => age1, age2, models, actvsubs, uniqsubs, forgntvl, mtrcycle, truck, hnd_price

# Variable "age1"  
summary(data1$age1)
data1%>%count(churn,levels=age1)%>%filter(churn==1)->dataC121
dataC121$N<-unclass(data1%>%filter(age1%in%dataC121$levels)%>%count(age1))[[2]]
dataC121$ChurnPerc<-dataC121$n/dataC121$N
dataC121$Var.Name<-rep("age1",nrow(dataC121))

# Variable "age2"  
summary(data1$age2)
data1%>%count(churn,levels=age2)%>%filter(churn==1)->datC12
datC12$N<-unclass(data1%>%filter(age2%in%datC12$levels)%>%count(age2))[[2]]
datC12$ChurnPerc<-datC12$n/datC12$N
datC12$Var.Name<-rep("age2",nrow(datC12))

# Variable "models"  
summary(data1$models)
data1%>%count(churn,levels=models)%>%filter(churn==1)->datC13
datC13$N<-unclass(data1%>%filter(models%in%datC13$levels)%>%count(models))[[2]]
datC13$ChurnPerc<-datC13$n/datC13$N
datC13$Var.Name<-rep("models",nrow(datC13))

# Variable "actvsubs"  
summary(data1$actvsubs)
data1%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datC14
datC14$N<-unclass(data1%>%filter(actvsubs%in%datC14$levels)%>%count(actvsubs))[[2]]
datC14$ChurnPerc<-datC14$n/datC14$N
datC14$Var.Name<-rep("actvsubs",nrow(datC14))


# Variable "uniqsubs"  
summary(data1$uniqsubs)
data1%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datC15
datC15$N<-unclass(data1%>%filter(uniqsubs%in%datC15$levels)%>%count(uniqsubs))[[2]]
datC15$ChurnPerc<-datC15$n/datC15$N
datC15$Var.Name<-rep("uniqsubs",nrow(datC15))


# Variable "forgntvl"  
summary(data1$forgntvl)
data1%>%count(churn,levels=forgntvl)%>%filter(churn==1)->datC16
datC16$N<-unclass(data1%>%filter(forgntvl%in%datC16$levels)%>%count(forgntvl))[[2]]
datC16$ChurnPerc<-datC16$n/datC16$N
datC16$Var.Name<-rep("forgntvl",nrow(datC16))


# Variable "mtrcycle"  
summary(data1$mtrcycle)
data1%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->datC17
datC17$N<-unclass(data1%>%filter(mtrcycle%in%datC17$levels)%>%count(mtrcycle))[[2]]
datC17$ChurnPerc<-datC17$n/datC17$N
datC17$Var.Name<-rep("mtrcycle",nrow(datC17))


# Variable "truck"  
summary(data1$truck)
data1%>%count(churn,levels=truck)%>%filter(churn==1)->datC18
datC18$N<-unclass(data1%>%filter(truck%in%datC18$levels)%>%count(truck))[[2]]
datC18$ChurnPerc<-datC18$n/datC18$N
datC18$Var.Name<-rep("Truck",nrow(datC18))


# Variable "hnd_price"  
summary(data1$hnd_price)
data1%>%count(churn,levels=hnd_price)%>%filter(churn==1)->datC19
datC19$N<-unclass(data1%>%filter(hnd_price%in%datC19$levels)%>%count(hnd_price))[[2]]
datC19$ChurnPerc<-datC19$n/datC19$N
datC19$Var.Name<-rep("hnd_price",nrow(datC19))



#Adding datC1 to datC19 objects to create a datC object
datC_1<-rbind(datC1,datC2,datC3,datC4,datC5,datC6,datC7,datC8,datC9,
              datC10,datC11)

datC_2<-rbind(datC121,datC12,datC13,datC14,datC15,datC16,datC17,datC18,datC19)


#Exporting Deciled variables
write.csv(datC_1,"Event Rate - Categorical variables1.csv",row.names = F)
write.csv(datC_2,"Event Rate - Categorical variables2.csv",row.names = F)


#Removing Variables with levels less than 5% churn rate as will come insignificant
as.data.frame(colnames(data1))
data1<-data1[,-c(25,44)]
names(data1)



#====================================================Data Preparation=================================================================#

#------------------------------------------------------------------------------------------------------------------------------------------
# Step 5 -> Outlier Treatment
#---------------------------------------------------------------------------------------------------------------------------------------
#Outlier Treatment in continuous variable
names(data1)
summary(data1)
str(data1)
as.data.frame(colnames(data1))
list_cont<- names(data1)
list_cont<- list_cont[-c(25:42,50,51)]
list_cont
# Outlier Plots
par(mfrow=c(3,11))
for(i in 1:length(list_cont))
{
  boxplot(data1[,list_cont[i]],main=list_cont[i])
}

for(i in 1:length(list_cont))
{
  plot(data1[,list_cont[i]],main=list_cont[i])
}


# Outlier Treatment
for(i in 1:length(list_cont))
{
  x<-boxplot(data1[,list_cont[i]],main=list_cont[i])
  out<-x$out
  index<-which(data1[,list_cont[i]]%in% x$out)
  data1[index,list_cont[i]]<-mean(data1[,list_cont[i]],na.rm = T)
  rm(x)
  rm(out)
}

dev.off()


#--------------------------------------------------------------------------------------------------------------------------------
# Step 6 -> Missing Value Treatment
#------------------------------------------------------------------------------------------------------------------------------------

# variable "mou_mean"
summary(data1)
index1<-which(is.na(data1$mou_mean))
data1<- data1[-index1,]

# variable "change_mou"
summary(data1)
index2<- which(is.na(data1$change_mou))
data1<- data1[-index2,]

#-----Missing Value Imputation--------#
# Variable "avg6mou"
summary(data1)
data1$avg6mou[is.na(data1$avg6mou)]<- mean(data1$avg6mou, na.rm = T)

# Variable "avg6qty"
summary(data1)
data1$avg6qty[is.na(data1$avg6qty)]<- mean(data1$avg6qty, na.rm = T)

# Variable "hnd_price"
summary(data1)
data1$hnd_price[is.na(data1$hnd_price)]<- mean(data1$hnd_price, na.rm = T)


#Missing value treatment of categorical variable

# Variable "prizm_social_one"
summary(data1)
data1$prizm_social_one[is.na(data1$prizm_social_one)]<-"T"
summary(data1$prizm_social_one)

# Variable "area"
summary(data1)
index3 <- which(is.na(data1$area))
data1<- data1[-index3,]

# Variable "hnd_webcap"
summary(data1)
data1$hnd_webcap_1<- ifelse(is.na(data1$hnd_webcap), "Missing", as.factor(data1$hnd_webcap))
str(data$hnd_webcap_1)
data1$hnd_webcap_1<- as.factor(data1$hnd_webcap_1)
summary(data1$hnd_webcap)
summary(data1$hnd_webcap_1)
data1$hnd_webcap_1 <- factor(data1$hnd_webcap_1, labels = c("UNKW","WC","WCMB","Missing"))
summary(data1$hnd_webcap_1)

names(data1)
data1<- data1[,-29]
summary(data1)


# Variable "material"
data1$material[is.na(data1$material)] <- 'S'
summary(data1$material)

# Variable "ethnic"
summary(data1$ethnic)
summary(data1)

# Variable age1
data1$age1[is.na(data1$age1)]<- 80.0
summary(data1$age1)

# Variable "age2"
data1$age2[is.na(data1$age2)]<- 78.0
summary(data1$age2)

# Variable "forgntvl"
summary(data1)
index4<- which(is.na(data1$forgntvl))
data1<- data1[-index4,]
summary(data1)

#Checking churn rate in the data after imputations.
table(data$churn)/nrow(data)
table(data1$churn)/nrow(data1)


# Convert to Factor and Create Dummy Variables => 
#age1, age2, models, actvsubs,uniqsubs, forgntvl, mtrcycle, truck, Customer ID, Churn

# Variable "age1"
str(data1$age1)
data1$age1_1<-ifelse(data1$age1==0,"Default",ifelse(data1$age1<=30,"Young",
                                                    ifelse(data1$age1>30 & data1$age1<=55,"Middle Age","Old")))
str(data1$age1_1)
data1$age1_1<-as.factor(data1$age1_1)
summary(data1$age1_1)

names(data1)
data1<-data1[,-31]
summary(data1)


# Variable "age2"
str(data1$age2)
data1$age2_1<-ifelse(data1$age2==0,"Default",ifelse(data1$age2<=30,"Young",
                                                    ifelse(data1$age2>30 & data1$age2<=55,"Middle Age","Old")))
str(data1$age2_1)
data1$age2_1<-as.factor(data1$age2_1)
summary(data1$age2_1)

names(data1)
data1<-data1[,-31]
summary(data1)


# Variable "models"
str(data1$models)
summary(data1$models)
data1$models<-as.factor(data1$models)
summary(data1$models)

# Variable "hnd_price"
str(data1$hnd_price)
summary(data1$hnd_price)
data1$hnd_price<-as.factor(data1$hnd_price)
summary(data1$hnd_price)

# Variable "actvsubs"
str(data1$actvsubs)
summary(data1$actvsubs)
data1$actvsubs<-as.factor(data1$actvsubs)
summary(data1$actvsubs)

# Variable "uniqsubs"
str(data1$uniqsubs)
summary(data1$uniqsubs)
data1$uniqsubs<-as.factor(data1$uniqsubs)
summary(data1$uniqsubs)

# Variable "forgntvl"
str(data1$forgntvl)
summary(data1$forgntvl)
data1$forgntvl<-as.factor(data1$forgntvl)
summary(data1$forgntvl)

# Variable "mtrcycle"
str(data1$mtrcycle)
summary(data1$mtrcycle)
data1$mtrcycle<-as.factor(data1$mtrcycle)
summary(data1$mtrcycle)

# Variable "truck"
str(data1$truck)
summary(data1$truck)
data1$truck<-as.factor(data1$truck)
summary(data1$truck)


str(data1)



###===================================== Logistic Regression Model Building ======================================= ###
#----------------------------------------------------------------------------------------------------------------------------------------
#Step 7 -> Splitting into Test and Training Samples
#------------------------------------------------------------------------------------------------------------------------------------
set.seed(200)
index<-sample(nrow(data1),0.70*nrow(data1),replace=F)
train<-data1[index,]
test<-data1[-index,]


#Checking Churn Rate 
table(train$churn)/nrow(train)
table(test$churn)/nrow(test)

names(data1)

#----------------------------------------------------------------------------------------------------------------------------------------------
# Step 7 -> Building Logistic Regression Model
#--------------------------------------------------------------------------------------------------------------------------------------

# Building Logistic Regression Model after excluding var "Customer_ID" 
mod<-glm(churn~.,data=train[,-47],family="binomial")
summary(mod)

## Creating Dummy Vars for Factor Vars with significant levels  

# Variable asl_flag
summary(data1$asl_flag)
train$asl_flag_Y<-ifelse(train$asl_flag == "Y", 1, 0)
test$asl_flag_Y<-ifelse(test$asl_flag == "Y", 1, 0)

# Variable "prizm_social_one"
summary(data1$prizm_social_one)
train$prizm_social_one_R <- ifelse(train$prizm_social_one =="R",1,0)
test$prizm_social_one_R <- ifelse(test$prizm_social_one =="R",1,0)

train$prizm_social_one_T <- ifelse(train$prizm_social_one =="T",1,0)
test$prizm_social_one_T <- ifelse(test$prizm_social_one =="T",1,0)

# Variable "area"
summary(data1$area)
train$area_CALIFORNIA_NORTH_AREA<-ifelse(train$area == "CALIFORNIA NORTH AREA", 1, 0)
test$area_CALIFORNIA_NORTH_AREA<-ifelse(test$area == "CALIFORNIA NORTH AREA", 1, 0)

train$areaCENTRAL_SOUTH_TEXAS_AREA<-ifelse(train$area == "CENTRAL/SOUTH TEXAS AREA", 1, 0)
test$areaCENTRAL_SOUTH_TEXAS_AREA<-ifelse(test$area == "CENTRAL/SOUTH TEXAS AREA", 1, 0)

train$areaMIDWEST_AREA<-ifelse(train$area == "MIDWEST AREA", 1, 0)
test$areaMIDWEST_AREA<-ifelse(test$area == "MIDWEST AREA", 1, 0)

train$areaNORTH_FLORIDA_AREA<-ifelse(train$area == "NORTH FLORIDA AREA", 1, 0)
test$areaNORTH_FLORIDA_AREA<-ifelse(test$area == "NORTH FLORIDA AREA", 1, 0)

train$areaNORTHWEST_ROCKY_MOUNTAIN_AREA<-ifelse(train$area == "NORTHWEST/ROCKY MOUNTAIN AREA", 1, 0)
test$areaNORTHWEST_ROCKY_MOUNTAIN_AREA<-ifelse(test$area == "NORTHWEST/ROCKY MOUNTAIN AREA", 1, 0)

train$areaSOUTH_FLORIDA_AREA<-ifelse(train$area == "SOUTH FLORIDA AREA", 1, 0)
test$areaSOUTH_FLORIDA_AREA<-ifelse(test$area == "SOUTH FLORIDA AREA", 1, 0)

train$areaSOUTHWEST_AREA<-ifelse(train$area == "SOUTHWEST AREA", 1, 0)
test$areaSOUTHWEST_AREA<-ifelse(test$area == "SOUTHWEST AREA", 1, 0)

train$areaTENNESSEE_AREA<-ifelse(train$area == "TENNESSEE AREA", 1, 0)
test$areaTENNESSEE_AREA<-ifelse(test$area == "TENNESSEE AREA", 1, 0)

# Variable "refurb_new"
summary(data1$refurb_new)
train$refurb_new_R<-ifelse(train$refurb_new == "R", 1, 0)
test$refurb_new_R<-ifelse(test$refurb_new == "R", 1, 0)

# Variable "ethnic"
summary(data1$ethnic)
names(train)
train$ethnic_C<-ifelse(train$ethnic == "C", 1, 0)
test$ethnic_C<-ifelse(test$ethnic == "C", 1, 0)

train$ethnic_N<-ifelse(train$ethnic == "N", 1, 0)
test$ethnic_N<-ifelse(test$ethnic == "N", 1, 0)

train$ethnic_O<-ifelse(train$ethnic == "O", 1, 0)
test$ethnic_O<-ifelse(test$ethnic == "O", 1, 0)

train$ethnic_S<-ifelse(train$ethnic == "S", 1, 0)
test$ethnic_S<-ifelse(test$ethnic == "S", 1, 0)

train$ethnic_U<-ifelse(train$ethnic == "U", 1, 0)
test$ethnic_U<-ifelse(test$ethnic == "U", 1, 0)

train$ethnic_Z<-ifelse(train$ethnic == "Z", 1, 0)
test$ethnic_Z<-ifelse(test$ethnic == "Z", 1, 0)

# Variable "hnd_price"
summary(data1$hnd_price)

train$hnd_price79.99<-ifelse(train$hnd_price == "79.98999023", 1, 0)
test$hnd_price79.99<-ifelse(test$hnd_price == "79.98999023", 1, 0)

train$hnd_price105.2852<-ifelse(train$hnd_price == "105.285162930597", 1, 0)
test$hnd_price105.2852<-ifelse(test$hnd_price == "105.285162930597", 1, 0)

train$hnd_price_129.99<-ifelse(train$hnd_price == "129.9899902", 1, 0)
test$hnd_price_129.99<-ifelse(test$hnd_price == "129.9899902", 1, 0)

train$hnd_price_149.99<-ifelse(train$hnd_price == "149.9899902", 1, 0)
test$hnd_price_149.99<-ifelse(test$hnd_price == "149.9899902", 1, 0)

train$hnd_price_199.99<-ifelse(train$hnd_price == "199.9899902", 1, 0)
test$hnd_price_199.99<-ifelse(test$hnd_price == "199.9899902", 1, 0)

train$hnd_price249.99<-ifelse(train$hnd_price == "249.9899902", 1, 0)
test$hnd_price249.99<-ifelse(test$hnd_price == "249.9899902", 1, 0)

# Variable "uniqsubs"
summary(data1$uniqsubs)

train$uniqsubs_2<-ifelse(train$uniqsubs == "2", 1, 0)
test$uniqsubs_2<-ifelse(test$uniqsubs == "2", 1, 0)

train$uniqsubs_3<-ifelse(train$uniqsubs == "3", 1, 0)
test$uniqsubs_3<-ifelse(test$uniqsubs == "3", 1, 0)

train$uniqsubs_4<-ifelse(train$uniqsubs == "4", 1, 0)
test$uniqsubs_4<-ifelse(test$uniqsubs == "4", 1, 0)

train$uniqsubs_5<-ifelse(train$uniqsubs == "5", 1, 0) 
test$uniqsubs_5<-ifelse(test$uniqsubs == "5", 1, 0)

train$uniqsubs_6<-ifelse(train$uniqsubs == "6", 1, 0) 
test$uniqsubs_6<-ifelse(test$uniqsubs == "6", 1, 0)

train$uniqsubs_7<-ifelse(train$uniqsubs == "7", 1, 0)
test$uniqsubs_7<-ifelse(test$uniqsubs == "7", 1, 0)

train$uniqsubs_9<-ifelse(train$uniqsubs == "9", 1, 0)
test$uniqsubs_9<-ifelse(test$uniqsubs == "9", 1, 0)

# Variable "truck"
summary(data1$truck)
train$truck_1<- ifelse(train$truck == '1',1,0)
test$truck_1<- ifelse(test$truck == '1',1,0)

# Variable "totrev"
summary(data1$totrev)

# Variable "retdays"
summary(data1$retdays_1)
train$retdays_1_1 <- ifelse(train$retdays_1 == "1",1,0)
test$retdays_1_1 <- ifelse(test$retdays_1 == "1",1,0)

# Variable "complete_mean"
summary(data1$complete_mean)

# Variable "age1_1"
summary(train$age1_1)
train$age1_1MiddleAge<-ifelse(train$age1_1 == "Middle Age", 1, 0)
test$age1_1MiddleAge<-ifelse(test$age1_1 == "Middle Age", 1, 0)

train$age1_1old<-ifelse(train$age1_1 == "Old", 1, 0)
test$age1_1old<-ifelse(test$age1_1 == "Old", 1, 0)

train$age1_1young<-ifelse(train$age1_1 == "Young", 1, 0)
test$age1_1young<-ifelse(test$age1_1 == "Young", 1, 0)

# Variable "age2_1"
summary(data1$age2_1)

train$age2_1Old<-ifelse(train$age2_1 == "Old", 1, 0)
test$age2_1Old<-ifelse(test$age2_1 == "Old", 1, 0)




##---------Rerunning Model with Significant Variables--------------##

names(train)
mod1<-glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + drop_vce_Range +
            mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean + avgmou + avg3qty + avgqty +
            avg6mou + asl_flag_Y + prizm_social_one_R + prizm_social_one_T + area_CALIFORNIA_NORTH_AREA+areaCENTRAL_SOUTH_TEXAS_AREA + 
            areaMIDWEST_AREA+areaNORTH_FLORIDA_AREA+areaNORTHWEST_ROCKY_MOUNTAIN_AREA+areaSOUTH_FLORIDA_AREA+areaSOUTHWEST_AREA+
            areaTENNESSEE_AREA+refurb_new_R + ethnic_C + ethnic_N + ethnic_O + ethnic_S + ethnic_U + 
            ethnic_Z + hnd_price_79.99 + hnd_price_105.2852 + hnd_price_129.99 + hnd_price_149.99 + 
            hnd_price_199.99 + hnd_price_249.99 + uniqsubs_2 + uniqsubs_3 + uniqsubs_4 + uniqsubs_5 + uniqsubs_6 + uniqsubs_7 + uniqsubs_9 +
            truck_1 + adjmou + totrev + retdays_1_1 + complete_mean  + age1_1MiddleAge +
            age1_1old + age1_1young + age2_1old,data=train,family="binomial")
summary(mod1)




## ----------Further Rerunning Model with Significant Variables-----------##

mod2<-glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + drop_vce_Range +
            mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean + avgmou + avg3qty + avgqty +
            avg6mou + asl_flag_Y + prizm_social_one_R + prizm_social_one_T + area_CALIFORNIA_NORTH_AREA+areaCENTRAL_SOUTH_TEXAS_AREA + 
            areaMIDWEST_AREA+areaNORTH_FLORIDA_AREA+areaNORTHWEST_ROCKY_MOUNTAIN_AREA+areaSOUTH_FLORIDA_AREA+areaSOUTHWEST_AREA+
            areaTENNESSEE_AREA+refurb_new_R + ethnic_C + ethnic_N + ethnic_O + ethnic_S + ethnic_U + 
            ethnic_Z + hnd_price_79.99 + hnd_price_105.2852 + hnd_price_129.99 + hnd_price_149.99 + 
            hnd_price_199.99 + hnd_price_249.99 + uniqsubs_2 + uniqsubs_3 + uniqsubs_4 + uniqsubs_7 +
            truck_1 + adjmou + totrev + retdays_1_1 + complete_mean  + age1_1MiddleAge +
            age1_1old + age1_1young + age2_1old,data=train,family="binomial")
summary(mod2)

# All the variables have come significant. Also all the signs of the beta coefficients are in line 

##-------Model Diagnostics-------##

# Checking For Multicollinearity
library(car)
vif(mod2)
# vif values should be < 5. Choosing vif cut-off value of 5, 

# Re-running Model with above vars omited to remove problem of Multicollinearity.

mod3<-glm(churn ~ totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + drop_vce_Range +
            mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean + avgqty +
            asl_flag_Y + prizm_social_one_R + prizm_social_one_T + area_CALIFORNIA_NORTH_AREA+areaCENTRAL_SOUTH_TEXAS_AREA + 
            areaMIDWEST_AREA+areaNORTH_FLORIDA_AREA+areaNORTHWEST_ROCKY_MOUNTAIN_AREA+areaSOUTH_FLORIDA_AREA+areaSOUTHWEST_AREA+
            areaTENNESSEE_AREA+refurb_new_R + ethnic_C + ethnic_N + ethnic_O + ethnic_S + ethnic_U + 
            ethnic_Z + hnd_price_79.99 + hnd_price_105.2852 + hnd_price_129.99 + hnd_price_149.99 + 
            hnd_price_199.99 + hnd_price_249.99 + uniqsubs_2 + uniqsubs_3 + uniqsubs_4 + uniqsubs_7 +
            truck_1 + adjmou + totrev + retdays_1_1 + complete_mean  + age1_1MiddleAge +
            age1_1old + age1_1young + age2_1old,data=train,family="binomial")
summary(mod3)

#Removing var adjmou insignificant variable
mod4<-glm(churn ~ totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + drop_vce_Range +
            mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean + avgqty +
            asl_flag_Y + prizm_social_one_R + prizm_social_one_T + area_CALIFORNIA_NORTH_AREA+areaCENTRAL_SOUTH_TEXAS_AREA + 
            areaMIDWEST_AREA+areaNORTH_FLORIDA_AREA+areaNORTHWEST_ROCKY_MOUNTAIN_AREA+areaSOUTH_FLORIDA_AREA+areaSOUTHWEST_AREA+
            areaTENNESSEE_AREA+refurb_new_R + ethnic_C + ethnic_N + ethnic_O + ethnic_S + ethnic_U + 
            ethnic_Z + hnd_price_79.99 + hnd_price_105.2852 + hnd_price_129.99 + hnd_price_149.99 + 
            hnd_price_199.99 + hnd_price_249.99 + uniqsubs_2 + uniqsubs_3 + uniqsubs_4 + uniqsubs_7 +
            truck_1 + totrev + retdays_1_1 + complete_mean  + age1_1MiddleAge +
            age1_1old + age1_1young + age2_1old,data=train,family="binomial")
summary(mod4)



# Re-run model with significant variable
mod5<-glm(churn ~ totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + drop_vce_Range +
            mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean + avgqty + avg6mou+
            asl_flag_Y + prizm_social_one_R + prizm_social_one_T + area_CALIFORNIA_NORTH_AREA+areaCENTRAL_SOUTH_TEXAS_AREA + 
            areaMIDWEST_AREA+areaNORTH_FLORIDA_AREA+areaNORTHWEST_ROCKY_MOUNTAIN_AREA+areaSOUTH_FLORIDA_AREA+areaSOUTHWEST_AREA+
            areaTENNESSEE_AREA+refurb_new_R + ethnic_C + ethnic_N + ethnic_O + ethnic_S + ethnic_U + 
            ethnic_Z + hnd_price_79.99 + hnd_price_105.2852 + hnd_price_129.99 + hnd_price_149.99 + 
            hnd_price_199.99 + hnd_price_249.99 + uniqsubs_2 + uniqsubs_3 + uniqsubs_4 + uniqsubs_7 +
            totrev + retdays_1_1 + complete_mean  + age1_1MiddleAge +
            age1_1old + age1_1young + age2_1old,data=train,family="binomial")
summary(mod5)
# All the variables have come significant. 

## ***** Model Diagnostics ***** ##
# Checking For Multicollinearity
vif(mod5)
# Checking Confidence Interval
confint(mod5)  


#------------------------------------------------------------------------------------------------------------------------------------
# Step 8 -> Model Testing
#---------------------------------------------------------------------------------------------------------------------------------------

#Predicted Values ==> Predicting the probability of a customer churning.
pred<-predict(mod5, type="response", newdata=test)
head(pred)


#Assuming cut-off probablity as per the churn rate in data set
table(data1$churn)/nrow(data1)

pred1<- ifelse(pred>0.2380871,1,0)
library(irr)
kappa2(data.frame(test$churn, pred1))
library(caret)
confusionMatrix(as.factor(pred1), as.factor(test$churn), positive = "1")

#choosing cutoff value according to kappa value
s<-seq(0.2,0.5,0.01)
n<-1 
a<-as.vector(length(s))
for (i in s ) {
  
  print(i)
  test$result<-ifelse(test$pred>i,1,0)
  a[n]<-confusionMatrix(as.factor(test$result),as.factor(test$churn),positive = "1")$overall[2]
  print(a[n])
  print(n)
  n=n+1
}
max(a)
#As maximum kappa is related to cutoff 0.28 we would go with this cutoff value
pred2<-ifelse(pred>=0.28,1,0)
table(pred2)
# After several itteration in cut-off values, the model is predicting the best at the above Cut-off level.


#------------Checking Prediction Quality----------#

#Kappa Matrix
library(irr)
kappa2(data.frame(test$churn,pred2))

#Confusion Matrix
library(caret)
confusionMatrix(as.factor(pred2),as.factor(test$churn),positive = "1")
table(test$churn)

#ROCR Curve
library(ROCR)
pred3<-prediction(pred2,test$churn)
pref<-performance(pred3,"tpr","fpr")
plot(pref,col="red")
abline(0,1,lty=8,col="grey")
auc<-performance(pred3,"auc")
auc
auc<-unlist(slot(auc,"y.values"))
auc

# The auc is 0.5767956  which is more than 0.50. 
# Also the curve seems to be well above the grey line.
# So the model seems to be ok and is acceptable.


#Gains Chart
library(gains)
gains(test$churn,predict(mod5,type="response",newdata=test),groups = 10)

#the Gains Chart shows that the top 30% of the probabilities contain 42.2% customers that are likely to churn.


test$prob<-predict(mod5,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#Top 40% of the probability scores lie between 0.2505593 and 0.7358838
#We can use this probablity to extract the data of customers who are highly likely to churn.




### ********** Answering Business Questions ********** ###
### Top Line Questions of Interest to Senior Management: 

#  1.  What are the top five factors driving likelihood of churn at Mobicom?

#From the final model(mod5) top 5 factor affecting churn rate are: 
head(sort(mod5$coefficients,decreasing=T),10)

#  Variable                                  coefficient      
#  uniqsubs_7                                 0.7193651
#  retdays_1_1                                0.6711097
#  ethnic_O                                   0.3140477
#  areaNORTHWEST_ROCKY_MOUNTAIN_AREA          0.2689002
#  areaSOUTH_FLORIDA_AREA                     0.2611169


#Family bundles should be rolled out for families with 7 unique subscriber.
#Special offer should be given to customers who make retention calls, at the earliest as per their grievances.
#Special plans should be rolled out for people with Asian Ethnicity.
#Special plans should be rolled out for customers located in Northwest/Rocky Mountain area and South Florida area.


#   2.  Validation of survey findings. 
# a) Whether "cost and billing" and "network and service quality" are important factors influencing churn behaviour.  

#  The following variable explain "cost and billing":
#    totmrc_Mean - Mean total monthly recurring charge, representing cost to customer.
#  rev_Range - range of revenue(charge amount), representing billing amount
#  ovrrev_Mean -Mean overage revenue, representing overage revenue earned from customers after billing the same to them.
#  totrev - Total revenue earned from customers.
#  
# 
#  The following variable explain "network and service quality":
#    mou_Range- Range of number of minutes of use.
#  change_mou - Percentage change in monthly minutes of use vs previous three month average
#  drop_blk_Mean- Mean number of dropped or blocked calls 
#  drop_vce_Range - Range of number of dropped (failed) voice calls
#  mou_opkv_Range - Range of unrounded minutes of use of off-peak voice calls 
#  iwylis_vce_Mean- Mean number of inbound wireless to wireless voice calls 
#  avgqty - Average monthly number of calls over the life of the customer 
#  avg6mou- Average monthly minutes of use over the previous six months 
#  retdays_1_1 - Number of days since last retention call  with value more than 0
#  complete_mean - Mean number of completed calls.


# Conclusion:
# The variables which explain "cost and billing", have 0% impact on churn. So it seems "cost and billing" 
# is not very important factors here, influencing churn behavior at Mobicom.
# The Variable which explain "Network and Service quality", except retdays_1_1, all variables have 0% impact 
#  and retdays_1_1 has 67.11% impact on churn.
#  So retdays_1_1 is very important factor influencing churn rate With the increase in number of days since a customer 
#  makes a retention call, the customer's chances of churning is very high.
#  This could probably be because their grievances are not being catered to properly. 
#  These customers should be paid more attention and special offers should be made to them depending upon their grievances. 


#  2b) Are data usage connectivity issues turning out to be costly? In other words, is it leading to churn? 

#   The following variables express data usage connectivity:
#  comp_dat_mean - Mean no. of completed data calls. 
#  plcd_dat_mean - Mean number of attempted data calls placed
#  opk_dat_mean - Mean number of off-peak data calls
#  blck_dat_mean - Mean no. of blocked / failed data calls
#  datovr_Mean - Mean revenue of data overage. 
#  datovr_Range - Range of revenue of data overage
#  drop_dat_mean - Mean no. of dropped / failed data calls
#  Command:
#    quantile(data$plcd_dat_mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1),na.rm=TRUE)
#  Similarly all variables can be processed to create quantile.

# Conclusion:
#  The Data Quality Report for all the above variables show that only 10% to 15% customers are actually making data calls or 
#  using the internet. This could be a matter of concern since the global market survey report shows "Subscribers
#  who have switched operators in recent months reported two key information sources in their decision
#  -> The Internet 
#  -> Recommendation of family and friends.

#  In this case it seems customers are not really using the internet. 
#  So it would be good to work towards attaining more customers to use data and also towards proving quality network connectivity
#  and service for maximum customer satisfaction and reduce Churn. 
#  Since there is not enough usable data for the above variables they are not showing any influence on the Churn behavior at Mobicom.


#   3. Would you recommend rate plan migration as a proactive retention strategy?

#  Variable ovrrev_mean has beta coefficient of 0.00708352. 
#  var ovrrev_mean = DATOVR_MEAN + VCEOVR_MEAN i.e. 'Mean overage revenue' 
#  It is the sum of data and voice overage revenues representing the overage revenue earned from customers after billing the same to them. 
#  The Beta coefficient is not showing a strong impact of overage billing as an influencer of churn behavior. 
#  This might be a matter of concern for few individual customers and they could be catered to on case to case basis. 
#  But overall rate plan migration as a proactive retention strategy might not help much at Mobicom.


#   4. What would be your recommendation on how to use this churn model for prioritisation
#   of customers for a proactive retention campaigns in the future?

# Solution:
#Gains Chart
library(gains)
gains(test$churn,predict(mod5,type="response",newdata=test),groups = 10)
#the Gains Chart shows that the top 20% of the probabilities contain 29.3% customers that are highly likely to churn.


# Selecting Customers with high churn rate
test$prob<-predict(mod5,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

# Top 20% of the probabilities lie between 0.3036880 and 0.7358838

# Applying cutoff value to predict customers who Will Churn
pred4<-predict(mod5, type="response", newdata=test)
pred4<-ifelse(pred4>=0.3036880 , 1, 0)
table(pred4,test$churn)

Targeted<-test[test$prob>0.3036880 & test$prob<=0.7358838 & test$churn=="1","Customer_ID"]
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
pred5<-predict(mod5, type="response", newdata=test)
test$prob<-predict(mod5,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
pred6<-ifelse(pred5<0.20,"Low_Score",ifelse(pred5>=0.20 & pred5<0.30,"Medium_Score","High_Score"))
table(pred6,test$churn)

str(test$totrev)
quantile(test$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
Revenue_Levels<-ifelse(test$totrev<670.660,"Low_Revenue",ifelse(test$totrev>=670.660 & 
                                                                  test$totrev<1034.281,"Medium_Revenue","High_Revenue"))

table(Revenue_Levels)

table(pred6,Revenue_Levels)

##  Thus this table can be used to select the levels of customers are to be targeted
##  and the Target list can be extracted as follows:

test$prob_levels<-ifelse(pred5<0.20,"Low_Score",ifelse(pred5>=0.20 & pred5<0.30,"Medium_Score","High_Score"))
test$Revenue_Levels<-ifelse(test$totrev<670.660,"Low_Revenue",ifelse(test$totrev>=670.660 & 
                                                                       test$totrev<1034.281,"Medium_Revenue","High_Revenue"))

Targeted1<-test[test$prob_levels=="High_Score" & test$Revenue_Levels=="High_Revenue","Customer_ID"]
Targeted1<-as.data.frame(Targeted1)
nrow(Targeted1)

write.csv(Targeted1,"High_Revenue_Target_Customers.csv",row.names = F)





