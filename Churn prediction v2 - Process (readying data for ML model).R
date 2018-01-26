setwd ("C:/Users/ChetanBhat/Dropbox/BI Shared Folder/Chetan/Churn Prediction v2") #set working directory

library(RMySQL)
library(stringr)
library(ggplot2)
library(xts)
library(lubridate)
library(RGoogleAnalytics)
library(plyr)
library(zoo)
library(reshape2)
library(reshape)
library(matrixStats)

###################################### CREATE FINAL FILES TO BE FED INTO ML MODELS #############################################

###################################### Most important step. Choose respective dates for analysis ###############################

final_date <- "2016-08-31" #should include at least one month of data for creating hold out dataset

################################################################################################################################

#------------------------------------- Readying subscriptions data based on date provided --------------------------------------

#Churn / Renewal date and flag
temp <- aggregate(end_date~ray_practice_id,max,data = subset(Subs,Subs$end_date<=final_date & Subs$status != 'REVOKED'))
Subs$crdate <- temp$end_date[match(Subs$ray_practice_id,temp$ray_practice_id)] 
Subs$crdateflag <- ifelse(Subs$crdate==Subs$end_date,1,0)

# start date of next plan
temp <- aggregate(start_date~ray_practice_id,min,data = subset(Subs,Subs$end_date>final_date & Subs$status != 'REVOKED'))
Subs$startdatenextplan <- temp$start_date[match(Subs$ray_practice_id,temp$ray_practice_id)] 
Subs$daysnextplan <- Subs$startdatenextplan - Subs$crdate
Subs$daysnextplan[is.na(Subs$daysnextplan)] <- -10000

#Churn flag 

## Churn based on expired subscription status OR more than 45 days to next plan
Subs$churn_a <- ifelse(Subs$subscriptionstatus == 'ACTIVE'& Subs$daysnextplan < 45, 0, 1)

## Some tab plans can churn while still continuing with ray (TabRay#1,2,3 plans) and thus have an active subscription flag. Identify those.
temp <- subset(Subs, crdateflag == 1)
Subs$planatcrdate <- temp$plan[match(Subs$ray_practice_id, temp$ray_practice_id)]
Subs$tabcrdateflag <-ifelse(grepl("Tab", Subs$planatcrdate),1,0) # if plan at the time of renewal is tab or not
temp <- aggregate(plan~ray_practice_id, data = subset(Subs, end_date > final_date & (grepl("Tab", plan) | grepl("360", plan))), FUN = length) 
Subs$tabaftercrdateflag <- temp$plan[match(Subs$ray_practice_id, temp$ray_practice_id)]
Subs$tabaftercrdateflag <- ifelse(is.na(Subs$tabaftercrdateflag), 0, 1) # if there is any plan after current date that is tab or not
Subs$churn_b <- ifelse(Subs$tabcrdateflag == 1 & Subs$tabaftercrdateflag == 0, 1, 0)

## Final flag
Subs$churn <- ifelse(Subs$churn_b == 1, 1, Subs$churn_a)

# #Churn re-engineered flags (NOT IMPORTANT)
# temp <- aggregate(end_date~ray_practice_id,max,data = subset(Subs,status=='ACTIVE'))
# Subs$edactive <- temp$end_date[match(Subs$ray_practice_id,temp$ray_practice_id)]
# Subs$churn2 <- ifelse(Subs$edactive>"2016-07-01", 0,1) 

# Whether renewal is for tab subscription or not
Subs$tab <- ifelse(grepl("Tab",Subs$plan),1,0)

# Whether the practice was an active Tab customer at the time of renewal
temp <- aggregate(end_date~ray_practice_id, max, data = subset(Subs, end_date > crdate & start_date < crdate & status =='ACTIVE' & grepl("Tab",Subs$plan) == TRUE))
Subs$tabC <- ifelse(is.na(temp$end_date[match(Subs$ray_practice_id,temp$ray_practice_id)]),0,1)

#Multi-year subscription or not
Subs$dur <- difftime(Subs$end_date,Subs$start_date, units = "days")
Subs$multiyr <- ifelse(difftime(Subs$end_date,Subs$start_date, units = "days")>=728,1,0) 

#Number of renewals done before
temp <- subset(Subs,end_date <= final_date & dur>=180 & status=='ACTIVE')
temp <- aggregate(start_date~ray_practice_id, data=temp, FUN=length)
Subs$nrenewals <- temp$start_date[match(Subs$ray_practice_id,temp$ray_practice_id)] 
Subs$nrenewals[is.na(Subs$nrenewals)] <- 0

write.csv(Subs, "Subs.csv")

# #create active and churned customer db
# temp <- aggregate(end_date~ray_practice_id, data=subset(Subs, dur>=179), FUN=max)
# Subs$latestplanenddate <- temp$end_date[match(Subs$ray_practice_id, temp$ray_practice_id)]
# Subs$latestplanflag <- ifelse(Subs$end_date == Subs$latestplanenddate, 1, 0)
# Actcustomerdb <- subset(Subs, subscriptionstatus == 'ACTIVE' & latestplanflag == 1)
# Actcustomerdb <- Actcustomerdb[, c(1:3, 6:8)]
# Actcustomerdb <- unique(Actcustomerdb)
# Expcustomerdb <- subset(Subs, subscriptionstatus == 'EXPIRED')
# Expcustomerdb <- Expcustomerdb[, c(1:3, 6:8)]
# Expcustomerdb <- unique(Expcustomerdb)

### NEED TO SOLVE FOR DUPLICATES IN DATA

#Final data set for model
SubsF <- unique(subset(Subs, crdateflag == 1))
#Some Ray Practice Ids appear twice due to two entries for churn / renewal date. Removing them. Need to think of a solution later
temp <- aggregate(start_date~ray_practice_id, data=SubsF, FUN=length)
SubsF$freq <- temp$start_date[match(SubsF$ray_practice_id,temp$ray_practice_id)]
SubsF <- unique(subset(SubsF, freq == 1))
SubsF <- SubsF[,c("ray_practice_id","churn", "crdate","tab","tabC","multiyr","nrenewals")]
SubsF <- unique(SubsF)

#Adding analysis set column
# SubsF$analysisset <- ifelse(SubsF$crdate >= as.Date(analysis_start_date, format="%Y-%m-%d") & SubsF$crdate <= as.Date(analysis_end_date, format="%Y-%m-%d"),1,0) 
# SubsF$holdoutset <- ifelse(SubsF$crdate >= as.Date(holdout_start_date, format="%Y-%m-%d") & SubsF$crdate <= as.Date(holdout_end_date, format="%Y-%m-%d"),1,0) 

write.csv(SubsF, "./Model/SubsF.csv")

#------------------------------------- Readying final usage data -------------------------------------------------------------

#Pivot up raw usage data and retain only 12 months of usage upto renewal

Usagetablename <- data.frame(tablename = c("Cal", "Apptfiles", "Presc", "Inv", "Ptntfiles", "Pymts", "Notes","Growth", "Vsigns", "Imm", "SMS", "TFb", "TAppts", "TReg", "TApptfiles", "TPresc", "TNotes", "CalABS"))

Usagetables <- list()
Usagetables[[1]] <- Cal
Usagetables[[2]] <- Apptfiles
Usagetables[[3]] <- Presc
Usagetables[[4]] <- Inv
Usagetables[[5]] <- Ptntfiles
Usagetables[[6]] <- Pymts
Usagetables[[7]] <- Notes
Usagetables[[8]] <- Growth
Usagetables[[9]] <- Vsigns
Usagetables[[10]] <- Imm
Usagetables[[11]] <- SMS
Usagetables[[12]] <- TFb
Usagetables[[13]] <- TAppts
Usagetables[[14]] <- TReg
Usagetables[[15]] <- TApptfiles
Usagetables[[16]] <- TPresc
Usagetables[[17]] <- TNotes
Usagetables[[18]] <- CalABS

i=1

for (i in 1:length(Usagetables))
{
  #Subsetting only months on or before renewal month
  temp <- merge(x = Usagetables[[i]], y = SubsF, by = "ray_practice_id", all.x = T, all.y = F)
  temp$date <- paste(temp$date, "01", sep = '-')
  temp$date <- as.Date(temp$date)
  temp <- subset(temp, substr(date, 1, 7) <= substr(crdate, 1, 7))
  
  #Calculating number of months from renewal month
  temp$ed <- as.POSIXlt(temp$crdate)
  temp$sd <- as.POSIXlt(temp$date)
  temp$NMonths <- 12 * (temp$ed$year - temp$sd$year) + (temp$ed$mon - temp$sd$mon)
  
  #Taking only 12 months including renewal month and pivoting up
  temp <- subset(temp, NMonths < 12)
  temp$mon <- paste("M", temp$NMonths, sep='')
  temp <- temp[, c("ray_practice_id", "churn", "crdate", "mon", "count")]
  temp$ray_practice_id <- as.character(temp$ray_practice_id)
  temp$churn <- as.character(temp$churn)
  temp$crdate <- as.character(temp$crdate)
  temp$mon <- as.character(temp$mon)
  temp <- dcast(melt(temp), ray_practice_id + churn + crdate ~ mon + variable)
  temp <- temp[,c(1:5, 8:15, 6:7)]
  temp[is.na(temp)] <- 0
  assign(paste0(Usagetablename[i,1],"_DN"),temp)
  print(paste0(Usagetablename[i,1],"_DN done"))
}

# Create average usage columns and other useful columns for the model. 1st and 12th months are removed from average
Cal_DN$avgcalmanual <- rowMeans(subset(Cal_DN, select=c(5:14)), na.rm=FALSE)
CalABS_DN$avgcalABS <- rowMeans(subset(CalABS_DN, select=c(5:14)), na.rm=FALSE)
Inv_DN$avginv <- rowMeans(subset(Inv_DN, select=c(5:14)), na.rm=FALSE)
Pymts_DN$avgpymts <- rowMeans(subset(Pymts_DN, select=c(5:14)), na.rm=FALSE)
Apptfiles_DN$avgapptfiles <- rowMeans(subset(Apptfiles_DN, select=c(5:14)), na.rm=FALSE)
Ptntfiles_DN$avgptntfiles <- rowMeans(subset(Ptntfiles_DN, select=c(5:14)), na.rm=FALSE)
Notes_DN$avgnotes <- rowMeans(subset(Notes_DN, select=c(5:14)), na.rm=FALSE)
Presc_DN$avgpresc <- rowMeans(subset(Presc_DN, select=c(5:14)), na.rm=FALSE)

Cal_DN$nzeros <- rowSums(subset(Cal_DN, select=c(5:14))==0)
Cal_DN$dimusageflag <- ifelse((rowMeans(subset(Cal_DN, select=c(10:14))) - rowMeans(subset(Cal_DN, select=c(5:9))))>0, 1, 0)
Cal_DN$var <- data.frame(apply(subset(Cal_DN, select=c(5:14)), 1, var))
colnames(Cal_DN[,19]) <- "var"
Cal_DN$varbymean <- Cal_DN$var/Cal_DN$avgcal

# merge all tables into one
Cal_DN_F <- Cal_DN[-c(4:15)]
CalABS_DN_F <- CalABS_DN[-c(4:15)]
Inv_DN_F <- Inv_DN[-c(4:15)]
Pymts_DN_F <- Pymts_DN[-c(4:15)]
Apptfiles_DN_F <- Apptfiles_DN[-c(4:15)]
Ptntfiles_DN_F <- Ptntfiles_DN[-c(4:15)]
Notes_DN_F <- Notes_DN[-c(4:15)]
Presc_DN_F <- Presc_DN[-c(4:15)]

datlist <- list()
datlist[[1]] <- Cal_DN_F
datlist[[2]] <- Inv_DN_F
datlist[[3]] <- Pymts_DN_F
datlist[[4]] <- Apptfiles_DN_F
datlist[[5]] <- Ptntfiles_DN_F
datlist[[6]] <- Notes_DN_F
datlist[[7]] <- Presc_DN_F
datlist[[8]] <- CalABS_DN_F

Usage_F<-Reduce(function(x,y) {merge(x,y,by=intersect(names(x),names(y)),all.x=TRUE,all.y=TRUE)}, datlist)

# Replace usage NAs with 0
Usage_F$avginv[is.na(Usage_F$avginv)] <- 0
Usage_F$avgpymts[is.na(Usage_F$avgpymts)] <- 0
Usage_F$avgcalmanual[is.na(Usage_F$avgcalmanual)] <- 0
Usage_F$avgapptfiles[is.na(Usage_F$avgapptfiles)] <- 0
Usage_F$avgptntfiles[is.na(Usage_F$avgptntfiles)] <- 0
Usage_F$avgnotes[is.na(Usage_F$avgnotes)] <- 0
Usage_F$avgpresc[is.na(Usage_F$avgpresc)] <- 0
Usage_F$avgcalABS[is.na(Usage_F$avgcalABS)] <- 0

Usage_F$avgcal <- Usage_F$avgcalmanual + Usage_F$avgcalABS
Usage_F$nfeatures <- rowSums(subset(Usage_F, select = c(4,9:14))>=5)
Usage_F$perccalmanual <- Usage_F$avgcalmanual / Usage_F$avgcal

# Replace other NAs
Usage_F$nzeros[is.na(Usage_F$nzeros)] <- 10
Usage_F$dimusageflag[is.na(Usage_F$dimusageflag)] <- 0
Usage_F$var[is.na(Usage_F$var)] <- 0
Usage_F$varbymean[is.na(Usage_F$varbymean)] <- 0
Usage_F$perccalmanual[is.na(Usage_F$perccalmanual)] <- 0

write.csv(Usage_F,"./Model/Usage_F.csv")

#### creating tab usage data for model
TFb_DN$avgTFb <- rowMeans(subset(TFb_DN, select=c(5:14)), na.rm=FALSE)
TReg_DN$avgTReg <- rowMeans(subset(TReg_DN, select=c(5:14)), na.rm=FALSE)
TFb_DN_F <- TFb_DN[-c(4:15)]
TReg_DN_F <- TReg_DN[-c(4:15)]

temp <- merge(x=TFb_DN_F, y=TReg_DN_F, by = c("ray_practice_id", "churn", "crdate"), all.x=T, all.y=T)
temp[is.na(temp)] <- 0

Tab_Usage_F <- merge(x=SubsF, y=temp, by = c("ray_practice_id", "churn", "crdate"), all.x=T)
Tab_Usage_F[is.na(Tab_Usage_F)] <- 0
Tab_Usage_F$tab_noncust <- ifelse(Tab_Usage_F$tab == 0,1,0) 
Tab_Usage_F$tab_Fbuser <- ifelse(Tab_Usage_F$avgTFb >=3,1,0)
Tab_Usage_F$tab_Reguser <- ifelse(Tab_Usage_F$avgTReg >=3,1,0)
Tab_Usage_F$tab_user <- ifelse(Tab_Usage_F$avgTReg >= 3 | Tab_Usage_F$avgTFb >= 3, 1, 0)
Tab_Usage_F$tab_nonuser <- ifelse(Tab_Usage_F$avgTReg < 3 & Tab_Usage_F$avgTFb < 3, 1, 0)
Tab_Usage_F <- Tab_Usage_F[, c("ray_practice_id", "churn", "crdate", "tab_noncust", "tab_Fbuser", "tab_Reguser","tab_user", "tab_nonuser")] 

write.csv(Tab_Usage_F,"./Model/Tab_Usage_F.csv")

#------------------------------------- Readying final Issues data -------------------------------------------------------------

Issues_F <- merge(x=SubsF, y=IssuesI.2, by = "ray_practice_id", all.x = T)
Issues_F$Nmonths <- (as.yearmon(strptime(Issues_F$crdate, format="%Y-%m-%d")) - as.yearmon(strptime(as.POSIXct(paste(Issues_F$created_on,"-01",sep=""),format="%Y-%m-%d"),format="%Y-%m-%d")))*12
Issues_F <- subset(Issues_F, Nmonths < 12 & Nmonths >= 0) #Taking only 12 months of issues from renewal date
Issues_F[is.na(Issues_F)] <- 0
temp <- aggregate(time~ray_practice_id+churn+crdate, data = Issues_F, FUN = mean) # mean issue resolution time
Issues_F <- aggregate(cbind(Totalissues,othersP,ray,search,tab.y,generic,othersT,profile,training,vn,call,chat,email,fabric_website,feedback,othersS,staff,angry,happy,neutral,unhappy)~ray_practice_id+churn+crdate, data=Issues_F, FUN=sum)
Issues_F <- merge(x=Issues_F, y=temp, by=c("ray_practice_id", "churn", "crdate"), all.x = T)

#Create columns in percentages
Issues_F$percothersP <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$othersP/Issues_F$Totalissues)
Issues_F$percray <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$ray/Issues_F$Totalissues)
Issues_F$percsearch <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$search/Issues_F$Totalissues)
Issues_F$perctab.y <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$tab.y/Issues_F$Totalissues)
Issues_F$percgeneric <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$generic/Issues_F$Totalissues)
Issues_F$percothersT <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$othersT/Issues_F$Totalissues)
Issues_F$percprofile <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$profile/Issues_F$Totalissues)
Issues_F$perctraining <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$training/Issues_F$Totalissues)
Issues_F$percvn <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$vn/Issues_F$Totalissues)
Issues_F$perccall <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$call/Issues_F$Totalissues)
Issues_F$percchat <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$chat/Issues_F$Totalissues)
Issues_F$percemail <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$email/Issues_F$Totalissues)
Issues_F$percfabric_website <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$fabric_website/Issues_F$Totalissues)
Issues_F$percfeedback <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$feedback/Issues_F$Totalissues)
Issues_F$percothersS <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$othersS/Issues_F$Totalissues)
Issues_F$percstaff <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$staff/Issues_F$Totalissues)
Issues_F$percangry <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$angry/Issues_F$Totalissues)
Issues_F$perchappy <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$happy/Issues_F$Totalissues)
Issues_F$percneutral <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$neutral/Issues_F$Totalissues)
Issues_F$percunhappy <- ifelse(is.na(Issues_F$Totalissues), 0, Issues_F$unhappy/Issues_F$Totalissues)

# Drop raw columns
Issues_F$othersP <- NULL
Issues_F$ray <- NULL
Issues_F$search <- NULL
Issues_F$tab.y <- NULL
Issues_F$generic <- NULL
Issues_F$othersT <- NULL
Issues_F$profile <- NULL
Issues_F$training <- NULL
Issues_F$vn <- NULL
Issues_F$call <- NULL
Issues_F$chat <- NULL
Issues_F$email <- NULL
Issues_F$fabric_website <- NULL
Issues_F$feedback <- NULL
Issues_F$othersS <- NULL
Issues_F$staff <- NULL
Issues_F$angry <- NULL
Issues_F$happy <- NULL
Issues_F$neutral <- NULL
Issues_F$unhappy <- NULL

write.csv(Issues_F, "./Model/Issues_F.csv")

#------------------------------------- Readying final Reach data -------------------------------------------------------------

#Adding some important columns and creating an intermediate table
Reachraw$ray_practice_id <- as.character(Reachraw$ray_practice_id)
Reachraw$subscription_end <- as.Date(Reachraw$subscription_end, format="%Y-%m-%d")
Reachraw$subscription_start <- as.Date(Reachraw$subscription_start, format="%Y-%m-%d")
Reachraw$deleted_at <- as.Date(Reachraw$deleted_at, format="%Y-%m-%d")
Reachraw$subsdur <- difftime(Reachraw$subscription_end, Reachraw$subscription_start, unit="days")
Reachraw$validsubsflag <- ifelse(Reachraw$subsdur>=89 & (Reachraw$subscription_end <= Reachraw$deleted_at | is.na(Reachraw$deleted_at)), 1, 0)
write.csv(Reachraw,"All time reach subscriptions.csv")

ReachI <- merge(x=SubsF, y=Reachraw, by="ray_practice_id", all.x=T)
ReachI$reach <- ifelse(ReachI$subscription_start < ReachI$crdate & ReachI$subscription_end > ReachI$crdate & ReachI$validsubsflag ==1,1,0)
ReachI$reach[is.na(ReachI$reach)] <-0
ReachI$reachchurned <- ifelse(ReachI$subscription_end > ReachI$start_date & ReachI$subscription_end < ReachI$crdate & ReachI$validsubsflag ==1, 1, ifelse(ReachI$reach==1, 2, 0))
ReachI$reachchurned[is.na(ReachI$reachchurned)] <-0

#Reach customer and no. of active slots held flag
temp <- aggregate(reach~ray_practice_id+churn+crdate,data=ReachI, FUN=sum)
Reach_F <- temp
colnames(Reach_F)[colnames(Reach_F)=='reach'] <- 'nslotsatrenewal'
Reach_F$reachflag <- ifelse(Reach_F$nslotsatrenewal > 0, 1, 0)

#Reach churned customer during Ray subscription period
temp <- aggregate(reachchurned~ray_practice_id+churn+crdate, data=ReachI, FUN=max)
temp <- subset(temp, reachchurned ==1)
Reach_F$reachchurnedflag <- temp$reachchurned[match(Reach_F$ray_practice_id, temp$ray_practice_id)]
Reach_F$reachchurnedflag[is.na(Reach_F$reachchurnedflag)] <- 0

#Number of days from reach churn to ray date of renewal
temp1 <- aggregate(subscription_end~ray_practice_id+churn+crdate, data=subset(ReachI, reachchurned == 1), FUN=max)
temp$reachchurneddate <- temp1$subscription_end[match(temp$ray_practice_id, temp1$ray_practice_id)]
temp$reachchurneddays <- difftime(temp$crdate, temp$reachchurneddate, unit='days')
Reach_F$reachchurneddays <- temp$reachchurneddays[match(Reach_F$ray_practice_id, temp$ray_practice_id)]
Reach_F$reachchurneddays[is.na(Reach_F$reachchurneddays)] <- 10000

#Number of slots bought+renewed until churn date (indicator of how much the doctor had invested in reach till the point of ray churn)
temp <- aggregate(validsubsflag~ray_practice_id+churn+crdate, data = subset(ReachI, subscription_end < crdate), FUN=sum)
Reach_F$nrenewals <- temp$validsubsflag[match(Reach_F$ray_practice_id, temp$ray_practice_id)]
Reach_F$nrenewals[is.na(Reach_F$nrenewals)] <- 0

#Total duration in months where customer was an active reach customer
temp <- subset(ReachI, validsubsflag==1 & subscription_end < crdate)
temp <- temp[,c("ray_practice_id", "crdate", "subscription_start", "subscription_end")]
temp_min <- aggregate(subscription_start~ray_practice_id+crdate, data=temp, FUN=min)
temp_max <- aggregate(subscription_end~ray_practice_id, data=temp, FUN=max)
temp1 <- merge(x=temp_min, y=temp_max, by="ray_practice_id", all.x=T)

i=1
base <- data.frame()
for (i in 1:nrow(temp)){
  df <- data.frame(months=seq(as.Date(paste0(format(temp[i,3], "%Y-%m"),"-01")), as.Date(paste0(format(temp[i,4], "%Y-%m"),"-01")), by="month"))
  df$ray_practice_id <- temp[i,1] 
  base <- rbind(base, df)
  paste(i)
}

base <- unique(base)
base <- aggregate(months~ray_practice_id, data=base, FUN=length)
Reach_F$durationonreach <- base$months[match(Reach_F$ray_practice_id,base$ray_practice_id)]
Reach_F$durationonreach[is.na(Reach_F$durationonreach)] <- 0

write.csv(Reach_F, "./Model/Reach_F.csv")

#------------------------------------- Readying final Profile data -------------------------------------------------------------

# Retain only those practices present in subscriptions table
Profiles_I.3 <- merge(x=SubsF, y=Profiles_I.1.2, by = "ray_practice_id", all.X = T)

#Get DQS
temp <- aggregate(dqs~ray_practice_id, dDQS, mean)
Profiles_I.3$ddqs <- temp$dqs[match(Profiles_I.3$ray_practice_id,temp$ray_practice_id)]
Profiles_I.3$pdqs <- pDQS$pdqs[match(Profiles_I.3$ray_practice_id,pDQS$ray_practice_id)]/100

#Get ABS status and ABS deactivation date
ABS$ABS_deact_date <- as.Date(ifelse(is.na(ABS$month_deleted),"", ifelse(ABS$month_deleted>9,paste(ABS$year_deleted,"-",ABS$month_deleted,"-01",sep=""),paste(ABS$year_deleted,"-0",ABS$month_deleted,"-01",sep=""))), format = "%Y-%m-%d")
temp <- aggregate(pdp_id~ray_practice_id, data = subset(ABS, is.na(ABS$ABS_deact_date)), FUN = length) #practices with at least one NA entry is ABS enabled
temp$ABS <- 1
Profiles_I.3$ABS <- temp$ABS[match(Profiles_I.3$ray_practice_id, temp$ray_practice_id)]
temp <- aggregate(ABS_deact_date~ray_practice_id, data = ABS, FUN = max)
Profiles_I.3$ABS_deact_date <- temp$ABS_deact_date[match(Profiles_I.3$ray_practice_id,temp$ray_practice_id)] #other practices with max ABS deactivation date. If this is before renewal date, then ABS deactivated
Profiles_I.3$ABS2 <- ifelse((Profiles_I.3$ABS_deact_date < Profiles_I.3$crdate) & is.na(Profiles_I.3$ABS), 0, ifelse((Profiles_I.3$ABS_deact_date > Profiles_I.3$crdate) & is.na(Profiles_I.3$ABS), 1, Profiles_I.3$ABS))

# Sum of appointments, calls, reviews, recommendations
temp <- aggregate(cbind(appts,calls,doctor_reviews,recommendations)~ray_practice_id, data = Profiles_I.3, sum)

# Subsetting data only to pick last 12 months from churn
Profiles_I.3$Nmonths <- (as.yearmon(strptime(Profiles_I.3$crdate, format="%Y-%m-%d")) - as.yearmon(strptime(as.POSIXct(paste(Profiles_I.3$date,"-01",sep=""),format="%Y-%m-%d"),format="%Y-%m-%d")))*12
Profiles_I.3 <- subset(Profiles_I.3, Profiles_I.3$Nmonths <=12 & Profiles_I.3$Nmonths >=0)
temp1 <- aggregate(Nmonths~ray_practice_id, data=Profiles_I.3, min)
Profiles_F <- merge(x=Profiles_I.3, y=temp1, by = c("ray_practice_id", "Nmonths"), all.x=F, all.y=F)
Profiles_F <- Profiles_F[,c("ray_practice_id", "churn", "crdate", "ABS2", "fees_mean", "fees_max", "exp_mean", "exp_max", "exp_min", "number_of_photos", "dphotos_mean", "dphotos_sum", "logo_presence", "ddqs", "pdqs")]

Profiles_F <- merge(x=Profiles_F, y=temp, by = "ray_practice_id", all.x=T)
Profiles_F$number_of_photos[is.na(Profiles_F$number_of_photos)] <- 0
Profiles_F$dphotos_mean[is.na(Profiles_F$dphotos_mean)] <- 0
Profiles_F$dphotos_sum[is.na(Profiles_F$dphotos_sum)] <- 0
Profiles_F$logo_presence[is.na(Profiles_F$logo_presence)] <- 0
Profiles_F$ddqs[is.na(Profiles_F$ddqs)] <- 0
Profiles_F$pdqs[is.na(Profiles_F$pdqs)] <- 0
Profiles_F$appts[is.na(Profiles_F$appts)] <- 0
Profiles_F$calls[is.na(Profiles_F$calls)] <- 0
Profiles_F$doctor_reviews[is.na(Profiles_F$doctor_reviews)] <- 0
Profiles_F$recommendations[is.na(Profiles_F$recommendations)] <- 0

write.csv(Profiles_F, "./Model/Profiles_F.csv")

## To dos on profile
# Remove outlier entries
# ABS flag check
# Use information on qualification, registration etc

#------------------------------------- Readying Final consolidated data -------------------------------------------------------

# Load individual final data sets
Usage_F_R <- read.csv("./model/Usage_F.csv", stringsAsFactors = FALSE)
Subs_F_R <- read.csv("./model/SubsF.csv", stringsAsFactors = FALSE)
Tab_Usage_F_R <- read.csv("./model/Tab_Usage_F.csv", stringsAsFactors = FALSE)
Profiles_F_R <- read.csv("./model/Profiles_F.csv", stringsAsFactors = FALSE)
Reach_F_R <- read.csv("./model/Reach_F.csv", stringsAsFactors = FALSE)
Issues_F_R <- read.csv("./model/Issues_F.csv", stringsAsFactors = FALSE)

# Drop column X
Usage_F_R$X <- NULL
Subs_F_R$X <- NULL
Tab_Usage_F_R$X <- NULL
Profiles_F_R$X <- NULL
Reach_F_R$X <- NULL
Issues_F_R$X <- NULL

# CONSOLIDATE ALL

# Usage and subscriptions
dat <- list ()
dat[[1]] <- Usage_F_R
dat[[2]] <- Subs_F_R
dat[[3]] <- Tab_Usage_F_R

All_F<-Reduce(function(x,y) {merge(x,y,by=intersect(names(x),names(y)),all.x=TRUE,all.y=TRUE)}, dat)
All_F$ray_practice_id <- as.character(All_F$ray_practice_id)

# replace NAs in usage with zero
All_F$avginv[is.na(All_F$avginv)] <- 0
All_F$avgpymts[is.na(All_F$avgpymts)] <- 0
All_F$avgapptfiles[is.na(All_F$avgapptfiles)] <- 0
All_F$avgptntfiles[is.na(All_F$avgptntfiles)] <- 0
All_F$avgpresc[is.na(All_F$avgpresc)] <- 0
All_F$avgnotes[is.na(All_F$avgnotes)] <- 0
All_F$avgcalmanual[is.na(All_F$avgcalmanual)] <- 0
All_F$avgcalABS[is.na(All_F$avgcalABS)] <- 0
All_F$avgcal[is.na(All_F$avgcal)] <- 0
All_F$perccalmanual[is.na(All_F$perccalmanual)] <- 0
All_F$var[is.na(All_F$var)] <- 0
All_F$varbymean[is.na(All_F$varbymean)] <- 0
All_F$dimusageflag[is.na(All_F$dimusageflag)] <- 0
All_F$nfeatures[is.na(All_F$nfeatures)] <- 0
All_F$nzeros[is.na(All_F$nzeros)] <- 10

# merging with profiles
All_F <- merge(x=All_F, y=Profiles_F_R, by=c("ray_practice_id","churn", "crdate"), all.x=T)

# drop columns with poor data completion
All_F$fees_mean <- NULL 
All_F$exp_mean <- NULL
All_F$fees_max <- NULL
All_F$exp_max <- NULL
All_F$exp_min <- NULL

# drop ABS2 column as something is wrong with the variable
All_F$ABS2 <- NULL 

# replace all other nulls in profile with zero
All_F[is.na(All_F)] <- 0

# merging with reach
All_F <- merge(x=All_F, y=Reach_F_R, by=c("ray_practice_id","churn", "crdate"), all.x=T)

# merging with issues
All_F <- merge(x=All_F, y=Issues_F_R, by=c("ray_practice_id","churn", "crdate"), all.x=T)
All_F[is.na(All_F)] <- 0

################################################## CREATING SOME NEW VARIABLES ########################################3

#a. # sudden drop in calendar usage (first months and throughout) b. # zeroes in first 3 months
#c1. Diminishing billing usage c2. Variance in billing usage c3. Number of zeroes in billing usage

# a and b
Cal_DN$zeroesF3 <- rowSums(subset(Cal_DN, select = c(13:15)) == 0)

usagedrop <- function(a, b)
{
  if(a==0 | b==0)
  {
    0
  }else if(a<=0.5*b)
  {
    1
  } else {0}
}

Cal_DN$dropsF12 <- 0

for(i in 1:11)
{
  Cal_DN$temp <- apply(Cal_DN[,c(15-i,16-i)], 1, function(x) usagedrop(x[1],x[2]))
  Cal_DN$dropsF12 <- Cal_DN$dropsF12 + Cal_DN$temp
}

Cal_DN$dropsF4 <- 0

for(i in 1:3)
{
  Cal_DN$temp <- apply(Cal_DN[,c(15-i,16-i)], 1, function(x) usagedrop(x[1],x[2]))
  Cal_DN$dropsF4 <- Cal_DN$dropsF4 + Cal_DN$temp
}

Cal_DN$temp <- NULL

All_F$zeroesF3 <- Cal_DN$zeroesF3[match(All_F$ray_practice_id, Cal_DN$ray_practice_id)]
All_F$dropsF4 <- Cal_DN$dropsF4[match(All_F$ray_practice_id, Cal_DN$ray_practice_id)]
All_F$dropsF12 <- Cal_DN$dropsF12[match(All_F$ray_practice_id, Cal_DN$ray_practice_id)]
All_F$zeroesF3[is.na(All_F$zeroesF3)] <- 3
All_F$dropsF4[is.na(All_F$dropsF4)] <- 0
All_F$dropsF12[is.na(All_F$dropsF12)] <- 0

# c1-c3: Create diminishing usage, variance in usage and number of zeroes columns for invoices and payments data
Inv_DN$invnzeros <- rowSums(subset(Inv_DN, select=c(5:14))==0)
Inv_DN$invdimusageflag <- ifelse((rowMeans(subset(Inv_DN, select=c(10:14))) - rowMeans(subset(Inv_DN, select=c(5:9))))>0, 1, 0)
Inv_DN$var <- data.frame(apply(subset(Inv_DN, select=c(5:14)), 1, var))
colnames(Inv_DN[,19]) <- "var"
Inv_DN$invvarbymean <- Inv_DN$var/Inv_DN$avginv

Pymts_DN$pymtsnzeros <- rowSums(subset(Pymts_DN, select=c(5:14))==0)
Pymts_DN$pymtsdimusageflag <- ifelse((rowMeans(subset(Pymts_DN, select=c(10:14))) - rowMeans(subset(Pymts_DN, select=c(5:9))))>0, 1, 0)
Pymts_DN$var <- data.frame(apply(subset(Pymts_DN, select=c(5:14)), 1, var))
colnames(Pymts_DN[,19]) <- "var"
Pymts_DN$pymtsvarbymean <- Pymts_DN$var/Pymts_DN$avgpymts

All_F$invnzeros <- Inv_DN$invnzeros[match(All_F$ray_practice_id, Inv_DN$ray_practice_id)]
All_F$invdimusageflag <- Inv_DN$invdimusageflag[match(All_F$ray_practice_id, Inv_DN$ray_practice_id)]
All_F$invvarbymean <- Inv_DN$invvarbymean[match(All_F$ray_practice_id, Inv_DN$ray_practice_id)]
All_F$pymtsnzeros <- Pymts_DN$pymtsnzeros[match(All_F$ray_practice_id, Pymts_DN$ray_practice_id)]
All_F$pymtsdimusageflag <- Pymts_DN$pymtsdimusageflag[match(All_F$ray_practice_id, Pymts_DN$ray_practice_id)]
All_F$pymtsvarbymean <- Pymts_DN$pymtsvarbymean[match(All_F$ray_practice_id, Pymts_DN$ray_practice_id)]

All_F$invnzeros[is.na(All_F$invnzeros)] <- 10
All_F$invdimusageflag[is.na(All_F$invdimusageflag)] <- 0
All_F$invvarbymean[is.na(All_F$invvarbymean)] <- 0
All_F$pymtsnzeros[is.na(All_F$pymtsnzeros)] <- 10
All_F$pymtsdimusageflag[is.na(All_F$pymtsdimusageflag)] <- 0
All_F$pymtsvarbymean[is.na(All_F$pymtsvarbymean)] <- 0