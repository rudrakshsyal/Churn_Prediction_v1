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

################################# LOAD CODES ############################################################################

#---------------------------------Load all time ray subscriptions data------------------------------------------------------

SubsRaw <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/cff6950e-4bf3-6eb3-a2db-9f119a556372")

write.csv (SubsRaw,"SubsRaw.csv")
Subs <- SubsRaw
Subs$practice_id <- as.character(Subs$practice_id)
colnames(Subs)[colnames(Subs)=="practice_id"] <- "ray_practice_id"
Subs$start_date <- as.Date(Subs$start_date)
Subs$end_date <- as.Date(Subs$end_date)

#---------------------------------Load Ray usage data------------------------------------------------------------------------

#Calendar, manual added
Cal2016 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/5d372ea8-a1ae-fad0-e353-665bfa883ffb")
Cal2015 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/22d69c64-0833-e07e-82b9-54b6c96a4f80")
Cal2014 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/9d56f33c-9f26-f121-14f3-643ac93b03f3")
Cal <- rbind(Cal2016, Cal2015, Cal2014)
Cal$ray_practice_id <- as.character(Cal$ray_practice_id)

Cal2016 <- NULL
Cal2015 <- NULL
Cal2014 <- NULL

#Calendar, ABS added
CalABS2016 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/aa6fe72c-1e9c-83ba-62fd-7db2b2d5ca5b")
CalABS2015 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/e8fe46aa-d138-ff0d-efa7-05117650252b")
CalABS2014 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/d8f96169-a5d8-6f71-4986-a3ca6d3d0259")
CalABS <- rbind(CalABS2016, CalABS2015, CalABS2014)

CalABS2016 <- NULL
CalABS2015 <- NULL
CalABS2014 <- NULL

#Appointment files
Apptfiles2016 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/64de9af8-6058-dbdd-6ca7-2c381ddbd38a")
Apptfiles2015 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/e1b3adae-eb5e-9a67-ce1a-8f0da2ed07fd")
Apptfiles2014 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/cb9c35ca-2401-76c2-0f44-5764b1adaeb8")

Apptfiles <- rbind(Apptfiles2016, Apptfiles2015, Apptfiles2014)

Apptfiles2016 <- NULL
Apptfiles2015 <- NULL
Apptfiles2014 <- NULL

#Prescriptions
Presc2016 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/e42e3b54-c35e-caf7-3bf1-659f7c4a91e4")
Presc2015 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/3c5a1d3a-52ec-f0a9-ef7e-bbe5b56b5786")
Presc2014 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/1a646a1b-c174-ec0e-ff19-88b1c2ee2ea8")

Presc <- rbind(Presc2016, Presc2015, Presc2014)


Presc2016 <- NULL
Presc2015 <- NULL
Presc2014 <- NULL

#PatientInvoices
Inv2016 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/7823ac65-f5d4-11ac-7d02-94ac1ec7ae8b")
Inv2015 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/d4979832-598c-1acb-c160-e82cd7d72c7f")
Inv2014 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/95a56c84-1271-3818-e5e2-e62b141526a3")

Inv <- rbind(Inv2016, Inv2015, Inv2014)

Inv2016 <- NULL
Inv2015 <- NULL
Inv2014 <- NULL

#PatientFiles
Ptntfiles2016 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/df7cf62d-80b0-eeb3-637b-b4f6cdc91b0b")
Ptntfiles2015 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/dadbd34a-2e1b-1349-c719-ba11e7ad063e")
Ptntfiles2014 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/907413f9-6fe9-21e6-b8e0-9045d6336a41")

Ptntfiles <- rbind(Ptntfiles2016, Ptntfiles2015, Ptntfiles2014)

Ptntfiles2016 <- NULL
Ptntfiles2015 <- NULL
Ptntfiles2014 <- NULL

#Payments
Pymts2016 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/4b6e9127-f355-9880-67c2-cb03ac18d6df")
Pymts2015 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/99cc7010-5885-5320-5f02-28c7796e9a7e")
Pymts2014 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/a34fe7bd-e102-6a30-475c-7ea35c116957")

Pymts <- rbind(Pymts2016, Pymts2015, Pymts2014)

Pymts2016 <- NULL
Pymts2015 <- NULL
Pymts2014 <- NULL

#SoapNotes
Notes2016 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/fbdd4f7e-7f43-8b90-5701-6002793372b6")
Notes2015 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/77a79d94-f44f-faf8-bbc6-e40d32590691")
Notes2014 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/9d9fc54a-25be-9cea-6a05-301eedd812b1")

Notes <- rbind(Notes2016, Notes2015, Notes2014)

Notes2016 <- NULL
Notes2015 <- NULL
Notes2014 <- NULL

#GrowthCharts
Growth2016 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/7ae5abbb-643c-abea-1a44-28a4edf3ebd7")
Growth2015 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/09428ea1-6f26-3967-5f17-fee62589f57d")
Growth2014 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/94d54028-8914-9a6b-4fcf-5c59028fd6cd")

Growth <- rbind(Growth2016, Growth2015, Growth2014)

Growth2016 <- NULL
Growth2015 <- NULL
Growth2014 <- NULL

#VitalSigns
Vsigns2016 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/40de2901-5d24-79ae-8f34-5afb8067f11c")
Vsigns2015 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/29d6407f-865e-ca46-7a8a-dbf366919d8d")
Vsigns2014 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/1d54ed6f-f3ba-fd25-87ed-7bca6e598591")

Vsigns <- rbind(Vsigns2016, Vsigns2015, Vsigns2014)

Vsigns2016 <- NULL
Vsings2015 <- NULL
Vsigns2014 <- NULL

#Immunisations
Imm2016 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/89ff2efc-cf70-1032-b9b2-825a9088ac81")
Imm2015 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/9d810948-5051-65e1-bcc0-072b1d1d7d34")
Imm2014 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/7228c4ec-063c-291d-803d-102bf03850dd")

Imm <- rbind(Imm2016, Imm2015, Imm2014)

Imm2016 <- NULL
Imm2015 <- NULL
Imm2014 <- NULL

#Dental charts and lab tracking yet to be done

#SMSes
SMS2016 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/f571d8e9-bfde-c37d-8587-b7b4c181c3a6")
SMS2015 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/124a7136-2b3d-2fe9-d461-2600b09cb8f3")
SMS2014 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/51b86119-08b9-a4ef-2d22-a506937c32a4")

SMS <- rbind(SMS2016, SMS2015, SMS2014)

SMS2016 <- NULL
SMS2015 <- NULL
SMS2014 <- NULL

#Tab usage
TFb <- read.csv ("http://www.periscopedata.com/api/practo/chart/csv/3fe47be2-8d5b-fe54-45fd-8b10f1d894ea")
TAppts <- read.csv ("http://www.periscopedata.com/api/practo/chart/csv/be58055b-eb2f-c6ae-14e7-1b1b74fe1ed8")
TReg <- read.csv ("http://www.periscopedata.com/api/practo/chart/csv/e67d700e-5da8-5b8f-c94e-8b077811cf33")
TApptfiles <- read.csv ("http://www.periscopedata.com/api/practo/chart/csv/f70ad305-8a59-9f3b-bdc7-fdde9fdb69c4")
TPresc <- read.csv ("http://www.periscopedata.com/api/practo/chart/csv/ba4ebd14-5f87-61ec-cfc5-4a901fbcfe2d")
TNotes <- read.csv ("http://www.periscopedata.com/api/practo/chart/csv/c0beff8e-1abb-d85c-cf3e-85a49f122935")

#---------------------------------Load Issues data-------------------------------------------------------------

Cissues0516 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/7585fce0-5f82-6b86-d2a0-fec6839fb6c1",stringsAsFactors = FALSE)
Cissues0616 <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/fd63101b-aa0b-13b2-0628-21676a9d04aa",stringsAsFactors = FALSE)
Cissues <- rbind(Cissues0516, Cissues0616)
Gissues <- read.csv("C:/Users/ChetanBhat/Dropbox/Issues/issues_consolidated.csv",stringsAsFactors = FALSE)

#create raw issues data after union of chronos and galaxy
Gissues$sentiment <- ifelse(grepl("angry",Gissues$tags),"angry",ifelse(grepl("happy",Gissues$tags),"happy", ifelse(grepl("neutral",Gissues$tags),"neutral","unknown"))) 
temp <- Gissues[,c("practice_id", "source", "product", "type", "status", "created_on", "resolved_on", "sentiment")]
colnames(temp)[colnames(temp)=="practice_id"] <- "ray_practice_id"
colnames(Cissues)[colnames(Cissues)=="modified_on"] <- "resolved_on"
Issuesraw <- rbind(temp, Cissues)
Issuesraw <- subset(Issuesraw, Issuesraw$status != "INVALID")

#---------------------------------Load ABS, appointments, calls data, DQS, reviews, recos------------------------------

ABS <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/b74cee3b-2254-2dda-3eb6-580ff601fcd7")
ABSAppts <- read.csv ("http://www.periscopedata.com/api/practo/chart/csv/0e2b93f4-0345-0bdb-df9b-b33ea3793296")
VNCalls <- read.csv ("http://www.periscopedata.com/api/practo/chart/csv/2c81afc5-9843-6d6e-6c09-642e3a0670eb")
dDQS <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/2bb62ce8-fe5c-415c-6491-27e841da8815")
pDQS <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/0b9242a5-0a57-6e86-fe90-2527d3873078")
Recoraw <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/8c2c6f54-a096-fc42-58c3-5e0caddbe9ff")
Revraw <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/472b243a-93f8-9677-e75b-c35e487767eb")

Recoraw$ray_practice_id <- as.character(Recoraw$ray_practice_id)
dDQS$ray_practice_id <- as.character(dDQS$ray_practice_id)
ABS$ray_practice_id <- as.character(ABS$ray_practice_id)
ABSAppts$ray_practice_id <- as.character(ABSAppts$ray_practice_id)

ABSAppts <- subset(ABSAppts, !is.na(ABSAppts$ray_practice_id))
VNCalls <- subset(VNCalls, !is.na(VNCalls$ray_practice_id))

#---------------------------------Load Reach subscriptions data--------------------------------------------------------

Reachraw <- read.csv ("http://www.periscopedata.com/api/practo/chart/csv/608ef427-613a-2fdf-a953-9461ae225ba7")
write.csv(Reachraw,"All time reach subscriptions.csv")

#---------------------------------Load Orders and discounts data--------------------------------------------------------

Ord2015_16 <- read.csv ("http://www.periscopedata.com/api/practo/chart/csv/e58ef8ec-834c-ed43-b57e-8912ca5be7b4")
Ord2014 <- read.csv ("http://www.periscopedata.com/api/practo/chart/csv/b9e148a7-8c83-6acd-1bf7-ea5b77bd16a5")

Ord <- rbind(Ord2015_16,Ord2014)

Ord2015_16 <- NULL
Ord2014 <- NULL

#---------------------------------Load and create profile master--------------------------------------------------------

directory <- "C:/Office/Profile Log"

#Create consolidated profiles file
file_list <- list.files(directory, pattern="doctor*", full.names=TRUE)
temp<-data.frame()

i = 1

for (i in length(file_list):1)
{
  data<-read.csv(file_list[i])
  data$key <- as.Date(paste0(substr(file_list[i],38,44),"-01"))
  data <- data[,c("ray_practice_id", "PracticeDoctor_id", 'locality', 'city', 'speciality', 'award', 'number_of_photos',	'logo_presence',	'doctor_photos',	'Experience',	'organizations',	'consultation_fee','memberships',	'qualifications',	'registration',	'services',"key")]
  temp<-rbind.fill(temp,data)
  print(file_list[i])
}

temp<-subset(temp,!is.na(temp$ray_practice_id)) 
temp<-subset(temp,temp$ray_practice_id != '#N/A') 

write.csv(temp,"C:/Office/Profile Log/Processed Profiles data/Profiles_All.csv",row.names = F)

Profiles_All <- read.csv("C:/Office/Profile Log/Processed Profiles data/Profiles_All.csv")
Profiles_All$ray_practice_id <- as.character(Profiles_All$ray_practice_id)
Profiles_All$PracticeDoctor_id <- as.character(Profiles_All$PracticeDoctor_id)

#---------------------------------Load data required for Geography dynamics processing. NEED TO REFINE FURTHER-------------------------------

temp <- read.csv("C:/Office/Profile Log/doctor_profile_2016-07-03complete.csv")
temp <- temp[,c("ray_practice_id", "doctor_id", "PracticeDoctor_id", "status", "consultation_fee", "locality", "city", "dqs", "speciality", "recommendation")]
temp <- unique(temp)

Zonelocality <- read.csv("http://www.periscopedata.com/api/practo/chart/csv/220c56a8-fb88-d7a2-1684-aef0e4aeac5e")
Zonefactor <- read.csv("C:/Users/ChetanBhat/Dropbox/BI Shared Folder/Zone-ZoneFactor Map.csv")
Practiceowners <- read.csv("C:/Users/ChetanBhat/Dropbox/Ray Customers/Ray_Customers_Owners.csv")
colnames(Practiceowners)[colnames(Practiceowners)=='PK_idCalendarSettings'] <- "ray_practice_id"
Practicepdp <- read.csv("C:/Users/ChetanBhat/Dropbox/Ray Customers/Practice_Doctor_Mappings.csv")
Practicepdp <- subset(Practicepdp, !is.na(pdp_id))

################################# PRE-PROCESS CODES #####################################################################
################################# ISSUES AND PROFILES DATA NEEDS PRE-PROCESSING #########################################

#---------------------------------Profiles pre-processing-------------------------------------------------------------------

#Create Intermediate version (I.1) containing several model IDVs

##Consultation fees processing

###First Calculating fee averages at the locality-specialty and city-specialty levels

####locality-specialty level
####Remove random fees entries sometime later
temp <- aggregate(consultation_fee~locality+city+speciality, data=Profiles_All, FUN = mean, na.rm=T)
colnames(temp) <- c("locality", "city", "speciality", "fees_ls")
Profiles_All_I <- merge(x=Profiles_All, y=temp, by = c("locality","city", "speciality"), all.x = T, all.y=F)

####city-specialty level
temp <- aggregate(consultation_fee~city+speciality, data=Profiles_All, FUN = mean, na.rm=T)
colnames(temp) <- c("city", "speciality", "fees_cs")
Profiles_All_I <- merge(x=Profiles_All_I, y=temp, by = c("city", "speciality"), all.x = T, all.y=F)

#### Choose best available feels info
Profiles_All_I$Newfees <- ifelse(!is.na(Profiles_All_I$consultation_fee) == TRUE, Profiles_All_I$consultation_fee, ifelse(!is.na(Profiles_All_I$fees_ls) ==TRUE,Profiles_All_I$fees_ls,Profiles_All_I$fees_cs))

#### Calculate practice level mean and max fees
temp_mean <- aggregate(Newfees~ray_practice_id+key,data=Profiles_All_I,FUN=mean, na.rm=T)
colnames(temp_mean) <- c("ray_practice_id", "key", "fees_mean")
temp_max <- aggregate(Newfees~ray_practice_id+key,data=Profiles_All_I,FUN=max, na.rm=T)
colnames(temp_max) <- c("ray_practice_id", "key", "fees_max")
Profiles_fees <- merge(x=temp_mean, y=temp_max, by=c("ray_practice_id","key"), all.x = T)

#### FOR LATER: Further refine Profiles_Fees further. Investigate very low and very high fees. Drop them from data

##Experience processing
Profiles_All$Experience <- ifelse(Profiles_All$Experience>=1900,2016-Profiles_All$Experience,Profiles_All$Experience)
temp_mean <- aggregate(Experience~ray_practice_id+key,data=Profiles_All, FUN=mean, na.rm=T)
colnames(temp_mean) <- c("ray_practice_id", "key", "exp_mean")
temp_max <- aggregate(Experience~ray_practice_id+key,data=Profiles_All, FUN=max, na.rm=T)
colnames(temp_max) <- c("ray_practice_id", "key", "exp_max")
temp_min <- aggregate(Experience~ray_practice_id+key,data=Profiles_All, FUN=min, na.rm=T)
colnames(temp_min) <- c("ray_practice_id", "key", "exp_min")

temp <- merge(x=temp_mean, y=temp_max, by=c("ray_practice_id","key"), all.x = T)
Profiles_exp <-merge(x=temp, y=temp_min, by=c("ray_practice_id","key"), all.x = T)

##FOR LATER Remove very low and high experience from data. Similarly, remove outliers from all datasets below

##Doctor count
Profiles_ndocs <- aggregate(PracticeDoctor_id~ray_practice_id+key, data=Profiles_All, FUN = length)

##Practice photos
Profiles_pphotos <- unique(Profiles_All[, c("ray_practice_id", "key","number_of_photos")])
Profiles_pphotos[is.na(Profiles_pphotos)] <- 0

##Practice logo
Profiles_logo <- unique(Profiles_All[, c("ray_practice_id", "key","logo_presence")])
Profiles_logo[is.na(Profiles_logo)] <- 0

##Doc photos and Logo processing
temp <- Profiles_All
temp$doctor_photos <- ifelse(is.na(temp$doctor_photos)==TRUE,0,temp$doctor_photos)
temp$logo_presence <- ifelse(is.na(temp$logo_presence)==TRUE,0,temp$logo_presence)
temp_mean <-aggregate(doctor_photos~ray_practice_id+key,data=temp, FUN=mean)
colnames(temp_mean) <- c("ray_practice_id", "key", "dphotos_mean")
temp_sum <-aggregate(doctor_photos~ray_practice_id+key,data=temp, FUN=sum)
colnames(temp_sum) <- c("ray_practice_id", "key", "dphotos_sum")

Profiles_dphotos <- merge(x=temp_mean, y=temp_sum, by=c("ray_practice_id","key"), all.x = T)

##Merging all individual profile tables into final intermediate table I.1
datlist <- list()
datlist[[1]] <- Profiles_fees
datlist[[2]] <- Profiles_exp
datlist[[3]] <- Profiles_ndocs
datlist[[4]] <- Profiles_pphotos
datlist[[5]] <- Profiles_dphotos
datlist[[6]] <- Profiles_logo

Profiles_I.1<-Reduce(function(x,y) {merge(x,y,by=intersect(names(x),names(y)),all.x=TRUE,all.y=TRUE)}, datlist)
colnames(Profiles_I.1)[colnames(Profiles_I.1) == 'key'] <- 'date'
Profiles_I.1$date <- as.Date(Profiles_I.1$date)

#Create Intermediate version (I.2) containing appointments,reviews/recos, calls IDVs

datlist <- list()
datlist[[1]] <- ABSAppts
datlist[[2]] <- VNCalls
datlist[[3]] <- Revraw
datlist[[4]] <- Recoraw

Profiles_I.2<-Reduce(function(x,y) {merge(x,y,by=intersect(names(x),names(y)),all.x=TRUE,all.y=TRUE)}, datlist)
Profiles_I.2$ray_practice_id <- as.character(Profiles_I.2$ray_practice_id)
Profiles_I.2$date <- as.Date(paste(Profiles_I.2$date,"-01", sep=""), format="%Y-%m-%d")

#Create Final intermediate version (I.1.2) by merging both I.1 and I.2
Profiles_I.1.2 <- merge(x=Profiles_I.1, y=Profiles_I.2, by = c("ray_practice_id", "date"), all.x=T, all.y=T)
Profiles_I.1.2$ray_practice_id <- as.character(Profiles_I.1.2$ray_practice_id)

#---------------------------------Issues pre-processing-------------------------------------------------------------------

Issuesraw$created_on <- ifelse(substr(Issuesraw$created_on,7,10)=='2015',paste0(substr(Issuesraw$created_on,7,10),"-",substr(Issuesraw$created_on,4,5),"-",substr(Issuesraw$created_on,1,2)),Issuesraw$created_on) 
Issuesraw$resolved_on <- ifelse(substr(Issuesraw$resolved_on,7,10)=='2015',paste0(substr(Issuesraw$resolved_on,7,10),"-",substr(Issuesraw$resolved_on,4,5),"-",substr(Issuesraw$resolved_on,1,2)),Issuesraw$resolved_on) 
Issuesraw$time <- difftime(as.Date(Issuesraw$resolved_on), as.Date(Issuesraw$created_on), units="days")

#Intermediate issues table: Create various variable buckets
IssuesI.1 <- Issuesraw
IssuesI.1$product <- tolower(IssuesI.1$product)
IssuesI.1$productbucket <- ifelse(grepl("ray",IssuesI.1$product), "ray", ifelse(grepl("search",IssuesI.1$product), "search", ifelse(grepl("tab",IssuesI.1$product), "tab", "othersP")))
IssuesI.1$type <- tolower(IssuesI.1$type)
IssuesI.1$typebucket <- ifelse(grepl("profile",IssuesI.1$type) | grepl("reviews",IssuesI.1$type) , "profile", ifelse(grepl("training",IssuesI.1$type) | grepl("onboarding",IssuesI.1$type), "training", ifelse(grepl("vn",IssuesI.1$type), "vn", ifelse(grepl("orphan",IssuesI.1$type) | grepl("query",IssuesI.1$type) | grepl("followup",IssuesI.1$type), "generic", "othersT"))))
IssuesI.1$source <- tolower(IssuesI.1$source)
IssuesI.1$sourcebucket <- ifelse(grepl("call", IssuesI.1$source), "call", ifelse(grepl("chat", IssuesI.1$source), "chat", ifelse(grepl("email", IssuesI.1$source), "email", ifelse(grepl("feedback", IssuesI.1$source), "feedback", ifelse(grepl("staff", IssuesI.1$source), "staff", ifelse(grepl("fabric_website", IssuesI.1$source), "fabric_website", "othersS"))))))
IssuesI.1$created_on <- substr(IssuesI.1$created_on, 1, 7)
IssuesI.1$created_on <- as.character(IssuesI.1$created_on)
IssuesI.1$sentiment <- tolower(IssuesI.1$sentiment)

#calculate number of issues by various issue buckets and store in intermediate issues table I.2
collist <- data.frame(col = c("productbucket", "typebucket", "sourcebucket", "sentiment"))

i=1

IssuesI.2 <- aggregate(product~ray_practice_id+created_on, IssuesI.1, FUN=length)
colnames(IssuesI.2)[3] <- "Totalissues"

for (i in 1:nrow(collist)){
  f <- paste(names(IssuesI.1)[3],"~",names(IssuesI.1)[1],"+",names(IssuesI.1)[6],"+",as.character(collist[i,1]))
  temp <- aggregate(as.formula(f), data=IssuesI.1, FUN=length)
  colnames(temp)[4] <- "freq"
  f <- paste(names(temp)[1],"+",names(temp)[2],"~",names(temp)[3])
  temp <- cast(temp, f, value='freq')
  IssuesI.2 <- merge(x=IssuesI.2, y=temp, by=c("ray_practice_id", "created_on"), all.x=T)
  print(collist[i,1])
}

#calculate average issue resolution time
temp <- aggregate(time~ray_practice_id+created_on, data=IssuesI.1, mean) #takes more than a minute to run. 
IssuesI.2 <- merge(x=IssuesI.2, y=temp, by=c("ray_practice_id", "created_on"), all.x=T)

