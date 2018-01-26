setwd("D:/Project - Upselling Algorithm/Queries")

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

#################################### Preparation for Populating Attributes #####################################################################################

Master_File <- read.csv("../Master_File.csv", stringsAsFactors = F)
Ray_Customers <- read.csv("C:/Users/Practo/Dropbox/Ray Customers/Ray_Customers.csv", stringsAsFactors = F)

# Post-Reading #
# Master_File$Hunting_Date <- as.Date(paste0(Master_File$Hunting_Date, '%Y-%m-%d'))
# Master_File$Upsell_Date <- as.Date(paste0(Master_File$Upsell_Date, '%Y-%m-%d'))

Master_File$Month <- ifelse((month(as.Date(Master_File$Hunting_Date))<10), paste0("0",month(as.Date(Master_File$Hunting_Date))), month(as.Date(Master_File$Hunting_Date)))
Master_File$Year <- year(as.Date(Master_File$Hunting_Date))
Master_File$Hunting_Date <- paste0(Master_File$Year, "-", Master_File$Month)
Master_File$Month <- Master_File$Year <- NULL

Month_Mapping <- read.csv("../Month_Mapping.csv")
Master_File$Reference <- Month_Mapping$Reference[match(Master_File$Hunting_Date, Month_Mapping$Date)]

Master_File$Upsell_Date[Master_File$Upsell_Date == ""] <- "2016-06-30"
Master_File$Month <- ifelse((month(as.Date(Master_File$Upsell_Date))<10), paste0("0",month(as.Date(Master_File$Upsell_Date))), month(as.Date(Master_File$Upsell_Date)))
Master_File$Year <- year(as.Date(Master_File$Upsell_Date))
Master_File$Upsell_Date <- paste0(Master_File$Year, "-", Master_File$Month)
Master_File$Month <- Master_File$Year <- NULL

Master_File$Upsell <- Month_Mapping$Reference[match(Master_File$Upsell_Date, Month_Mapping$Date)]
Master_File$practice_id <- Master_File$Hunting_Date <- Master_File$Upsell_Date <- NULL

# Ray Appointments #####################################################################################
Ray_Appnts_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/5d372ea8-a1ae-fad0-e353-665bfa883ffb")
Ray_Appnts_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/22d69c64-0833-e07e-82b9-54b6c96a4f80")
Ray_Appnts_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/9d56f33c-9f26-f121-14f3-643ac93b03f3")
Ray_Appnts <- rbind(Ray_Appnts_2014, Ray_Appnts_2015, Ray_Appnts_2016)
Ray_Appnts_2014 <- Ray_Appnts_2015 <- Ray_Appnts_2016 <- NULL

Ray_Appnts$Reference <- Month_Mapping$Reference[match(Ray_Appnts$date, Month_Mapping$Date)]
# Ray Appointments #
Ray_Appnts <- Ray_Appnts[order(Ray_Appnts$ray_practice_id, Ray_Appnts$date),]
Ray_Appnts$Indent[2:length(Ray_Appnts$ray_practice_id)] <- Ray_Appnts$count[1:length(Ray_Appnts$ray_practice_id)-1]
Ray_Appnts$Indent[is.na(Ray_Appnts$Indent)] <- 0
Ray_Appnts$Indent[Ray_Appnts$Indent > 828] <- 828
Ray_Appnts$count[Ray_Appnts$count > 828] <- Ray_Appnts$Indent[Ray_Appnts$count > 828]
Ray_Appnts$Indent <- NULL
# Ray Appointments #
Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp <- subset(Ray_Appnts, Ray_Appnts$ray_practice_id == id)
  temp1 <- subset(temp, temp$Reference == (dt+1))
  temp2 <- subset(temp, temp$Reference == (dt+2))
  temp3 <- subset(temp,temp$Reference == (dt+3))
  temp4 <- rbind(temp1, temp2, temp3)
  if(nrow(temp4) == 0) {
    temp4 <- temp
  } else {
    temp4 <- aggregate(days~ray_practice_id, data = temp4, FUN = sum)
  }
  ##
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- 0)
  ifelse((nrow(temp2) != 0),Master_File$Time2[i] <- temp2$count[match(Master_File$ray_practice_id[i], temp2$ray_practice_id)], Master_File$Time2[i] <- 0)
  ifelse((nrow(temp3) != 0), Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)],Master_File$Time3[i] <- 0)
  ifelse((nrow(temp4) != 0), Master_File$days[i] <- temp4$days[match(Master_File$ray_practice_id[i], temp4$ray_practice_id)],Master_File$days[i] <- 0)
}

colnames(Master_File) <- c("ray_practice_id", "Type", "Reference", "Month-1", "Month-2", "Month-3", "Days_Used")
Master_File$`C_Month-2` <- as.numeric(Master_File$`Month-1`) + as.numeric(Master_File$`Month-2`)
Master_File$`C_Month-3` <- as.numeric(Master_File$`C_Month-2`) + as.numeric(Master_File$`Month-2`)
Master_File$Average_3Months <- as.numeric(Master_File$`C_Month-3`)/3
Master_File[,"Max_3Months"] <- apply(Master_File[,4:6],1, max)
Master_File$`D_Month-2` <- as.numeric(Master_File$`Month-1`)/2 + as.numeric(Master_File$`Month-2`)
Master_File$`D_Month-3` <- as.numeric(Master_File$`D_Month-2`)/2 + as.numeric(Master_File$`Month-3`)
Master_File$`Sqrt_Month-1` <- sqrt(as.numeric(Master_File$`Month-1`))
Master_File$`Sqrt_Month-2` <- sqrt(Master_File$`C_Month-2`)
Master_File$`Sqrt_Month-3` <- sqrt(Master_File$`C_Month-3`)

Master_File$`Month-1` <- as.numeric(Master_File$`Month-1`)
Master_File$`Categ_Month-1` <- 'Low'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 13] <- 'Medium'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 111] <- 'High'
Master_File$`Month-2` <- as.numeric(Master_File$`Month-2`)
Master_File$`Categ_Month-2` <- 'Low'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 13] <- 'Medium'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 111] <- 'High'
Master_File$`Month-3` <- as.numeric(Master_File$`Month-3`)
Master_File$`Categ_Month-3` <- 'Low'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 13] <- 'Medium'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 111] <- 'High'

write.csv(Master_File, "../Ray Usage Attributes/To be Fed into Usage Model/Ray_Appnts.csv")

# Ray Appointment Files #####################################################################################

Ray_Appnt_Files_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/64de9af8-6058-dbdd-6ca7-2c381ddbd38a")
Ray_Appnt_Files_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/e1b3adae-eb5e-9a67-ce1a-8f0da2ed07fd")
Ray_Appnt_Files_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/cb9c35ca-2401-76c2-0f44-5764b1adaeb8")
Ray_Appnt_Files <- rbind(Ray_Appnt_Files_2014, Ray_Appnt_Files_2015, Ray_Appnt_Files_2016)

Ray_Appnt_Files$Reference <- Month_Mapping$Reference[match(Ray_Appnt_Files$date, Month_Mapping$Date)]
# Ray Appointment Files #
Ray_Appnt_Files <- Ray_Appnt_Files[order(Ray_Appnt_Files$ray_practice_id, Ray_Appnt_Files$date),]
Ray_Appnt_Files$Indent[2:length(Ray_Appnt_Files$ray_practice_id)] <- Ray_Appnt_Files$count[1:length(Ray_Appnt_Files$ray_practice_id)-1]
Ray_Appnt_Files$Indent[is.na(Ray_Appnt_Files$Indent)] <- 0
Ray_Appnt_Files$Indent[Ray_Appnt_Files$Indent > 423] <- 423
Ray_Appnt_Files$count[Ray_Appnt_Files$count > 423] <- Ray_Appnt_Files$Indent[Ray_Appnt_Files$count > 423]
Ray_Appnt_Files$Indent <- NULL
# Ray Appointment Files #
Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp <- subset(Ray_Appnt_Files, Ray_Appnt_Files$ray_practice_id == id)
  temp1 <- subset(temp, temp$Reference == (dt+1))
  temp2 <- subset(temp, temp$Reference == (dt+2))
  temp3 <- subset(temp,temp$Reference == (dt+3))
  temp4 <- rbind(temp1, temp2, temp3)
  if(nrow(temp4) == 0) {
    temp4 <- temp
  } else {
    temp4 <- aggregate(days~ray_practice_id, data = temp4, FUN = sum)
  }
  ##
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- 0)
  ifelse((nrow(temp2) != 0),Master_File$Time2[i] <- temp2$count[match(Master_File$ray_practice_id[i], temp2$ray_practice_id)], Master_File$Time2[i] <- 0)
  ifelse((nrow(temp3) != 0), Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)],Master_File$Time3[i] <- 0)
  ifelse((nrow(temp4) != 0), Master_File$days[i] <- temp4$days[match(Master_File$ray_practice_id[i], temp4$ray_practice_id)],Master_File$days[i] <- 0)
}

colnames(Master_File) <- c("ray_practice_id", "Type", "Reference", "Month-1", "Month-2", "Month-3", "Days_Used")
Master_File$`C_Month-2` <- as.numeric(Master_File$`Month-1`) + as.numeric(Master_File$`Month-2`)
Master_File$`C_Month-3` <- as.numeric(Master_File$`C_Month-2`) + as.numeric(Master_File$`Month-2`)
Master_File$Average_3Months <- as.numeric(Master_File$`C_Month-3`)/3
Master_File[,"Max_3Months"] <- apply(Master_File[,4:6],1, max)
Master_File$`D_Month-2` <- as.numeric(Master_File$`Month-1`)/2 + as.numeric(Master_File$`Month-2`)
Master_File$`D_Month-3` <- as.numeric(Master_File$`D_Month-2`)/2 + as.numeric(Master_File$`Month-3`)
Master_File$`Sqrt_Month-1` <- sqrt(as.numeric(Master_File$`Month-1`))
Master_File$`Sqrt_Month-2` <- sqrt(Master_File$`C_Month-2`)
Master_File$`Sqrt_Month-3` <- sqrt(Master_File$`C_Month-3`)

Master_File$`Month-1` <- as.numeric(Master_File$`Month-1`)
Master_File$`Categ_Month-1` <- 'Low'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 2] <- 'Medium'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 12] <- 'High'
Master_File$`Month-2` <- as.numeric(Master_File$`Month-2`)
Master_File$`Categ_Month-2` <- 'Low'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 2] <- 'Medium'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 12] <- 'High'
Master_File$`Month-3` <- as.numeric(Master_File$`Month-3`)
Master_File$`Categ_Month-3` <- 'Low'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 2] <- 'Medium'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 12] <- 'High'

write.csv(Master_File, "../Ray Usage Attributes/To be Fed into Usage Model/Ray_Appnt_Files.csv")

# Ray Prescriptions #####################################################################################
Ray_Prescrip_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/e42e3b54-c35e-caf7-3bf1-659f7c4a91e4")
Ray_Prescrip_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/3c5a1d3a-52ec-f0a9-ef7e-bbe5b56b5786")
Ray_Prescrip_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/1a646a1b-c174-ec0e-ff19-88b1c2ee2ea8")
Ray_Prescrip <- rbind(Ray_Prescrip_2014, Ray_Prescrip_2015, Ray_Prescrip_2016)

Ray_Prescrip$Reference <- Month_Mapping$Reference[match(Ray_Prescrip$date, Month_Mapping$Date)]
# Ray Prescriptions #
Ray_Prescrip <- Ray_Prescrip[order(Ray_Prescrip$ray_practice_id, Ray_Prescrip$date),]
Ray_Prescrip$Indent[2:length(Ray_Prescrip$ray_practice_id)] <- Ray_Prescrip$count[1:length(Ray_Prescrip$ray_practice_id)-1]
Ray_Prescrip$Indent[is.na(Ray_Prescrip$Indent)] <- 0
Ray_Prescrip$Indent[Ray_Prescrip$Indent > 497] <- 497
Ray_Prescrip$count[Ray_Prescrip$count > 497] <- Ray_Prescrip$Indent[Ray_Prescrip$count > 497]
Ray_Prescrip$Indent <- NULL
# Ray Prescriptions #
Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp <- subset(Ray_Prescrip, Ray_Prescrip$ray_practice_id == id)
  temp1 <- subset(temp, temp$Reference == (dt+1))
  temp2 <- subset(temp, temp$Reference == (dt+2))
  temp3 <- subset(temp,temp$Reference == (dt+3))
  temp4 <- rbind(temp1, temp2, temp3)
  if(nrow(temp4) == 0) {
    temp4 <- temp
  } else {
    temp4 <- aggregate(days~ray_practice_id, data = temp4, FUN = sum)
  }
  ##
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- 0)
  ifelse((nrow(temp2) != 0),Master_File$Time2[i] <- temp2$count[match(Master_File$ray_practice_id[i], temp2$ray_practice_id)], Master_File$Time2[i] <- 0)
  ifelse((nrow(temp3) != 0), Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)],Master_File$Time3[i] <- 0)
  ifelse((nrow(temp4) != 0), Master_File$days[i] <- temp4$days[match(Master_File$ray_practice_id[i], temp4$ray_practice_id)],Master_File$days[i] <- 0)
}

colnames(Master_File) <- c("ray_practice_id", "Type", "Reference", "Month-1", "Month-2", "Month-3", "Days_Used")
Master_File$`C_Month-2` <- as.numeric(Master_File$`Month-1`) + as.numeric(Master_File$`Month-2`)
Master_File$`C_Month-3` <- as.numeric(Master_File$`C_Month-2`) + as.numeric(Master_File$`Month-2`)
Master_File$Average_3Months <- as.numeric(Master_File$`C_Month-3`)/3
Master_File[,"Max_3Months"] <- apply(Master_File[,4:6],1, max)
Master_File$`D_Month-2` <- as.numeric(Master_File$`Month-1`)/2 + as.numeric(Master_File$`Month-2`)
Master_File$`D_Month-3` <- as.numeric(Master_File$`D_Month-2`)/2 + as.numeric(Master_File$`Month-3`)
Master_File$`Sqrt_Month-1` <- sqrt(as.numeric(Master_File$`Month-1`))
Master_File$`Sqrt_Month-2` <- sqrt(Master_File$`C_Month-2`)
Master_File$`Sqrt_Month-3` <- sqrt(Master_File$`C_Month-3`)

Master_File$`Month-1` <- as.numeric(Master_File$`Month-1`)
Master_File$`Categ_Month-1` <- 'Low'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 1] <- 'Medium'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 6] <- 'High'
Master_File$`Month-2` <- as.numeric(Master_File$`Month-2`)
Master_File$`Categ_Month-2` <- 'Low'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 1] <- 'Medium'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 6] <- 'High'
Master_File$`Month-3` <- as.numeric(Master_File$`Month-3`)
Master_File$`Categ_Month-3` <- 'Low'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 1] <- 'Medium'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 6] <- 'High'

write.csv(Master_File, "../Ray Usage Attributes/To be Fed into Usage Model/Ray_Prescrip.csv")

# Ray Invoices #####################################################################################

Ray_Invoices_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/7823ac65-f5d4-11ac-7d02-94ac1ec7ae8b")
Ray_Invoices_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/d4979832-598c-1acb-c160-e82cd7d72c7f")
Ray_Invoices_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/95a56c84-1271-3818-e5e2-e62b141526a3")
Ray_Invoices <- rbind(Ray_Invoices_2014, Ray_Invoices_2015, Ray_Invoices_2016)

Ray_Invoices$Reference <- Month_Mapping$Reference[match(Ray_Invoices$date, Month_Mapping$Date)]
# Ray Invoices #
Ray_Invoices <- Ray_Invoices[order(Ray_Invoices$ray_practice_id, Ray_Invoices$date),]
Ray_Invoices$Indent[2:length(Ray_Invoices$ray_practice_id)] <- Ray_Invoices$count[1:length(Ray_Invoices$ray_practice_id)-1]
Ray_Invoices$Indent[is.na(Ray_Invoices$Indent)] <- 0
Ray_Invoices$Indent[Ray_Invoices$Indent > 560] <- 560
Ray_Invoices$count[Ray_Invoices$count > 560] <- Ray_Invoices$Indent[Ray_Invoices$count > 560]
Ray_Invoices$Indent <- NULL
# Ray Invoices #
Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp <- subset(Ray_Invoices, Ray_Invoices$ray_practice_id == id)
  temp1 <- subset(temp, temp$Reference == (dt+1))
  temp2 <- subset(temp, temp$Reference == (dt+2))
  temp3 <- subset(temp,temp$Reference == (dt+3))
  temp4 <- rbind(temp1, temp2, temp3)
  if(nrow(temp4) == 0) {
    temp4 <- temp
  } else {
    temp4 <- aggregate(days~ray_practice_id, data = temp4, FUN = sum)
  }
  ##
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- 0)
  ifelse((nrow(temp2) != 0),Master_File$Time2[i] <- temp2$count[match(Master_File$ray_practice_id[i], temp2$ray_practice_id)], Master_File$Time2[i] <- 0)
  ifelse((nrow(temp3) != 0), Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)],Master_File$Time3[i] <- 0)
  ifelse((nrow(temp4) != 0), Master_File$days[i] <- temp4$days[match(Master_File$ray_practice_id[i], temp4$ray_practice_id)],Master_File$days[i] <- 0)
}

colnames(Master_File) <- c("ray_practice_id", "Type", "Reference", "Month-1", "Month-2", "Month-3", "Days_Used")
Master_File$`C_Month-2` <- as.numeric(Master_File$`Month-1`) + as.numeric(Master_File$`Month-2`)
Master_File$`C_Month-3` <- as.numeric(Master_File$`C_Month-2`) + as.numeric(Master_File$`Month-2`)
Master_File$Average_3Months <- as.numeric(Master_File$`C_Month-3`)/3
Master_File[,"Max_3Months"] <- apply(Master_File[,4:6],1, max)
Master_File$`D_Month-2` <- as.numeric(Master_File$`Month-1`)/2 + as.numeric(Master_File$`Month-2`)
Master_File$`D_Month-3` <- as.numeric(Master_File$`D_Month-2`)/2 + as.numeric(Master_File$`Month-3`)
Master_File$`Sqrt_Month-1` <- sqrt(as.numeric(Master_File$`Month-1`))
Master_File$`Sqrt_Month-2` <- sqrt(Master_File$`C_Month-2`)
Master_File$`Sqrt_Month-3` <- sqrt(Master_File$`C_Month-3`)

Master_File$`Month-1` <- as.numeric(Master_File$`Month-1`)
Master_File$`Categ_Month-1` <- 'Low'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 13] <- 'Medium'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 111] <- 'High'
Master_File$`Month-2` <- as.numeric(Master_File$`Month-2`)
Master_File$`Categ_Month-2` <- 'Low'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 13] <- 'Medium'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 111] <- 'High'
Master_File$`Month-3` <- as.numeric(Master_File$`Month-3`)
Master_File$`Categ_Month-3` <- 'Low'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 13] <- 'Medium'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 111] <- 'High'

write.csv(Master_File, "../Ray Usage Attributes/To be Fed into Usage Model/Ray_Invoices.csv")

# Ray Patient Files #####################################################################################

Ray_PF_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/df7cf62d-80b0-eeb3-637b-b4f6cdc91b0b")
Ray_PF_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/dadbd34a-2e1b-1349-c719-ba11e7ad063e")
Ray_PF_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/907413f9-6fe9-21e6-b8e0-9045d6336a41")
Ray_PF <- rbind(Ray_PF_2014, Ray_PF_2015, Ray_PF_2016)

Ray_PF$Reference <- Month_Mapping$Reference[match(Ray_PF$date, Month_Mapping$Date)]
# Ray PF #
Ray_PF <- Ray_PF[order(Ray_PF$ray_practice_id, Ray_PF$date),]
Ray_PF$Indent[2:length(Ray_PF$ray_practice_id)] <- Ray_PF$count[1:length(Ray_PF$ray_practice_id)-1]
Ray_PF$Indent[is.na(Ray_PF$Indent)] <- 0
Ray_PF$Indent[Ray_PF$Indent > 151] <- 151
Ray_PF$count[Ray_PF$count > 151] <- Ray_PF$Indent[Ray_PF$count > 151]
Ray_PF$Indent <- NULL
# Ray PF #
Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp <- subset(Ray_PF, Ray_PF$ray_practice_id == id)
  temp1 <- subset(temp, temp$Reference == (dt+1))
  temp2 <- subset(temp, temp$Reference == (dt+2))
  temp3 <- subset(temp,temp$Reference == (dt+3))
  temp4 <- rbind(temp1, temp2, temp3)
  if(nrow(temp4) == 0) {
    temp4 <- temp
  } else {
    temp4 <- aggregate(days~ray_practice_id, data = temp4, FUN = sum)
  }
  ##
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- 0)
  ifelse((nrow(temp2) != 0),Master_File$Time2[i] <- temp2$count[match(Master_File$ray_practice_id[i], temp2$ray_practice_id)], Master_File$Time2[i] <- 0)
  ifelse((nrow(temp3) != 0), Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)],Master_File$Time3[i] <- 0)
  ifelse((nrow(temp4) != 0), Master_File$days[i] <- temp4$days[match(Master_File$ray_practice_id[i], temp4$ray_practice_id)],Master_File$days[i] <- 0)
}

colnames(Master_File) <- c("ray_practice_id", "Type", "Reference", "Month-1", "Month-2", "Month-3", "Days_Used")
Master_File$`C_Month-2` <- as.numeric(Master_File$`Month-1`) + as.numeric(Master_File$`Month-2`)
Master_File$`C_Month-3` <- as.numeric(Master_File$`C_Month-2`) + as.numeric(Master_File$`Month-2`)
Master_File$Average_3Months <- as.numeric(Master_File$`C_Month-3`)/3
Master_File[,"Max_3Months"] <- apply(Master_File[,4:6],1, max)
Master_File$`D_Month-2` <- as.numeric(Master_File$`Month-1`)/2 + as.numeric(Master_File$`Month-2`)
Master_File$`D_Month-3` <- as.numeric(Master_File$`D_Month-2`)/2 + as.numeric(Master_File$`Month-3`)
Master_File$`Sqrt_Month-1` <- sqrt(as.numeric(Master_File$`Month-1`))
Master_File$`Sqrt_Month-2` <- sqrt(Master_File$`C_Month-2`)
Master_File$`Sqrt_Month-3` <- sqrt(Master_File$`C_Month-3`)

Master_File$`Month-1` <- as.numeric(Master_File$`Month-1`)
Master_File$`Categ_Month-1` <- 'Low'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 2] <- 'Medium'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 7] <- 'High'
Master_File$`Month-2` <- as.numeric(Master_File$`Month-2`)
Master_File$`Categ_Month-2` <- 'Low'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 2] <- 'Medium'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 7] <- 'High'
Master_File$`Month-3` <- as.numeric(Master_File$`Month-3`)
Master_File$`Categ_Month-3` <- 'Low'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 2] <- 'Medium'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 7] <- 'High'

write.csv(Master_File, "../Ray Usage Attributes/To be Fed into Usage Model/Ray_PF.csv")

# Ray Payments #####################################################################################

Ray_Pay_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/4b6e9127-f355-9880-67c2-cb03ac18d6df")
Ray_Pay_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/99cc7010-5885-5320-5f02-28c7796e9a7e")
Ray_Pay_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/a34fe7bd-e102-6a30-475c-7ea35c116957")
Ray_Pay <- rbind(Ray_Pay_2014, Ray_Pay_2015, Ray_Pay_2016)

Ray_Pay$Reference <- Month_Mapping$Reference[match(Ray_Pay$date, Month_Mapping$Date)]
# Ray Pay #
Ray_Pay <- Ray_Pay[order(Ray_Pay$ray_practice_id, Ray_Pay$date),]
Ray_Pay$Indent[2:length(Ray_Pay$ray_practice_id)] <- Ray_Pay$count[1:length(Ray_Pay$ray_practice_id)-1]
Ray_Pay$Indent[is.na(Ray_Pay$Indent)] <- 0
Ray_Pay$Indent[Ray_Pay$Indent > 674] <- 674
Ray_Pay$count[Ray_Pay$count > 674] <- Ray_Pay$Indent[Ray_Pay$count > 674]
Ray_Pay$Indent <- NULL
# Ray Pay #
Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp <- subset(Ray_Pay, Ray_Pay$ray_practice_id == id)
  temp1 <- subset(temp, temp$Reference == (dt+1))
  temp2 <- subset(temp, temp$Reference == (dt+2))
  temp3 <- subset(temp,temp$Reference == (dt+3))
  temp4 <- rbind(temp1, temp2, temp3)
  if(nrow(temp4) == 0) {
    temp4 <- temp
  } else {
    temp4 <- aggregate(days~ray_practice_id, data = temp4, FUN = sum)
  }
  ##
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- 0)
  ifelse((nrow(temp2) != 0),Master_File$Time2[i] <- temp2$count[match(Master_File$ray_practice_id[i], temp2$ray_practice_id)], Master_File$Time2[i] <- 0)
  ifelse((nrow(temp3) != 0), Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)],Master_File$Time3[i] <- 0)
  ifelse((nrow(temp4) != 0), Master_File$days[i] <- temp4$days[match(Master_File$ray_practice_id[i], temp4$ray_practice_id)],Master_File$days[i] <- 0)
}

colnames(Master_File) <- c("ray_practice_id", "Type", "Reference", "Month-1", "Month-2", "Month-3", "Days_Used")
Master_File$`C_Month-2` <- as.numeric(Master_File$`Month-1`) + as.numeric(Master_File$`Month-2`)
Master_File$`C_Month-3` <- as.numeric(Master_File$`C_Month-2`) + as.numeric(Master_File$`Month-2`)
Master_File$Average_3Months <- as.numeric(Master_File$`C_Month-3`)/3
Master_File[,"Max_3Months"] <- apply(Master_File[,4:6],1, max)
Master_File$`D_Month-2` <- as.numeric(Master_File$`Month-1`)/2 + as.numeric(Master_File$`Month-2`)
Master_File$`D_Month-3` <- as.numeric(Master_File$`D_Month-2`)/2 + as.numeric(Master_File$`Month-3`)
Master_File$`Sqrt_Month-1` <- sqrt(as.numeric(Master_File$`Month-1`))
Master_File$`Sqrt_Month-2` <- sqrt(Master_File$`C_Month-2`)
Master_File$`Sqrt_Month-3` <- sqrt(Master_File$`C_Month-3`)

Master_File$`Month-1` <- as.numeric(Master_File$`Month-1`)
Master_File$`Categ_Month-1` <- 'Low'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 3] <- 'Medium'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 42] <- 'High'
Master_File$`Month-2` <- as.numeric(Master_File$`Month-2`)
Master_File$`Categ_Month-2` <- 'Low'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 3] <- 'Medium'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 42] <- 'High'
Master_File$`Month-3` <- as.numeric(Master_File$`Month-3`)
Master_File$`Categ_Month-3` <- 'Low'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 3] <- 'Medium'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 42] <- 'High'

write.csv(Master_File, "../Ray Usage Attributes/To be Fed into Usage Model/Ray_Pay.csv")

# Ray Soap Notes #####################################################################################

Ray_Soap_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/fbdd4f7e-7f43-8b90-5701-6002793372b6")
Ray_Soap_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/77a79d94-f44f-faf8-bbc6-e40d32590691")
Ray_Soap_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/9d9fc54a-25be-9cea-6a05-301eedd812b1")
Ray_Soap <- rbind(Ray_Soap_2014, Ray_Soap_2015, Ray_Soap_2016)

Ray_Soap$Reference <- Month_Mapping$Reference[match(Ray_Soap$date, Month_Mapping$Date)]
# Ray Soap #
Ray_Soap <- Ray_Soap[order(Ray_Soap$ray_practice_id, Ray_Soap$date),]
Ray_Soap$Indent[2:length(Ray_Soap$ray_practice_id)] <- Ray_Soap$count[1:length(Ray_Soap$ray_practice_id)-1]
Ray_Soap$Indent[is.na(Ray_Soap$Indent)] <- 0
Ray_Soap$Indent[Ray_Soap$Indent > 430] <- 430
Ray_Soap$count[Ray_Soap$count > 430] <- Ray_Soap$Indent[Ray_Soap$count > 430]
Ray_Soap$Indent <- NULL
# Ray Soap #
Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp <- subset(Ray_Soap, Ray_Soap$ray_practice_id == id)
  temp1 <- subset(temp, temp$Reference == (dt+1))
  temp2 <- subset(temp, temp$Reference == (dt+2))
  temp3 <- subset(temp,temp$Reference == (dt+3))
  temp4 <- rbind(temp1, temp2, temp3)
  if(nrow(temp4) == 0) {
    temp4 <- temp
  } else {
    temp4 <- aggregate(days~ray_practice_id, data = temp4, FUN = sum)
  }
  ##
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- 0)
  ifelse((nrow(temp2) != 0),Master_File$Time2[i] <- temp2$count[match(Master_File$ray_practice_id[i], temp2$ray_practice_id)], Master_File$Time2[i] <- 0)
  ifelse((nrow(temp3) != 0), Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)],Master_File$Time3[i] <- 0)
  ifelse((nrow(temp4) != 0), Master_File$days[i] <- temp4$days[match(Master_File$ray_practice_id[i], temp4$ray_practice_id)],Master_File$days[i] <- 0)
}

colnames(Master_File) <- c("ray_practice_id", "Type", "Reference", "Month-1", "Month-2", "Month-3", "Days_Used")
Master_File$`C_Month-2` <- as.numeric(Master_File$`Month-1`) + as.numeric(Master_File$`Month-2`)
Master_File$`C_Month-3` <- as.numeric(Master_File$`C_Month-2`) + as.numeric(Master_File$`Month-2`)
Master_File$Average_3Months <- as.numeric(Master_File$`C_Month-3`)/3
Master_File[,"Max_3Months"] <- apply(Master_File[,4:6],1, max)
Master_File$`D_Month-2` <- as.numeric(Master_File$`Month-1`)/2 + as.numeric(Master_File$`Month-2`)
Master_File$`D_Month-3` <- as.numeric(Master_File$`D_Month-2`)/2 + as.numeric(Master_File$`Month-3`)
Master_File$`Sqrt_Month-1` <- sqrt(as.numeric(Master_File$`Month-1`))
Master_File$`Sqrt_Month-2` <- sqrt(Master_File$`C_Month-2`)
Master_File$`Sqrt_Month-3` <- sqrt(Master_File$`C_Month-3`)

Master_File$`Month-1` <- as.numeric(Master_File$`Month-1`)
Master_File$`Categ_Month-1` <- 'Low'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 1] <- 'Medium'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 10] <- 'High'
Master_File$`Month-2` <- as.numeric(Master_File$`Month-2`)
Master_File$`Categ_Month-2` <- 'Low'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 1] <- 'Medium'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 10] <- 'High'
Master_File$`Month-3` <- as.numeric(Master_File$`Month-3`)
Master_File$`Categ_Month-3` <- 'Low'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 1] <- 'Medium'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 10] <- 'High'

write.csv(Master_File, "../Ray Usage Attributes/To be Fed into Usage Model/Ray_Soap.csv")

# Ray Patient Growth #####################################################################################

Ray_PG_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/7ae5abbb-643c-abea-1a44-28a4edf3ebd7")
Ray_PG_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/09428ea1-6f26-3967-5f17-fee62589f57d")
Ray_PG_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/94d54028-8914-9a6b-4fcf-5c59028fd6cd")
Ray_PG <- rbind(Ray_PG_2014, Ray_PG_2015, Ray_PG_2016)

Ray_PG$Reference <- Month_Mapping$Reference[match(Ray_PG$date, Month_Mapping$Date)]
# Ray PG #
Ray_PG <- Ray_PG[order(Ray_PG$ray_practice_id, Ray_PG$date),]
Ray_PG$Indent[2:length(Ray_PG$ray_practice_id)] <- Ray_PG$count[1:length(Ray_PG$ray_practice_id)-1]
Ray_PG$Indent[is.na(Ray_PG$Indent)] <- 0
Ray_PG$Indent[Ray_PG$Indent > 96] <- 96
Ray_PG$count[Ray_PG$count > 96] <- Ray_PG$Indent[Ray_PG$count > 96]
Ray_PG$Indent <- NULL
# Ray PG #
Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp <- subset(Ray_PG, Ray_PG$ray_practice_id == id)
  temp1 <- subset(temp, temp$Reference == (dt+1))
  temp2 <- subset(temp, temp$Reference == (dt+2))
  temp3 <- subset(temp,temp$Reference == (dt+3))
  temp4 <- rbind(temp1, temp2, temp3)
  if(nrow(temp4) == 0) {
    temp4 <- temp
  } else {
    temp4 <- aggregate(days~ray_practice_id, data = temp4, FUN = sum)
  }
  ##
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- 0)
  ifelse((nrow(temp2) != 0),Master_File$Time2[i] <- temp2$count[match(Master_File$ray_practice_id[i], temp2$ray_practice_id)], Master_File$Time2[i] <- 0)
  ifelse((nrow(temp3) != 0), Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)],Master_File$Time3[i] <- 0)
  ifelse((nrow(temp4) != 0), Master_File$days[i] <- temp4$days[match(Master_File$ray_practice_id[i], temp4$ray_practice_id)],Master_File$days[i] <- 0)
}

colnames(Master_File) <- c("ray_practice_id", "Type", "Reference", "Month-1", "Month-2", "Month-3", "Days_Used")
Master_File$`C_Month-2` <- as.numeric(Master_File$`Month-1`) + as.numeric(Master_File$`Month-2`)
Master_File$`C_Month-3` <- as.numeric(Master_File$`C_Month-2`) + as.numeric(Master_File$`Month-2`)
Master_File$Average_3Months <- as.numeric(Master_File$`C_Month-3`)/3
Master_File[,"Max_3Months"] <- apply(Master_File[,4:6],1, max)
Master_File$`D_Month-2` <- as.numeric(Master_File$`Month-1`)/2 + as.numeric(Master_File$`Month-2`)
Master_File$`D_Month-3` <- as.numeric(Master_File$`D_Month-2`)/2 + as.numeric(Master_File$`Month-3`)
Master_File$`Sqrt_Month-1` <- sqrt(as.numeric(Master_File$`Month-1`))
Master_File$`Sqrt_Month-2` <- sqrt(Master_File$`C_Month-2`)
Master_File$`Sqrt_Month-3` <- sqrt(Master_File$`C_Month-3`)

Master_File$`Month-1` <- as.numeric(Master_File$`Month-1`)
Master_File$`Categ_Month-1` <- 'Low'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 1] <- 'Medium'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 3] <- 'High'
Master_File$`Month-2` <- as.numeric(Master_File$`Month-2`)
Master_File$`Categ_Month-2` <- 'Low'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 1] <- 'Medium'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 3] <- 'High'
Master_File$`Month-3` <- as.numeric(Master_File$`Month-3`)
Master_File$`Categ_Month-3` <- 'Low'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 1] <- 'Medium'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 3] <- 'High'

write.csv(Master_File, "../Ray Usage Attributes/To be Fed into Usage Model/Ray_PG.csv")

# Ray Patient Vital Signs #####################################################################################

Ray_PVS_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/40de2901-5d24-79ae-8f34-5afb8067f11c")
Ray_PVS_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/29d6407f-865e-ca46-7a8a-dbf366919d8d")
Ray_PVS_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/1d54ed6f-f3ba-fd25-87ed-7bca6e598591")
Ray_PVS <- rbind(Ray_PVS_2014, Ray_PVS_2015, Ray_PVS_2016)

Ray_PVS$Reference <- Month_Mapping$Reference[match(Ray_PVS$date, Month_Mapping$Date)]
# Ray PVS #
Ray_PVS <- Ray_PVS[order(Ray_PVS$ray_practice_id, Ray_PVS$date),]
Ray_PVS$Indent[2:length(Ray_PVS$ray_practice_id)] <- Ray_PVS$count[1:length(Ray_PVS$ray_practice_id)-1]
Ray_PVS$Indent[is.na(Ray_PVS$Indent)] <- 0
Ray_PVS$Indent[Ray_PVS$Indent > 175] <- 175
Ray_PVS$count[Ray_PVS$count > 175] <- Ray_PVS$Indent[Ray_PVS$count > 175]
Ray_PVS$Indent <- NULL
# Ray PVS #
Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp <- subset(Ray_PVS, Ray_PVS$ray_practice_id == id)
  temp1 <- subset(temp, temp$Reference == (dt+1))
  temp2 <- subset(temp, temp$Reference == (dt+2))
  temp3 <- subset(temp,temp$Reference == (dt+3))
  temp4 <- rbind(temp1, temp2, temp3)
  if(nrow(temp4) == 0) {
    temp4 <- temp
  } else {
    temp4 <- aggregate(days~ray_practice_id, data = temp4, FUN = sum)
  }
  ##
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- 0)
  ifelse((nrow(temp2) != 0),Master_File$Time2[i] <- temp2$count[match(Master_File$ray_practice_id[i], temp2$ray_practice_id)], Master_File$Time2[i] <- 0)
  ifelse((nrow(temp3) != 0), Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)],Master_File$Time3[i] <- 0)
  ifelse((nrow(temp4) != 0), Master_File$days[i] <- temp4$days[match(Master_File$ray_practice_id[i], temp4$ray_practice_id)],Master_File$days[i] <- 0)
}

colnames(Master_File) <- c("ray_practice_id", "Type", "Reference", "Month-1", "Month-2", "Month-3", "Days_Used")
Master_File$`C_Month-2` <- as.numeric(Master_File$`Month-1`) + as.numeric(Master_File$`Month-2`)
Master_File$`C_Month-3` <- as.numeric(Master_File$`C_Month-2`) + as.numeric(Master_File$`Month-2`)
Master_File$Average_3Months <- as.numeric(Master_File$`C_Month-3`)/3
Master_File[,"Max_3Months"] <- apply(Master_File[,4:6],1, max)
Master_File$`D_Month-2` <- as.numeric(Master_File$`Month-1`)/2 + as.numeric(Master_File$`Month-2`)
Master_File$`D_Month-3` <- as.numeric(Master_File$`D_Month-2`)/2 + as.numeric(Master_File$`Month-3`)
Master_File$`Sqrt_Month-1` <- sqrt(as.numeric(Master_File$`Month-1`))
Master_File$`Sqrt_Month-2` <- sqrt(Master_File$`C_Month-2`)
Master_File$`Sqrt_Month-3` <- sqrt(Master_File$`C_Month-3`)

Master_File$`Month-1` <- as.numeric(Master_File$`Month-1`)
Master_File$`Categ_Month-1` <- 'Low'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 1] <- 'Medium'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 11] <- 'High'
Master_File$`Month-2` <- as.numeric(Master_File$`Month-2`)
Master_File$`Categ_Month-2` <- 'Low'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 1] <- 'Medium'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 11] <- 'High'
Master_File$`Month-3` <- as.numeric(Master_File$`Month-3`)
Master_File$`Categ_Month-3` <- 'Low'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 1] <- 'Medium'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 11] <- 'High'

write.csv(Master_File, "../Ray Usage Attributes/To be Fed into Usage Model/Ray_PVS.csv")

# Ray Patient Immunizations #####################################################################################

Ray_Imm_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/89ff2efc-cf70-1032-b9b2-825a9088ac81")
Ray_Imm_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/9d810948-5051-65e1-bcc0-072b1d1d7d34")
Ray_Imm <- rbind(Ray_Imm_2015, Ray_Imm_2016)

Ray_Imm$Reference <- Month_Mapping$Reference[match(Ray_Imm$date, Month_Mapping$Date)]
# Ray Imm #
Ray_Imm <- Ray_Imm[order(Ray_Imm$ray_practice_id, Ray_Imm$date),]
Ray_Imm$Indent[2:length(Ray_Imm$ray_practice_id)] <- Ray_Imm$count[1:length(Ray_Imm$ray_practice_id)-1]
Ray_Imm$Indent[is.na(Ray_Imm$Indent)] <- 0
Ray_Imm$Indent[Ray_Imm$Indent > 2778] <- 2778
Ray_Imm$count[Ray_Imm$count > 2778] <- Ray_Imm$Indent[Ray_Imm$count > 2778]
Ray_Imm$Indent <- NULL
# Ray Imm #
Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp <- subset(Ray_Imm, Ray_Imm$ray_practice_id == id)
  temp1 <- subset(temp, temp$Reference == (dt+1))
  temp2 <- subset(temp, temp$Reference == (dt+2))
  temp3 <- subset(temp,temp$Reference == (dt+3))
  temp4 <- rbind(temp1, temp2, temp3)
  if(nrow(temp4) == 0) {
    temp4 <- temp
  } else {
    temp4 <- aggregate(days~ray_practice_id, data = temp4, FUN = sum)
  }
  ##
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- 0)
  ifelse((nrow(temp2) != 0),Master_File$Time2[i] <- temp2$count[match(Master_File$ray_practice_id[i], temp2$ray_practice_id)], Master_File$Time2[i] <- 0)
  ifelse((nrow(temp3) != 0), Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)],Master_File$Time3[i] <- 0)
  ifelse((nrow(temp4) != 0), Master_File$days[i] <- temp4$days[match(Master_File$ray_practice_id[i], temp4$ray_practice_id)],Master_File$days[i] <- 0)
}

colnames(Master_File) <- c("ray_practice_id", "Type", "Reference", "Month-1", "Month-2", "Month-3", "Days_Used")
Master_File$`C_Month-2` <- as.numeric(Master_File$`Month-1`) + as.numeric(Master_File$`Month-2`)
Master_File$`C_Month-3` <- as.numeric(Master_File$`C_Month-2`) + as.numeric(Master_File$`Month-2`)
Master_File$Average_3Months <- as.numeric(Master_File$`C_Month-3`)/3
Master_File[,"Max_3Months"] <- apply(Master_File[,4:6],1, max)
Master_File$`D_Month-2` <- as.numeric(Master_File$`Month-1`)/2 + as.numeric(Master_File$`Month-2`)
Master_File$`D_Month-3` <- as.numeric(Master_File$`D_Month-2`)/2 + as.numeric(Master_File$`Month-3`)
Master_File$`Sqrt_Month-1` <- sqrt(as.numeric(Master_File$`Month-1`))
Master_File$`Sqrt_Month-2` <- sqrt(Master_File$`C_Month-2`)
Master_File$`Sqrt_Month-3` <- sqrt(Master_File$`C_Month-3`)

Master_File$`Month-1` <- as.numeric(Master_File$`Month-1`)
Master_File$`Categ_Month-1` <- 'Low'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 42] <- 'Medium'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 507] <- 'High'
Master_File$`Month-2` <- as.numeric(Master_File$`Month-2`)
Master_File$`Categ_Month-2` <- 'Low'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 42] <- 'Medium'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 507] <- 'High'
Master_File$`Month-3` <- as.numeric(Master_File$`Month-3`)
Master_File$`Categ_Month-3` <- 'Low'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 42] <- 'Medium'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 507] <- 'High'

write.csv(Master_File, "../Ray Usage Attributes/To be Fed into Usage Model/Ray_Imm.csv")

# Ray Appointments from ABS #####################################################################################

Ray_ABS_Appt_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/aa6fe72c-1e9c-83ba-62fd-7db2b2d5ca5b")
Ray_ABS_Appt_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/e8fe46aa-d138-ff0d-efa7-05117650252b")
Ray_ABS_Appt_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/d8f96169-a5d8-6f71-4986-a3ca6d3d0259")
Ray_ABS_Appt <- rbind(Ray_ABS_Appt_2014, Ray_ABS_Appt_2015, Ray_ABS_Appt_2016)

Ray_ABS_Appt$Reference <- Month_Mapping$Reference[match(Ray_ABS_Appt$date, Month_Mapping$Date)]
# Ray ABS Appt #
Ray_ABS_Appt <- Ray_ABS_Appt[order(Ray_ABS_Appt$ray_practice_id, Ray_ABS_Appt$date),]
Ray_ABS_Appt$Indent[2:length(Ray_ABS_Appt$ray_practice_id)] <- Ray_ABS_Appt$count[1:length(Ray_ABS_Appt$ray_practice_id)-1]
Ray_ABS_Appt$Indent[is.na(Ray_ABS_Appt$Indent)] <- 0
Ray_ABS_Appt$Indent[Ray_ABS_Appt$Indent > 131] <- 131
Ray_ABS_Appt$count[Ray_ABS_Appt$count > 131] <- Ray_ABS_Appt$Indent[Ray_ABS_Appt$count > 131]
Ray_ABS_Appt$Indent <- NULL
# Ray ABS Appt #
Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp <- subset(Ray_ABS_Appt, Ray_ABS_Appt$ray_practice_id == id)
  temp1 <- subset(temp, temp$Reference == (dt+1))
  temp2 <- subset(temp, temp$Reference == (dt+2))
  temp3 <- subset(temp,temp$Reference == (dt+3))
  temp4 <- rbind(temp1, temp2, temp3)
  if(nrow(temp4) == 0) {
    temp4 <- temp
  } else {
    temp4 <- aggregate(days~ray_practice_id, data = temp4, FUN = sum)
  }
  ##
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- 0)
  ifelse((nrow(temp2) != 0),Master_File$Time2[i] <- temp2$count[match(Master_File$ray_practice_id[i], temp2$ray_practice_id)], Master_File$Time2[i] <- 0)
  ifelse((nrow(temp3) != 0), Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)],Master_File$Time3[i] <- 0)
  ifelse((nrow(temp4) != 0), Master_File$days[i] <- temp4$days[match(Master_File$ray_practice_id[i], temp4$ray_practice_id)],Master_File$days[i] <- 0)
}

colnames(Master_File) <- c("ray_practice_id", "Type", "Reference", "Month-1", "Month-2", "Month-3", "Days_Used")
Master_File$`C_Month-2` <- as.numeric(Master_File$`Month-1`) + as.numeric(Master_File$`Month-2`)
Master_File$`C_Month-3` <- as.numeric(Master_File$`C_Month-2`) + as.numeric(Master_File$`Month-2`)
Master_File$Average_3Months <- as.numeric(Master_File$`C_Month-3`)/3
Master_File[,"Max_3Months"] <- apply(Master_File[,4:6],1, max)
Master_File$`D_Month-2` <- as.numeric(Master_File$`Month-1`)/2 + as.numeric(Master_File$`Month-2`)
Master_File$`D_Month-3` <- as.numeric(Master_File$`D_Month-2`)/2 + as.numeric(Master_File$`Month-3`)
Master_File$`Sqrt_Month-1` <- sqrt(as.numeric(Master_File$`Month-1`))
Master_File$`Sqrt_Month-2` <- sqrt(Master_File$`C_Month-2`)
Master_File$`Sqrt_Month-3` <- sqrt(Master_File$`C_Month-3`)

Master_File$`Month-1` <- as.numeric(Master_File$`Month-1`)
Master_File$`Categ_Month-1` <- 'Low'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 2] <- 'Medium'
Master_File$`Categ_Month-1`[Master_File$`Month-1` > 6] <- 'High'
Master_File$`Month-2` <- as.numeric(Master_File$`Month-2`)
Master_File$`Categ_Month-2` <- 'Low'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 2] <- 'Medium'
Master_File$`Categ_Month-2`[Master_File$`Month-2` > 6] <- 'High'
Master_File$`Month-3` <- as.numeric(Master_File$`Month-3`)
Master_File$`Categ_Month-3` <- 'Low'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 2] <- 'Medium'
Master_File$`Categ_Month-3`[Master_File$`Month-3` > 6] <- 'High'

Master_File$`Incre_Month-2` <- Master_File$`Month-2` - Master_File$`Month-1`
Master_File$`Incre_Month-3` <- Master_File$`Month-3` - Master_File$`Month-2`

Master_File$Days_Used <- Master_File$Days_Used

Master_File$`Sqrt_Average_3Months` <- sqrt(Master_File$Average_3Months)

Master_File$`Categ_Month-1-High`[Master_File$`Categ_Month-1` == "High"] <- 1
Master_File$`Categ_Month-1-High`[is.na(Master_File$`Categ_Month-1-High`)] <- 0
Master_File$`Categ_Month-2-High`[Master_File$`Categ_Month-2` == "High"] <- 1
Master_File$`Categ_Month-2-High`[is.na(Master_File$`Categ_Month-2-High`)] <- 0
Master_File$`Categ_Month-3-High`[Master_File$`Categ_Month-3` == "High"] <- 1
Master_File$`Categ_Month-3-High`[is.na(Master_File$`Categ_Month-3-High`)] <- 0

Master_File$`Categ_Month-1-Low`[Master_File$`Categ_Month-1` == "Low"] <- 1
Master_File$`Categ_Month-1-Low`[is.na(Master_File$`Categ_Month-1-Low`)] <- 0
Master_File$`Categ_Month-2-Low`[Master_File$`Categ_Month-2` == "Low"] <- 1
Master_File$`Categ_Month-2-Low`[is.na(Master_File$`Categ_Month-2-Low`)] <- 0
Master_File$`Categ_Month-3-Low`[Master_File$`Categ_Month-3` == "Low"] <- 1
Master_File$`Categ_Month-3-Low`[is.na(Master_File$`Categ_Month-3-Low`)] <- 0

Master_File$`Log1_C_Month-1` <-  log(Master_File$`Month-1` + 1)
Master_File$`Log1_C_Month-2`  <- log(Master_File$`C_Month-2` + 1)
Master_File$`Log1_C_Month-3`  <- log(Master_File$`C_Month-3` + 1)

Master_File$`Log_Average_3Months` <- log(Master_File$Average_3Months + 1)

write.csv(Master_File, "../Ray Usage Attributes/To be Fed into Usage Model/Ray_ABS_Appt.csv")

# Aggregation into 1 df IMPORTANT #################################################################################

directory <- "../Ray Usage Attributes/To be Fed into Usage Model/"
file_list <- list.files(directory, pattern="Ray_*", full.names=TRUE)

for(i in 1:length(file_list)){
  temp<-read.csv(file_list[i],stringsAsFactors = F)[,-1]
  colnames(temp)[4:ncol(temp)] <- paste0(substr(file_list[i],52, nchar(file_list[i])),colnames(temp)[4:ncol(temp)])
  if(i==1){
  df<-temp
  }
  if(i!=1){
    temp$Type<-temp$Reference<-NULL
    df<-merge(df,temp, by = "ray_practice_id", sort = F)
  }
}

# Correlation M.A.T.R.I.X. ###########################################################

usage <- read.csv("../Ray Usage Attributes/To be Fed into Usage Model/Ray_ABS_Appt.csv", stringsAsFactors = F)[,-1]
usage$Type[usage$Type == "Hunt"] <- 0
usage$Type[usage$Type == "Upsell"] <- 1

# cols.num <- c("Type", "Reference","Month.1","Month.2","Month.3","Days_Used","C_Month.2","C_Month.3","Average_3Months","Max_3Months","D_Month.2","D_Month.3","Sqrt_Month.1","Sqrt_Month.2","Sqrt_Month.3","Categ_Month.1","Categ_Month.2","Categ_Month.3")
cols.num <- c("Type","Month.3","Incre_Month.2","Incre_Month.3","Categ_Month.1.High","Categ_Month.2.High","Categ_Month.3.High","Categ_Month.1.Low","Categ_Month.2.Low","Categ_Month.3.Low","Log_Average_3Months")
temp <- usage
temp$ray_practice_id<- temp$Reference <- temp$Categ_Month.1 <- temp$Categ_Month.2 <- temp$Categ_Month.3 <- temp$Month.1 <- temp$Month.2 <-temp$Sqrt_Month.1 <- temp$Sqrt_Month.2 <- temp$Sqrt_Month.3 <- temp$Average_3Months <- temp$Days_Used <- temp$Log1_C_Month.1 <- temp$Log1_C_Month.2 <- temp$Log1_C_Month.3 <- temp$C_Month.2 <- temp$C_Month.3 <- temp$Sqrt_Average_3Months <- temp$D_Month.2 <- temp$D_Month.3 <- temp$Max_3Months <- NULL

temp[cols.num] <- sapply(temp[cols.num], as.numeric)
sapply(temp, class)
write.csv(cor(temp),"../Ray Usage Attributes/To be Fed into Usage Model/Correlation - v12.csv")

##### 9. LR + RANDOM_FOREST + TREE_BASED (R.E.G.R.E.S.S.I.O.N) ----
LogSummary<-list()
RanImportance<-list()
DecisionTree<-list()
LRModel<-list()
RFModel<-list()
CARTModel<-list()

Comped<-data.frame(Run=integer(),
                   LRAccuracy=numeric(),
                   LRSpecif=numeric(),
                   LRSensit=numeric(),
                   RFAccuracy=numeric(),
                   RFSpecif=numeric(),
                   RFSensit=numeric(),
                   DTAccuracy=numeric(),
                   DTSpecif=numeric(),
                   DTSensit=numeric()
)
## Removing Practice_ID + CITY + THREE ATTRIBUTES (coz they assume a single value) --
R_101 <- read.csv("../Ray Usage Attributes/To be Fed into Usage Model/Ray_ABS_Appt.csv", stringsAsFactors = F)[,-1]
R_101$Type[R_101$Type == "Hunt"] <- 0
R_101$Type[R_101$Type == "Upsell"] <- 1

t <- as.data.frame(R_101$ray_practice_id)
R_101$ray_practice_id<-NULL
R_101$Reference <- R_101$Categ_Month.1 <- R_101$Categ_Month.2 <- R_101$Categ_Month.3 <- R_101$Month.1 <- R_101$Month.2 <-R_101$Sqrt_Month.1 <- R_101$Sqrt_Month.2 <- R_101$Sqrt_Month.3 <- R_101$Average_3Months <- R_101$Days_Used <- R_101$Log1_C_Month.1 <- R_101$Log1_C_Month.2 <- R_101$Log1_C_Month.3 <- R_101$C_Month.2 <- R_101$C_Month.3 <- R_101$Sqrt_Average_3Months <- R_101$D_Month.2 <- R_101$D_Month.3 <- R_101$Max_3Months <- R_101$Categ_Month.1.High <- R_101$Categ_Month.2.High <- R_101$Categ_Month.1.Low <- R_101$Categ_Month.2.Low <- R_101$Month.3 <- NULL
temp2<-R_101
temp2$NewDV <- temp2$Type
temp2$Type <- NULL
temp2$NewDV <- as.numeric(temp2$NewDV)
for(i in 1:10){
  print(i)
  Comped[i,1]<-i
  
  
  #----------- Partial Sampling
  temp2 <- temp2[sample(nrow(temp2)),]
  set.seed(1)
  split <- sample.split(temp2$NewDV, SplitRatio = 0.70)
  
  train<-subset(temp2,split==T)
  test<- subset(temp2,split==F)
  
  LRModel[[i]]<-Logistic <- glm(NewDV ~.,family=binomial(link='logit'),data=train)
  LogSummary[[i]]<- summary(Logistic)
  # write.csv(train,"../train.csv",row.names = F)
  # write.csv(test,"../test.csv",row.names = F)
  #anova(model,test = "Chisq")
  
  l<-as.data.frame(predict(Logistic,newdata=test,type='response'))
  # l<-as.data.frame(predict(Logistic,newdata=temp2,type='response'))
  
  colnames(l)<-'fitted.results'
  
  var<-unlist(mean(temp2$NewDV))
  l$prob<-ifelse(l$fitted.results>var,1,0)
  # l$prob<-ifelse(l$fitted.results>0.5,1,0)
  l$NewDV<-test$NewDV
  # l$Customer<-temp2$Customer
  l$fitted.results<-NULL
  Comped[i,2]<-accuracy <- mean(l$prob == test$NewDV)
  # Comped[i,2]<-accuracy <- mean(l$prob == temp2$Customer)
  
  l<-count(l,c('prob','NewDV'))
  
  Comped[i,3]<-l[1,3]/(l[1,3]+l[2,3]) #Spec
  
  Comped[i,4]<-l[4,3]/(l[3,3]+l[4,3]) #Sens
  
  Comped$LRAccuracy <- (l[1,3]+l[4,3])/(l[1,3]+l[2,3]+l[3,3]+l[4,3])
  
  # ---------------------- Random Forest
  #   RFModel[[i]] <-RandomForestModel <- suppressWarnings(randomForest(Customer ~., data=train, ntree=70,importance=T))
  #   
  #   l<-as.data.frame(predict(RandomForestModel,newdata=test,type='response'))
  #   colnames(l)<-'fitted.results'
  #   
  #   l$prob<-ifelse(l$fitted.results>var,1,0)
  #   l$Customer<-test$Customer
  #   l$fitted.results<-NULL
  #   Comped[i,5]<-accuracy <- mean(l$prob == test$Customer)
  #   l<-count(l,c('prob','Customer'))
  #   #summary(RandomForestModel)
  #   #RanImportance[[i]]<-importance(RandomForestModel)
  #   Comped[i,6]<-l[1,3]/(l[1,3]+l[2,3])
  #   
  #   Comped[i,7]<-l[4,3]/(l[3,3]+l[4,3])
  
  #   #------ Tree split
  # tree<-rpart(Churn ~., data=train, method='anova',control=rpart.control(minsplit=200))
  #   tree<-rpart(Customer ~., data=train)#,control=rpart.control(minsplit=200)
  #   tree<-prune(tree,cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
  #   
  #   l<-as.data.frame(predict(tree,newdata=test))
  #   colnames(l)<-'fitted.results'
  #   
  #   l$prob<-ifelse(l$fitted.results>var,1,0)
  #   l$Customer<-test$Customer
  #   l$fitted.results<-NULL
  #   Comped[i,8]<-accuracy <- mean(l$prob == test$Customer)
  #   l<-count(l,c('prob','Customer'))
  #   
  #   Comped[i,9]<-l[1,3]/(l[1,3]+l[2,3])
  #   
  #   Comped[i,10]<-l[4,3]/(l[3,3]+l[4,3])
  #   
  #   summary(tree)
  #   
  #   DecisionTree[[i]]<- prp(tree,type=2,extra=1)
}
mean(Comped$LRAccuracy)
mean(Comped$RFAccuracy)
mean(Comped$DTAccuracy)
max(Comped$LRAccuracy)
## Finding Correlation & Removing the Attributes with HIGH CORRELATION --
cols.num <- c("has_organization", "has_software", "has_parking", "has_wifi", "has_computer", "has_ac", "has_receptionist")
temp <- R_101[1:7]
temp[cols.num] <- sapply(temp[cols.num], as.numeric)
sapply(temp, class)
write.csv(cor(temp),"../Correlation - v1.csv")
## HIGH Correlation in TWO ATTRIBUTES - has_computer & has_internet, so removing the attribute of has_internet --
R_101$has_internet <- NULL
## Running the model again --
##########################################################################
##### 10. Matching the PROBABILITY SCORES against the Lead List ----
l<-as.data.frame(predict(Logistic,newdata=R_101,type='response'))
colnames(l)<-'fitted.results'
temp_final <- cbind(t, R_101, l)
temp_final$fitted.results <- temp_final$fitted.results*100
write.csv(temp_final, "../Results - Model - 29k.csv")
##########################################################################

# Recommendations #####################################################################################

Ray_Reco <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/8c2c6f54-a096-fc42-58c3-5e0caddbe9ff")

Ray_Reco$Reference <- Month_Mapping$Reference[match(Ray_Reco$date, Month_Mapping$Date)]

raw <- expand.grid(Master_File[['ray_practice_id']],Dates[['date']])
colnames(raw) <- c("ray_practice_id", "date")
raw$conc <- paste0(raw$ray_practice_id, sep="-", raw$date)

Ray_Reco$conc <- paste0(Ray_Reco$ray_practice_id, sep = "-", Ray_Reco$Reference)
raw$Ray_Reco <- Ray_Reco$count[match(raw$conc, Ray_Reco$conc)]
raw$Ray_Reco[is.na(raw$Ray_Reco)] <- 0

raw$Reference <- Master_File$Reference[match(raw$ray_practice_id, Master_File$ray_practice_id)]
raw$upsell <- Master_File$Upsell[match(raw$ray_practice_id, Master_File$ray_practice_id)]
raw <- subset(raw, raw$date >= raw$Reference)
raw <- subset(raw, raw$date <= raw$upsell)

temp_Reco <- aggregate(Ray_Reco~ray_practice_id, data = raw, FUN = mean)

Master_File$Reco <- temp_Reco$Ray_Reco[match(Master_File$ray_practice_id, temp_Reco$ray_practice_id)]

Master_File$Categ_Reco <- ifelse(Master_File$Reco >= 50, "High", ifelse(Master_File$Reco >= 5, "Medium", "Low"))
Master_File$Categ_Reco_High_Medium[Master_File$Categ_Reco == "High" | Master_File$Categ_Reco == "Medium"] <- 1
Master_File$Categ_Reco_High_Medium[is.na(Master_File$Categ_Reco_High_Medium)] <- 0
Master_File$Categ_Reco <- NULL

Master_File$Reco_Review <- Master_File$Reco/Master_File$Review
Master_File$Reco_Review[is.infinite(Master_File$Reco_Review)] <- 0
Master_File$Reco_Review[is.na(Master_File$Reco_Review)] <- 0

# Reviews #####################################################################################

Ray_Review <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/472b243a-93f8-9677-e75b-c35e487767eb")

Ray_Review$Reference <- Month_Mapping$Reference[match(Ray_Review$date, Month_Mapping$Date)]

raw <- expand.grid(Master_File[['ray_practice_id']],Dates[['date']])
colnames(raw) <- c("ray_practice_id", "date")
raw$conc <- paste0(raw$ray_practice_id, sep="-", raw$date)

Ray_Review$conc <- paste0(Ray_Review$ray_practice_id, sep = "-", Ray_Review$Reference)
raw$Ray_Review <- Ray_Review$count[match(raw$conc, Ray_Review$conc)]
raw$Ray_Review[is.na(raw$Ray_Review)] <- 0

raw$Reference <- Master_File$Reference[match(raw$ray_practice_id, Master_File$ray_practice_id)]
raw$upsell <- Master_File$Upsell[match(raw$ray_practice_id, Master_File$ray_practice_id)]
raw <- subset(raw, raw$date >= raw$Reference)
raw <- subset(raw, raw$date <= raw$upsell)

temp_Review <- aggregate(Ray_Review~ray_practice_id, data = raw, FUN = mean)

Master_File$Review <- temp_Review$Ray_Review[match(Master_File$ray_practice_id, temp_Review$ray_practice_id)]

Master_File$Categ_Review <- ifelse(Master_File$Review >= 50, "High", ifelse(Master_File$Review >= 5, "Medium", "Low"))
Master_File$Categ_Review_High_Medium[Master_File$Categ_Review == "High" | Master_File$Categ_Review == "Medium"] <- 1
Master_File$Categ_Review_High_Medium[is.na(Master_File$Categ_Review_High_Medium)] <- 0
Master_File$Categ_Review <- NULL

Master_File$Log_Review <- log(Master_File$Review + 1)

Master_File$Review_Reco <- Master_File$Review/(Master_File$Reco + 1)
Master_File$Review_Reco[is.infinite(Master_File$Review_Reco)] <- 0
Master_File$Review_Reco[is.na(Master_File$Review_Reco)] <- 0

# ABS Appointments #####################################################################################

Ray_ABS_Appt <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/0e2b93f4-0345-0bdb-df9b-b33ea3793296")

Ray_ABS_Appt$Reference <- Month_Mapping$Reference[match(Ray_ABS_Appt$date, Month_Mapping$Date)]

raw <- expand.grid(Master_File[['ray_practice_id']],Dates[['date']])
colnames(raw) <- c("ray_practice_id", "date")
raw$conc <- paste0(raw$ray_practice_id, sep="-", raw$date)

Ray_ABS_Appt$conc <- paste0(Ray_ABS_Appt$ray_practice_id, sep = "-", Ray_ABS_Appt$Reference)
raw$Ray_ABS_Appt <- Ray_ABS_Appt$count[match(raw$conc, Ray_ABS_Appt$conc)]
raw$Ray_ABS_Appt[is.na(raw$Ray_ABS_Appt)] <- 0

raw$Reference <- Master_File$Reference[match(raw$ray_practice_id, Master_File$ray_practice_id)]
raw$upsell <- Master_File$Upsell[match(raw$ray_practice_id, Master_File$ray_practice_id)]
raw <- subset(raw, raw$date >= raw$Reference)
raw <- subset(raw, raw$date <= raw$upsell)

temp_ABS_Appt <- aggregate(Ray_ABS_Appt~ray_practice_id, data = raw, FUN = mean)

Master_File$ABS_Appt <- temp_ABS_Appt$Ray_ABS_Appt[match(Master_File$ray_practice_id, temp_ABS_Appt$ray_practice_id)]

Master_File$Categ_ABS_Appt <- ifelse(Master_File$ABS_Appt >= 50, "High", ifelse(Master_File$ABS_Appt >= 5, "Medium", "Low"))
Master_File$Categ_ABS_Appt_High_Medium[Master_File$Categ_ABS_Appt == "High" | Master_File$Categ_ABS_Appt == "Medium"] <- 1
Master_File$Categ_ABS_Appt_High_Medium[is.na(Master_File$Categ_ABS_Appt_High_Medium)] <- 0
Master_File$Categ_ABS_Appt <- NULL

Master_File$Log_ABS_Appt <- log(Master_File$ABS_Appt + 1)

# mean(Master_File$Reco)

# VN Calls #####################################################################################

Ray_VN <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/2c81afc5-9843-6d6e-6c09-642e3a0670eb")

Ray_VN$Reference <- Month_Mapping$Reference[match(Ray_VN$date, Month_Mapping$Date)]

raw <- expand.grid(Master_File[['ray_practice_id']],Dates[['date']])
colnames(raw) <- c("ray_practice_id", "date")
raw$conc <- paste0(raw$ray_practice_id, sep="-", raw$date)

Ray_VN$conc <- paste0(Ray_VN$ray_practice_id, sep = "-", Ray_VN$Reference)
raw$Ray_VN <- Ray_VN$count[match(raw$conc, Ray_VN$conc)]
raw$Ray_VN[is.na(raw$Ray_VN)] <- 0

raw$Reference <- Master_File$Reference[match(raw$ray_practice_id, Master_File$ray_practice_id)]
raw$upsell <- Master_File$Upsell[match(raw$ray_practice_id, Master_File$ray_practice_id)]
raw <- subset(raw, raw$date >= raw$Reference)
raw <- subset(raw, raw$date <= raw$upsell)

temp_VN <- aggregate(Ray_VN~ray_practice_id, data = raw, FUN = mean)

Master_File$VN <- temp_VN$Ray_VN[match(Master_File$ray_practice_id, temp_VN$ray_practice_id)]

Master_File$ABS_VN <- Master_File$ABS_Appt + Master_File$VN
Master_File$ABS_VN_2 <- Master_File$ABS_Appt + (Master_File$VN)/2

##### Master_File_1 AND Master_File_2 ##### -----

Master_File_1 <- read.csv("../Master_File_1.csv", stringsAsFactors = F)[,-1]
Master_File_2 <- read.csv("../Master_File_2.csv", stringsAsFactors = F)[,-1]

Master_File <- merge(Master_File_1, Master_File_2, by = c("ray_practice_id", "Reference", "Upsell", "Type") , all.x = T)
Master_File$Calendar <- Master_File$EMR <- Master_File$Billing <- NULL

##### Previous + Practo_Profiles -----

Churn_Conv <- read.csv("../all_data_churn_conversion.csv")

Master_File$Conv_Prob_Score <- Churn_Conv$conversion_prob_score[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$Churn_Prob_Score <- Churn_Conv$churn_prob_score[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$Doc_Count <- Churn_Conv$doctor_count[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$Consult_Fee_Mean <- Churn_Conv$consultation_fee_mean[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$Female_Doc <- Churn_Conv$female_doctor[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$Exp_Mean <- Churn_Conv$experience_mean[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$Qual_Count_Mean <- Churn_Conv$qualification_count_mean[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$Loc_Factor <- Churn_Conv$locality_factor[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$City_Factor <- Churn_Conv$city_factor[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$Clinic_Photos <- Churn_Conv$clinic_photos[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$Doc_Photos_Mean <- Churn_Conv$doctor_photos_mean[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$Org_Mean <- Churn_Conv$organizations_mean[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$DQS_Mean <- Churn_Conv$dqs_mean[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$Multi_Spec <- Churn_Conv$multi_spec[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$Dental <- Churn_Conv$has_dental[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$GP <- Churn_Conv$has_gp[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$Physio <- Churn_Conv$has_physio[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$Altmed <- Churn_Conv$has_altmed[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]
Master_File$Surgeon <- Churn_Conv$has_surgeon[match(Master_File$ray_practice_id, Churn_Conv$ray_practice_id)]

write.csv(Master_File, "../Master_File_F.csv")

## Speciality Merge with Practo_Profiles -----

PP <- read.csv("../../doctor_profile_2016-07-03complete.csv")
Spec_Map <- read.csv("../../Spec_Mapping.csv")

PP$Upsell_Model <- Master_File$ray_practice_id[match(PP$ray_practice_id, Master_File$ray_practice_id)]
PP <- subset(PP, !is.na(PP$Upsell_Model))

temp <- as.data.frame(unique(PP$speciality))
temp$Grp_Spec <- Spec_Map$Group[match(temp$`unique(PP$speciality)`, Spec_Map$practicespecialization)]
temp$Grp_Spec[grepl('cosmet', tolower(temp$`unique(PP$speciality)`))] <- "Dermat"
temp$Grp_Spec[grepl('gyn', tolower(temp$`unique(PP$speciality)`))] <- "Gynaec"
temp$Grp_Spec[grepl('obstr', tolower(temp$`unique(PP$speciality)`))] <- "Gynaec"

PP$speciality_grp <- temp$Grp_Spec[match(PP$speciality, temp$`unique(PP$speciality)`)]
PP <- subset(PP, !is.na(PP$speciality_grp))

PP$has_dermat[PP$speciality_grp == "Dermat"] <- 1
PP$has_dermat[is.na(PP$has_dermat)] <- 0

PP$has_pedia[PP$speciality_grp == "Pedia"] <- 1
PP$has_pedia[is.na(PP$has_pedia)] <- 0

PP$has_gynaec[PP$speciality_grp == "Gynaec"] <- 1
PP$has_gynaec[is.na(PP$has_gynaec)] <- 0

Master_File$Dermat <- PP$has_dermat[match(Master_File$ray_practice_id, PP$ray_practice_id)]
Master_File$Pedia <- PP$has_pedia[match(Master_File$ray_practice_id, PP$ray_practice_id)]
Master_File$Gynaec <- PP$has_gynaec[match(Master_File$ray_practice_id, PP$ray_practice_id)]

Master_File[is.na(Master_File)] <- 0
