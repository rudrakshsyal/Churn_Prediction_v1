# Preparation for Populating Attributes #####################################################################################

Master_File <- read.csv("../Master_File.csv")
Ray_Customers <- read.csv("C:/Users/Practo/Dropbox/Ray Customers/Ray_Customers.csv")

Master_File$Hunting_Date <- Ray_Customers$hunting_date[match(Master_File$ray_practice_id, Ray_Customers$ray_practice_id)]
Master_File <- subset(Master_File, as.Date(Master_File$Hunting_Date) >= as.Date("2015-01-01"))
Master_File$Month <- ifelse((month(as.Date(Master_File$Hunting_Date))<10), paste0("0",month(as.Date(Master_File$Hunting_Date))), month(as.Date(Master_File$Hunting_Date)))
Master_File$Year <- year(as.Date(Master_File$Hunting_Date))
Master_File$Hunting_Date <- paste0(Master_File$Year, "-", Master_File$Month)
Master_File$Month <- Master_File$Year <- NULL

Month_Mapping <- read.csv("../Month_Mapping.csv")
Master_File$Reference <- Month_Mapping$Reference[match(Master_File$Hunting_Date, Month_Mapping$Date)]

# Ray Appointments #####################################################################################

Ray_Appnts_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/5d372ea8-a1ae-fad0-e353-665bfa883ffb", stringsAsFactors = F)
Ray_Appnts_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/22d69c64-0833-e07e-82b9-54b6c96a4f80", stringsAsFactors = F)
Ray_Appnts_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/9d56f33c-9f26-f121-14f3-643ac93b03f3", stringsAsFactors = F)
Ray_Appnts <- rbind(Ray_Appnts_2014, Ray_Appnts_2015, Ray_Appnts_2016)
Ray_Appnts_2014 <- Ray_Appnts_2015 <- Ray_Appnts_2016 <- NULL
Ray_Appnts$Reference <- Month_Mapping$Reference[match(Ray_Appnts$date, Month_Mapping$Date)]

temp <- cast(Ray_Appnts, ray_practice_id~date, mean, value = "count")

Master_File <- Master_File[,1:5]
Master_File <- merge(Master_File, temp, "ray_practice_id", all.x = T)
Master_File[is.na(Master_File)] <- 0

write.csv(Master_File, "../Ray Usage Attributes/Usage Specific/MoM Distribution - Imputation/Ray_Appnts.csv")

# Ray Appointment Files #####################################################################################

Ray_Appnt_Files_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/64de9af8-6058-dbdd-6ca7-2c381ddbd38a")
Ray_Appnt_Files_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/e1b3adae-eb5e-9a67-ce1a-8f0da2ed07fd")
Ray_Appnt_Files_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/cb9c35ca-2401-76c2-0f44-5764b1adaeb8")
Ray_Appnt_Files <- rbind(Ray_Appnt_Files_2014, Ray_Appnt_Files_2015, Ray_Appnt_Files_2016)
Ray_Appnt_Files_2014 <- Ray_Appnt_Files_2015 <- Ray_Appnt_Files_2016 <- NULL
Ray_Appnt_Files$Reference <- Month_Mapping$Reference[match(Ray_Appnt_Files$date, Month_Mapping$Date)]

temp <- cast(Ray_Appnt_Files, ray_practice_id~date, mean, value = "count")

Master_File <- Master_File[,1:5]
Master_File <- merge(Master_File, temp, "ray_practice_id", all.x = T)
Master_File[is.na(Master_File)] <- 0

write.csv(Master_File, "../Ray Usage Attributes/Usage Specific/MoM Distribution - Imputation/Ray_Appnt_Files.csv")

# Ray Prescriptions #####################################################################################

Ray_Prescrip_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/e42e3b54-c35e-caf7-3bf1-659f7c4a91e4")
Ray_Prescrip_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/3c5a1d3a-52ec-f0a9-ef7e-bbe5b56b5786")
Ray_Prescrip_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/1a646a1b-c174-ec0e-ff19-88b1c2ee2ea8")
Ray_Prescrip <- rbind(Ray_Prescrip_2014, Ray_Prescrip_2015, Ray_Prescrip_2016)
Ray_Prescrip_2014 <- Ray_Prescrip_2015 <- Ray_Prescrip_2016 <- NULL
Ray_Prescrip$Reference <- Month_Mapping$Reference[match(Ray_Prescrip$date, Month_Mapping$Date)]

temp <- cast(Ray_Prescrip, ray_practice_id~date, mean, value = "count")

Master_File <- Master_File[,1:5]
Master_File <- merge(Master_File, temp, "ray_practice_id", all.x = T)
Master_File[is.na(Master_File)] <- 0

write.csv(Master_File, "../Ray Usage Attributes/Usage Specific/MoM Distribution - Imputation/Ray_Prescrip.csv")

# Ray Invoices #####################################################################################

Ray_Invoices_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/7823ac65-f5d4-11ac-7d02-94ac1ec7ae8b")
Ray_Invoices_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/d4979832-598c-1acb-c160-e82cd7d72c7f")
Ray_Invoices_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/95a56c84-1271-3818-e5e2-e62b141526a3")
Ray_Invoices <- rbind(Ray_Invoices_2014, Ray_Invoices_2015, Ray_Invoices_2016)
Ray_Invoices_2014 <- Ray_Invoices_2015 <- Ray_Invoices_2016 <- NULL
Ray_Invoices$Reference <- Month_Mapping$Reference[match(Ray_Invoices$date, Month_Mapping$Date)]

temp <- cast(Ray_Invoices, ray_practice_id~date, mean, value = "count")

Master_File <- Master_File[,1:5]
Master_File <- merge(Master_File, temp, "ray_practice_id", all.x = T)
Master_File[is.na(Master_File)] <- 0

write.csv(Master_File, "../Ray Usage Attributes/Usage Specific/MoM Distribution - Imputation/Ray_Invoices.csv")

# Ray Patient Files #####################################################################################

Ray_PF_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/df7cf62d-80b0-eeb3-637b-b4f6cdc91b0b")
Ray_PF_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/dadbd34a-2e1b-1349-c719-ba11e7ad063e")
Ray_PF_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/907413f9-6fe9-21e6-b8e0-9045d6336a41")
Ray_PF <- rbind(Ray_PF_2014, Ray_PF_2015, Ray_PF_2016)
Ray_PF_2014 <- Ray_PF_2015 <- Ray_PF_2016 <- NULL
Ray_PF$Reference <- Month_Mapping$Reference[match(Ray_PF$date, Month_Mapping$Date)]

temp <- cast(Ray_PF, ray_practice_id~date, mean, value = "count")

Master_File <- Master_File[,1:5]
Master_File <- merge(Master_File, temp, "ray_practice_id", all.x = T)
Master_File[is.na(Master_File)] <- 0

write.csv(Master_File, "../Ray Usage Attributes/Usage Specific/MoM Distribution - Imputation/Ray_PF.csv")

# Ray Payments #####################################################################################

Ray_Pay_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/4b6e9127-f355-9880-67c2-cb03ac18d6df")
Ray_Pay_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/99cc7010-5885-5320-5f02-28c7796e9a7e")
Ray_Pay_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/a34fe7bd-e102-6a30-475c-7ea35c116957")
Ray_Pay <- rbind(Ray_Pay_2014, Ray_Pay_2015, Ray_Pay_2016)
Ray_Pay_2014 <- Ray_Pay_2015 <- Ray_Pay_2016 <- NULL
Ray_Pay$Reference <- Month_Mapping$Reference[match(Ray_Pay$date, Month_Mapping$Date)]

temp <- cast(Ray_Pay, ray_practice_id~date, mean, value = "count")

Master_File <- Master_File[,1:5]
Master_File <- merge(Master_File, temp, "ray_practice_id", all.x = T)
Master_File[is.na(Master_File)] <- 0

write.csv(Master_File, "../Ray Usage Attributes/Usage Specific/MoM Distribution - Imputation/Ray_Pay.csv")

# Ray Soap Notes #####################################################################################

Ray_Soap_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/fbdd4f7e-7f43-8b90-5701-6002793372b6")
Ray_Soap_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/77a79d94-f44f-faf8-bbc6-e40d32590691")
Ray_Soap_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/9d9fc54a-25be-9cea-6a05-301eedd812b1")
Ray_Soap <- rbind(Ray_Soap_2014, Ray_Soap_2015, Ray_Soap_2016)
Ray_Soap_2014 <- Ray_Soap_2015 <- Ray_Soap_2016 <- NULL
Ray_Soap$Reference <- Month_Mapping$Reference[match(Ray_Soap$date, Month_Mapping$Date)]

temp <- cast(Ray_Soap, ray_practice_id~date, mean, value = "count")

Master_File <- Master_File[,1:5]
Master_File <- merge(Master_File, temp, "ray_practice_id", all.x = T)
Master_File[is.na(Master_File)] <- 0

write.csv(Master_File, "../Ray Usage Attributes/Usage Specific/MoM Distribution - Imputation/Ray_Soap.csv")

# Ray Patient Growth #####################################################################################

Ray_PG_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/7ae5abbb-643c-abea-1a44-28a4edf3ebd7")
Ray_PG_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/09428ea1-6f26-3967-5f17-fee62589f57d")
Ray_PG_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/94d54028-8914-9a6b-4fcf-5c59028fd6cd")
Ray_PG <- rbind(Ray_PG_2014, Ray_PG_2015, Ray_PG_2016)
Ray_PG_2014 <- Ray_PG_2015 <- Ray_PG_2016 <- NULL
Ray_PG$Reference <- Month_Mapping$Reference[match(Ray_PG$date, Month_Mapping$Date)]

temp <- cast(Ray_PG, ray_practice_id~date, mean, value = "count")

Master_File <- Master_File[,1:5]
Master_File <- merge(Master_File, temp, "ray_practice_id", all.x = T)
Master_File[is.na(Master_File)] <- 0

write.csv(Master_File, "../Ray Usage Attributes/Usage Specific/MoM Distribution - Imputation/Ray_PG.csv")

# Ray Patient Vital Signs #####################################################################################

Ray_PVS_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/40de2901-5d24-79ae-8f34-5afb8067f11c")
Ray_PVS_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/29d6407f-865e-ca46-7a8a-dbf366919d8d")
Ray_PVS_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/1d54ed6f-f3ba-fd25-87ed-7bca6e598591")
Ray_PVS <- rbind(Ray_PVS_2014, Ray_PVS_2015, Ray_PVS_2016)
Ray_PVS_2014 <- Ray_PVS_2015 <- Ray_PVS_2016 <- NULL
Ray_PVS$Reference <- Month_Mapping$Reference[match(Ray_PVS$date, Month_Mapping$Date)]

temp <- cast(Ray_PVS, ray_practice_id~date, mean, value = "count")

Master_File <- Master_File[,1:5]
Master_File <- merge(Master_File, temp, "ray_practice_id", all.x = T)
Master_File[is.na(Master_File)] <- 0

write.csv(Master_File, "../Ray Usage Attributes/Usage Specific/MoM Distribution - Imputation/Ray_PVS.csv")

# Ray Patient Immunizations #####################################################################################

Ray_Imm_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/89ff2efc-cf70-1032-b9b2-825a9088ac81")
Ray_Imm_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/9d810948-5051-65e1-bcc0-072b1d1d7d34")
Ray_Imm <- rbind(Ray_Imm_2015, Ray_Imm_2016)
Ray_Imm_2015 <- Ray_Imm_2016 <- NULL
Ray_Imm$Reference <- Month_Mapping$Reference[match(Ray_Imm$date, Month_Mapping$Date)]

temp <- cast(Ray_Imm, ray_practice_id~date, mean, value = "count")

Master_File <- Master_File[,1:5]
Master_File <- merge(Master_File, temp, "ray_practice_id", all.x = T)
Master_File[is.na(Master_File)] <- 0

write.csv(Master_File, "../Ray Usage Attributes/Usage Specific/MoM Distribution - Imputation/Ray_Imm.csv")

# Ray SMS #####################################################################################

Ray_SMS_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/f571d8e9-bfde-c37d-8587-b7b4c181c3a6")
Ray_SMS_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/124a7136-2b3d-2fe9-d461-2600b09cb8f3")
Ray_SMS_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/51b86119-08b9-a4ef-2d22-a506937c32a4")
Ray_SMS <- rbind(Ray_SMS_2014, Ray_SMS_2015, Ray_SMS_2016)
Ray_SMS_2014 <- Ray_SMS_2015 <- Ray_SMS_2016 <- NULL
Ray_SMS$Reference <- Month_Mapping$Reference[match(Ray_SMS$date, Month_Mapping$Date)]

temp <- cast(Ray_SMS, ray_practice_id~date, mean, value = "count")

Master_File <- Master_File[,1:5]
Master_File <- merge(Master_File, temp, "ray_practice_id", all.x = T)
Master_File[is.na(Master_File)] <- 0

write.csv(Master_File, "../Ray Usage Attributes/Usage Specific/MoM Distribution - Imputation/Ray_SMS.csv")

# Ray Appointments from ABS #####################################################################################

Ray_ABS_Appt_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/aa6fe72c-1e9c-83ba-62fd-7db2b2d5ca5b")
Ray_ABS_Appt_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/e8fe46aa-d138-ff0d-efa7-05117650252b")
Ray_ABS_Appt_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/d8f96169-a5d8-6f71-4986-a3ca6d3d0259")
Ray_ABS_Appt <- rbind(Ray_ABS_Appt_2014, Ray_ABS_Appt_2015, Ray_ABS_Appt_2016)
Ray_ABS_Appt_2014 <- Ray_ABS_Appt_2015 <- Ray_ABS_Appt_2016 <- NULL
Ray_ABS_Appt$Reference <- Month_Mapping$Reference[match(Ray_ABS_Appt$date, Month_Mapping$Date)]

temp <- cast(Ray_ABS_Appt, ray_practice_id~date, mean, value = "count")

Master_File <- Master_File[,1:5]
Master_File <- merge(Master_File, temp, "ray_practice_id", all.x = T)
Master_File[is.na(Master_File)] <- 0

write.csv(Master_File, "../Ray Usage Attributes/Usage Specific/MoM Distribution - Imputation/Ray_ABS_Appt.csv")

# Ray Patient Emails #####################################################################################

Ray_PEmails_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/d268c26a-5809-6127-da07-1e16d0ac1f05")
Ray_PEmails_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/34f540ce-bb19-0c54-7bbd-bf938ae36839")
Ray_PEmails_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/b83d4f31-1f64-4ed0-0a47-70d13100f622")
Ray_PEmails <- rbind(Ray_PEmails_2014, Ray_PEmails_2015, Ray_PEmails_2016)
Ray_PEmails_2014 <- Ray_PEmails_2015 <- Ray_PEmails_2016 <- NULL
write.csv(Ray_PEmails, "../Ray Usage Attributes/Usage Specific/Usage Distribution/Ray_PEmails.csv")

Ray_PEmails$Reference <- Month_Mapping$Reference[match(Ray_PEmails$date, Month_Mapping$Date)]

# Recommendations #####################################################################################

Ray_Reco <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/8c2c6f54-a096-fc42-58c3-5e0caddbe9ff")
Ray_Reco$Reference <- Month_Mapping$Reference[match(Ray_Reco$date, Month_Mapping$Date)]

temp <- cast(Ray_Reco, ray_practice_id~date, mean, value = "count")

Master_File <- Master_File[,1:5]
Master_File <- merge(Master_File, temp, "ray_practice_id", all.x = T)
Master_File[is.na(Master_File)] <- 0

write.csv(Master_File, "../Ray Usage Attributes/Usage Specific/MoM Distribution - Imputation/Ray_Reco.csv")

#######################################################################################
# Ray Appointments #####################################################################################
Ray_Appnts <- Ray_Appnts[order(Ray_Appnts$ray_practice_id, Ray_Appnts$date),]
Ray_Appnts$Indent[2:length(Ray_Appnts$ray_practice_id)] <- Ray_Appnts$count[1:length(Ray_Appnts$ray_practice_id)-1]
Ray_Appnts$Indent[is.na(Ray_Appnts$Indent)] <- 0
Ray_Appnts$Indent[Ray_Appnts$Indent > 828] <- 828
Ray_Appnts$count[Ray_Appnts$count > 828] <- Ray_Appnts$Indent[Ray_Appnts$count > 828]
# Ray Appointment Files #############################################################
Ray_Appnt_Files <- Ray_Appnt_Files[order(Ray_Appnt_Files$ray_practice_id, Ray_Appnt_Files$date),]
Ray_Appnt_Files$Indent[2:length(Ray_Appnt_Files$ray_practice_id)] <- Ray_Appnt_Files$count[1:length(Ray_Appnt_Files$ray_practice_id)-1]
Ray_Appnt_Files$Indent[is.na(Ray_Appnt_Files$Indent)] <- 0
Ray_Appnt_Files$Indent[Ray_Appnt_Files$Indent > 423] <- 423
Ray_Appnt_Files$count[Ray_Appnt_Files$count > 423] <- Ray_Appnt_Files$Indent[Ray_Appnt_Files$count > 423]
# Ray Prescriptions #################################################################
Ray_Prescrip <- Ray_Prescrip[order(Ray_Prescrip$ray_practice_id, Ray_Prescrip$date),]
Ray_Prescrip$Indent[2:length(Ray_Prescrip$ray_practice_id)] <- Ray_Prescrip$count[1:length(Ray_Prescrip$ray_practice_id)-1]
Ray_Prescrip$Indent[is.na(Ray_Prescrip$Indent)] <- 0
Ray_Prescrip$Indent[Ray_Prescrip$Indent > 497] <- 497
Ray_Prescrip$count[Ray_Prescrip$count > 497] <- Ray_Prescrip$Indent[Ray_Prescrip$count > 497]
# Ray Invoices ######################################################################
Ray_Invoices <- Ray_Invoices[order(Ray_Invoices$ray_practice_id, Ray_Invoices$date),]
Ray_Invoices$Indent[2:length(Ray_Invoices$ray_practice_id)] <- Ray_Invoices$count[1:length(Ray_Invoices$ray_practice_id)-1]
Ray_Invoices$Indent[is.na(Ray_Invoices$Indent)] <- 0
Ray_Invoices$Indent[Ray_Invoices$Indent > 560] <- 560
Ray_Invoices$count[Ray_Invoices$count > 560] <- Ray_Invoices$Indent[Ray_Invoices$count > 560]
# Ray PF #############################################################################
Ray_PF <- Ray_PF[order(Ray_PF$ray_practice_id, Ray_PF$date),]
Ray_PF$Indent[2:length(Ray_PF$ray_practice_id)] <- Ray_PF$count[1:length(Ray_PF$ray_practice_id)-1]
Ray_PF$Indent[is.na(Ray_PF$Indent)] <- 0
Ray_PF$Indent[Ray_PF$Indent > 151] <- 151
Ray_PF$count[Ray_PF$count > 151] <- Ray_PF$Indent[Ray_PF$count > 151]
# Ray Pay #############################################################################
Ray_Pay <- Ray_Pay[order(Ray_Pay$ray_practice_id, Ray_Pay$date),]
Ray_Pay$Indent[2:length(Ray_Pay$ray_practice_id)] <- Ray_Pay$count[1:length(Ray_Pay$ray_practice_id)-1]
Ray_Pay$Indent[is.na(Ray_Pay$Indent)] <- 0
Ray_Pay$Indent[Ray_Pay$Indent > 674] <- 674
Ray_Pay$count[Ray_Pay$count > 674] <- Ray_Pay$Indent[Ray_Pay$count > 674]
# Ray Soap #############################################################################
Ray_Soap <- Ray_Soap[order(Ray_Soap$ray_practice_id, Ray_Soap$date),]
Ray_Soap$Indent[2:length(Ray_Soap$ray_practice_id)] <- Ray_Soap$count[1:length(Ray_Soap$ray_practice_id)-1]
Ray_Soap$Indent[is.na(Ray_Soap$Indent)] <- 0
Ray_Soap$Indent[Ray_Soap$Indent > 430] <- 430
Ray_Soap$count[Ray_Soap$count > 430] <- Ray_Soap$Indent[Ray_Soap$count > 430]
# Ray PG #############################################################################
Ray_PG <- Ray_PG[order(Ray_PG$ray_practice_id, Ray_PG$date),]
Ray_PG$Indent[2:length(Ray_PG$ray_practice_id)] <- Ray_PG$count[1:length(Ray_PG$ray_practice_id)-1]
Ray_PG$Indent[is.na(Ray_PG$Indent)] <- 0
Ray_PG$Indent[Ray_PG$Indent > 96] <- 96
Ray_PG$count[Ray_PG$count > 96] <- Ray_PG$Indent[Ray_PG$count > 96]
# Ray PVS #############################################################################
Ray_PVS <- Ray_PVS[order(Ray_PVS$ray_practice_id, Ray_PVS$date),]
Ray_PVS$Indent[2:length(Ray_PVS$ray_practice_id)] <- Ray_PVS$count[1:length(Ray_PVS$ray_practice_id)-1]
Ray_PVS$Indent[is.na(Ray_PVS$Indent)] <- 0
Ray_PVS$Indent[Ray_PVS$Indent > 175] <- 175
Ray_PVS$count[Ray_PVS$count > 175] <- Ray_PVS$Indent[Ray_PVS$count > 175]
# Ray Imm #############################################################################
Ray_Imm <- Ray_Imm[order(Ray_Imm$ray_practice_id, Ray_Imm$date),]
Ray_Imm$Indent[2:length(Ray_Imm$ray_practice_id)] <- Ray_Imm$count[1:length(Ray_Imm$ray_practice_id)-1]
Ray_Imm$Indent[is.na(Ray_Imm$Indent)] <- 0
Ray_Imm$Indent[Ray_Imm$Indent > 2778] <- 2778
Ray_Imm$count[Ray_Imm$count > 2778] <- Ray_Imm$Indent[Ray_Imm$count > 2778]
# Ray SMS ############################################################################
Ray_SMS <- Ray_SMS[order(Ray_SMS$ray_practice_id, Ray_SMS$date),]
Ray_SMS$Indent[2:length(Ray_SMS$ray_practice_id)] <- Ray_SMS$count[1:length(Ray_SMS$ray_practice_id)-1]
Ray_SMS$Indent[is.na(Ray_SMS$Indent)] <- 0
Ray_SMS$Indent[Ray_SMS$Indent > 828] <- 828
Ray_SMS$count[Ray_SMS$count > 828] <- Ray_SMS$Indent[Ray_SMS$count > 828]
# Ray ABS Appt #######################################################################
Ray_ABS_Appt <- Ray_ABS_Appt[order(Ray_ABS_Appt$ray_practice_id, Ray_ABS_Appt$date),]
Ray_ABS_Appt$Indent[2:length(Ray_ABS_Appt$ray_practice_id)] <- Ray_ABS_Appt$count[1:length(Ray_ABS_Appt$ray_practice_id)-1]
Ray_ABS_Appt$Indent[is.na(Ray_ABS_Appt$Indent)] <- 0
Ray_ABS_Appt$Indent[Ray_ABS_Appt$Indent > 131] <- 131
Ray_ABS_Appt$count[Ray_ABS_Appt$count > 131] <- Ray_ABS_Appt$Indent[Ray_ABS_Appt$count > 131]
# Ray Patient Emails ###################################################################
Ray_PEmails <- Ray_PEmails[order(Ray_PEmails$ray_practice_id, Ray_PEmails$date),]
Ray_PEmails$Indent[2:length(Ray_PEmails$ray_practice_id)] <- Ray_PEmails$count[1:length(Ray_PEmails$ray_practice_id)-1]
Ray_PEmails$Indent[is.na(Ray_PEmails$Indent)] <- 0
Ray_PEmails$Indent[Ray_PEmails$Indent > 100] <- 100
Ray_PEmails$count[Ray_PEmails$count > 100] <- Ray_PEmails$Indent[Ray_PEmails$count > 100]
######################################################################################
