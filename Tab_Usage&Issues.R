# Tab Prescriptions #####################################################################################

Tab_Prescrip <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/ba4ebd14-5f87-61ec-cfc5-4a901fbcfe2d")

Tab_Prescrip$Reference <- Month_Mapping$Reference[match(Tab_Prescrip$date, Month_Mapping$Date)]

Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp1 <- subset(Tab_Prescrip, Tab_Prescrip$ray_practice_id == id)
  temp11 <- subset(Tab_Prescrip, Tab_Prescrip$ray_practice_id == id)
  temp3 <- subset(Tab_Prescrip, Tab_Prescrip$ray_practice_id == id)
  temp33 <- subset(Tab_Prescrip, Tab_Prescrip$ray_practice_id == id)
  temp6 <- subset(Tab_Prescrip, Tab_Prescrip$ray_practice_id == id)
  temp66 <- subset(Tab_Prescrip, Tab_Prescrip$ray_practice_id == id)
  temp1 <- subset(temp1, temp1$Reference == (dt+1))
  temp11 <- subset(temp11, temp11$Reference == (dt-1))
  temp3 <- subset(temp3, temp3$Reference <= (dt+3))
  if(nrow(temp3) == 0) {
    temp3 <- subset(temp3,temp3$Reference <= (dt+3))
  } else {
    temp3 <- aggregate(count~ray_practice_id, temp3, FUN = sum)
  }
  temp33 <- subset(temp33, temp33$Reference >= (dt-3) & temp33$Reference <= (dt-1))
  if(nrow(temp33) == 0) {
    temp33 <- subset(temp33,temp33$Reference <= (dt-3))
  } else {
    temp33 <- aggregate(count~ray_practice_id, temp33, FUN = sum)
  }
  temp6 <- subset(temp6,temp6$Reference <= (dt+6))
  if(nrow(temp6) == 0) {
    temp6 <- subset(temp6,temp6$Reference <= (dt+6))
  } else {
    temp6 <- aggregate(count~ray_practice_id, temp6, FUN = sum)
  }
  temp66 <- subset(temp66, temp66$Reference >= (dt-6) & temp66$Reference <= (dt-1))
  if(nrow(temp66) == 0) {
    temp66 <- subset(temp66,temp66$Reference <= (dt-6))
  } else {
    temp66 <- aggregate(count~ray_practice_id, temp66, FUN = sum)
  }
  ##
  ifelse((nrow(temp66) != 0),  Master_File$Time66[i] <- temp66$count[match(Master_File$ray_practice_id[i], temp66$ray_practice_id)],  Master_File$Time66[i] <- "-")
  ifelse((nrow(temp33) != 0), Master_File$Time33[i] <- temp33$count[match(Master_File$ray_practice_id[i], temp33$ray_practice_id)], Master_File$Time33[i] <- "-" )
  ifelse((nrow(temp11) != 0), Master_File$Time11[i] <- temp11$count[match(Master_File$ray_practice_id[i], temp11$ray_practice_id)], Master_File$Time11[i] <- "-")
  
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- "-")
  ifelse((nrow(temp3) != 0),Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)], Master_File$Time3[i] <- "-" )
  ifelse((nrow(temp6) != 0), Master_File$Time6[i] <- temp6$count[match(Master_File$ray_practice_id[i], temp6$ray_practice_id)],Master_File$Time6[i] <- "-"  )
}

write.csv(Master_File, "../Ray Usage Attributes/Tab_Prescriptions.csv")

# Tab Appointments #####################################################################################

Tab_Appnts <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/be58055b-eb2f-c6ae-14e7-1b1b74fe1ed8")

Tab_Appnts$Reference <- Month_Mapping$Reference[match(Tab_Appnts$date, Month_Mapping$Date)]

Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp1 <- subset(Tab_Appnts, Tab_Appnts$ray_practice_id == id)
  temp11 <- subset(Tab_Appnts, Tab_Appnts$ray_practice_id == id)
  temp3 <- subset(Tab_Appnts, Tab_Appnts$ray_practice_id == id)
  temp33 <- subset(Tab_Appnts, Tab_Appnts$ray_practice_id == id)
  temp6 <- subset(Tab_Appnts, Tab_Appnts$ray_practice_id == id)
  temp66 <- subset(Tab_Appnts, Tab_Appnts$ray_practice_id == id)
  temp1 <- subset(temp1, temp1$Reference == (dt+1))
  temp11 <- subset(temp11, temp11$Reference == (dt-1))
  temp3 <- subset(temp3, temp3$Reference <= (dt+3))
  if(nrow(temp3) == 0) {
    temp3 <- subset(temp3,temp3$Reference <= (dt+3))
  } else {
    temp3 <- aggregate(count~ray_practice_id, temp3, FUN = sum)
  }
  temp33 <- subset(temp33, temp33$Reference >= (dt-3) & temp33$Reference <= (dt-1))
  if(nrow(temp33) == 0) {
    temp33 <- subset(temp33,temp33$Reference <= (dt-3))
  } else {
    temp33 <- aggregate(count~ray_practice_id, temp33, FUN = sum)
  }
  temp6 <- subset(temp6,temp6$Reference <= (dt+6))
  if(nrow(temp6) == 0) {
    temp6 <- subset(temp6,temp6$Reference <= (dt+6))
  } else {
    temp6 <- aggregate(count~ray_practice_id, temp6, FUN = sum)
  }
  temp66 <- subset(temp66, temp66$Reference >= (dt-6) & temp66$Reference <= (dt-1))
  if(nrow(temp66) == 0) {
    temp66 <- subset(temp66,temp66$Reference <= (dt-6))
  } else {
    temp66 <- aggregate(count~ray_practice_id, temp66, FUN = sum)
  }
  ##
  ifelse((nrow(temp66) != 0),  Master_File$Time66[i] <- temp66$count[match(Master_File$ray_practice_id[i], temp66$ray_practice_id)],  Master_File$Time66[i] <- "-")
  ifelse((nrow(temp33) != 0), Master_File$Time33[i] <- temp33$count[match(Master_File$ray_practice_id[i], temp33$ray_practice_id)], Master_File$Time33[i] <- "-" )
  ifelse((nrow(temp11) != 0), Master_File$Time11[i] <- temp11$count[match(Master_File$ray_practice_id[i], temp11$ray_practice_id)], Master_File$Time11[i] <- "-")
  
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- "-")
  ifelse((nrow(temp3) != 0),Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)], Master_File$Time3[i] <- "-" )
  ifelse((nrow(temp6) != 0), Master_File$Time6[i] <- temp6$count[match(Master_File$ray_practice_id[i], temp6$ray_practice_id)],Master_File$Time6[i] <- "-"  )
}

write.csv(Master_File, "../Ray Usage Attributes/Tab_Appointments.csv")

# Tab Registrations #####################################################################################

Tab_Regis <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/e67d700e-5da8-5b8f-c94e-8b077811cf33")

Tab_Regis$Reference <- Month_Mapping$Reference[match(Tab_Regis$date, Month_Mapping$Date)]

Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp1 <- subset(Tab_Regis, Tab_Regis$ray_practice_id == id)
  temp11 <- subset(Tab_Regis, Tab_Regis$ray_practice_id == id)
  temp3 <- subset(Tab_Regis, Tab_Regis$ray_practice_id == id)
  temp33 <- subset(Tab_Regis, Tab_Regis$ray_practice_id == id)
  temp6 <- subset(Tab_Regis, Tab_Regis$ray_practice_id == id)
  temp66 <- subset(Tab_Regis, Tab_Regis$ray_practice_id == id)
  temp1 <- subset(temp1, temp1$Reference == (dt+1))
  temp11 <- subset(temp11, temp11$Reference == (dt-1))
  temp3 <- subset(temp3, temp3$Reference <= (dt+3))
  if(nrow(temp3) == 0) {
    temp3 <- subset(temp3,temp3$Reference <= (dt+3))
  } else {
    temp3 <- aggregate(count~ray_practice_id, temp3, FUN = sum)
  }
  temp33 <- subset(temp33, temp33$Reference >= (dt-3) & temp33$Reference <= (dt-1))
  if(nrow(temp33) == 0) {
    temp33 <- subset(temp33,temp33$Reference <= (dt-3))
  } else {
    temp33 <- aggregate(count~ray_practice_id, temp33, FUN = sum)
  }
  temp6 <- subset(temp6,temp6$Reference <= (dt+6))
  if(nrow(temp6) == 0) {
    temp6 <- subset(temp6,temp6$Reference <= (dt+6))
  } else {
    temp6 <- aggregate(count~ray_practice_id, temp6, FUN = sum)
  }
  temp66 <- subset(temp66, temp66$Reference >= (dt-6) & temp66$Reference <= (dt-1))
  if(nrow(temp66) == 0) {
    temp66 <- subset(temp66,temp66$Reference <= (dt-6))
  } else {
    temp66 <- aggregate(count~ray_practice_id, temp66, FUN = sum)
  }
  ##
  ifelse((nrow(temp66) != 0),  Master_File$Time66[i] <- temp66$count[match(Master_File$ray_practice_id[i], temp66$ray_practice_id)],  Master_File$Time66[i] <- "-")
  ifelse((nrow(temp33) != 0), Master_File$Time33[i] <- temp33$count[match(Master_File$ray_practice_id[i], temp33$ray_practice_id)], Master_File$Time33[i] <- "-" )
  ifelse((nrow(temp11) != 0), Master_File$Time11[i] <- temp11$count[match(Master_File$ray_practice_id[i], temp11$ray_practice_id)], Master_File$Time11[i] <- "-")
  
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- "-")
  ifelse((nrow(temp3) != 0),Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)], Master_File$Time3[i] <- "-" )
  ifelse((nrow(temp6) != 0), Master_File$Time6[i] <- temp6$count[match(Master_File$ray_practice_id[i], temp6$ray_practice_id)],Master_File$Time6[i] <- "-"  )
}

write.csv(Master_File, "../Ray Usage Attributes/Tab_Registrations.csv")

# Tab Soap Notes #####################################################################################

Tab_SN <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/c0beff8e-1abb-d85c-cf3e-85a49f122935")

Tab_SN$Reference <- Month_Mapping$Reference[match(Tab_SN$date, Month_Mapping$Date)]

Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp1 <- subset(Tab_SN, Tab_SN$ray_practice_id == id)
  temp11 <- subset(Tab_SN, Tab_SN$ray_practice_id == id)
  temp3 <- subset(Tab_SN, Tab_SN$ray_practice_id == id)
  temp33 <- subset(Tab_SN, Tab_SN$ray_practice_id == id)
  temp6 <- subset(Tab_SN, Tab_SN$ray_practice_id == id)
  temp66 <- subset(Tab_SN, Tab_SN$ray_practice_id == id)
  temp1 <- subset(temp1, temp1$Reference == (dt+1))
  temp11 <- subset(temp11, temp11$Reference == (dt-1))
  temp3 <- subset(temp3, temp3$Reference <= (dt+3))
  if(nrow(temp3) == 0) {
    temp3 <- subset(temp3,temp3$Reference <= (dt+3))
  } else {
    temp3 <- aggregate(count~ray_practice_id, temp3, FUN = sum)
  }
  temp33 <- subset(temp33, temp33$Reference >= (dt-3) & temp33$Reference <= (dt-1))
  if(nrow(temp33) == 0) {
    temp33 <- subset(temp33,temp33$Reference <= (dt-3))
  } else {
    temp33 <- aggregate(count~ray_practice_id, temp33, FUN = sum)
  }
  temp6 <- subset(temp6,temp6$Reference <= (dt+6))
  if(nrow(temp6) == 0) {
    temp6 <- subset(temp6,temp6$Reference <= (dt+6))
  } else {
    temp6 <- aggregate(count~ray_practice_id, temp6, FUN = sum)
  }
  temp66 <- subset(temp66, temp66$Reference >= (dt-6) & temp66$Reference <= (dt-1))
  if(nrow(temp66) == 0) {
    temp66 <- subset(temp66,temp66$Reference <= (dt-6))
  } else {
    temp66 <- aggregate(count~ray_practice_id, temp66, FUN = sum)
  }
  ##
  ifelse((nrow(temp66) != 0),  Master_File$Time66[i] <- temp66$count[match(Master_File$ray_practice_id[i], temp66$ray_practice_id)],  Master_File$Time66[i] <- "-")
  ifelse((nrow(temp33) != 0), Master_File$Time33[i] <- temp33$count[match(Master_File$ray_practice_id[i], temp33$ray_practice_id)], Master_File$Time33[i] <- "-" )
  ifelse((nrow(temp11) != 0), Master_File$Time11[i] <- temp11$count[match(Master_File$ray_practice_id[i], temp11$ray_practice_id)], Master_File$Time11[i] <- "-")
  
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- "-")
  ifelse((nrow(temp3) != 0),Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)], Master_File$Time3[i] <- "-" )
  ifelse((nrow(temp6) != 0), Master_File$Time6[i] <- temp6$count[match(Master_File$ray_practice_id[i], temp6$ray_practice_id)],Master_File$Time6[i] <- "-"  )
}

write.csv(Master_File, "../Ray Usage Attributes/Tab_Soap_Notes.csv")

# Tab Appointment Files #####################################################################################

Tab_Appt_Files <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/f70ad305-8a59-9f3b-bdc7-fdde9fdb69c4")

Tab_Appt_Files$Reference <- Month_Mapping$Reference[match(Tab_Appt_Files$date, Month_Mapping$Date)]

Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp1 <- subset(Tab_Appt_Files, Tab_Appt_Files$ray_practice_id == id)
  temp11 <- subset(Tab_Appt_Files, Tab_Appt_Files$ray_practice_id == id)
  temp3 <- subset(Tab_Appt_Files, Tab_Appt_Files$ray_practice_id == id)
  temp33 <- subset(Tab_Appt_Files, Tab_Appt_Files$ray_practice_id == id)
  temp6 <- subset(Tab_Appt_Files, Tab_Appt_Files$ray_practice_id == id)
  temp66 <- subset(Tab_Appt_Files, Tab_Appt_Files$ray_practice_id == id)
  temp1 <- subset(temp1, temp1$Reference == (dt+1))
  temp11 <- subset(temp11, temp11$Reference == (dt-1))
  temp3 <- subset(temp3, temp3$Reference <= (dt+3))
  if(nrow(temp3) == 0) {
    temp3 <- subset(temp3,temp3$Reference <= (dt+3))
  } else {
    temp3 <- aggregate(count~ray_practice_id, temp3, FUN = sum)
  }
  temp33 <- subset(temp33, temp33$Reference >= (dt-3) & temp33$Reference <= (dt-1))
  if(nrow(temp33) == 0) {
    temp33 <- subset(temp33,temp33$Reference <= (dt-3))
  } else {
    temp33 <- aggregate(count~ray_practice_id, temp33, FUN = sum)
  }
  temp6 <- subset(temp6,temp6$Reference <= (dt+6))
  if(nrow(temp6) == 0) {
    temp6 <- subset(temp6,temp6$Reference <= (dt+6))
  } else {
    temp6 <- aggregate(count~ray_practice_id, temp6, FUN = sum)
  }
  temp66 <- subset(temp66, temp66$Reference >= (dt-6) & temp66$Reference <= (dt-1))
  if(nrow(temp66) == 0) {
    temp66 <- subset(temp66,temp66$Reference <= (dt-6))
  } else {
    temp66 <- aggregate(count~ray_practice_id, temp66, FUN = sum)
  }
  ##
  ifelse((nrow(temp66) != 0),  Master_File$Time66[i] <- temp66$count[match(Master_File$ray_practice_id[i], temp66$ray_practice_id)],  Master_File$Time66[i] <- "-")
  ifelse((nrow(temp33) != 0), Master_File$Time33[i] <- temp33$count[match(Master_File$ray_practice_id[i], temp33$ray_practice_id)], Master_File$Time33[i] <- "-" )
  ifelse((nrow(temp11) != 0), Master_File$Time11[i] <- temp11$count[match(Master_File$ray_practice_id[i], temp11$ray_practice_id)], Master_File$Time11[i] <- "-")
  
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- "-")
  ifelse((nrow(temp3) != 0),Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)], Master_File$Time3[i] <- "-" )
  ifelse((nrow(temp6) != 0), Master_File$Time6[i] <- temp6$count[match(Master_File$ray_practice_id[i], temp6$ray_practice_id)],Master_File$Time6[i] <- "-"  )
}

write.csv(Master_File, "../Ray Usage Attributes/Tab_Appointment_Files.csv")

# Tab Feedback #####################################################################################

Tab_Feed <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/3fe47be2-8d5b-fe54-45fd-8b10f1d894ea")

Tab_Feed$Reference <- Month_Mapping$Reference[match(Tab_Feed$date, Month_Mapping$Date)]

Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp1 <- subset(Tab_Feed, Tab_Feed$ray_practice_id == id)
  temp11 <- subset(Tab_Feed, Tab_Feed$ray_practice_id == id)
  temp3 <- subset(Tab_Feed, Tab_Feed$ray_practice_id == id)
  temp33 <- subset(Tab_Feed, Tab_Feed$ray_practice_id == id)
  temp6 <- subset(Tab_Feed, Tab_Feed$ray_practice_id == id)
  temp66 <- subset(Tab_Feed, Tab_Feed$ray_practice_id == id)
  temp1 <- subset(temp1, temp1$Reference == (dt+1))
  temp11 <- subset(temp11, temp11$Reference == (dt-1))
  temp3 <- subset(temp3, temp3$Reference <= (dt+3))
  if(nrow(temp3) == 0) {
    temp3 <- subset(temp3,temp3$Reference <= (dt+3))
  } else {
    temp3 <- aggregate(count~ray_practice_id, temp3, FUN = sum)
  }
  temp33 <- subset(temp33, temp33$Reference >= (dt-3) & temp33$Reference <= (dt-1))
  if(nrow(temp33) == 0) {
    temp33 <- subset(temp33,temp33$Reference <= (dt-3))
  } else {
    temp33 <- aggregate(count~ray_practice_id, temp33, FUN = sum)
  }
  temp6 <- subset(temp6,temp6$Reference <= (dt+6))
  if(nrow(temp6) == 0) {
    temp6 <- subset(temp6,temp6$Reference <= (dt+6))
  } else {
    temp6 <- aggregate(count~ray_practice_id, temp6, FUN = sum)
  }
  temp66 <- subset(temp66, temp66$Reference >= (dt-6) & temp66$Reference <= (dt-1))
  if(nrow(temp66) == 0) {
    temp66 <- subset(temp66,temp66$Reference <= (dt-6))
  } else {
    temp66 <- aggregate(count~ray_practice_id, temp66, FUN = sum)
  }
  ##
  ifelse((nrow(temp66) != 0),  Master_File$Time66[i] <- temp66$count[match(Master_File$ray_practice_id[i], temp66$ray_practice_id)],  Master_File$Time66[i] <- "-")
  ifelse((nrow(temp33) != 0), Master_File$Time33[i] <- temp33$count[match(Master_File$ray_practice_id[i], temp33$ray_practice_id)], Master_File$Time33[i] <- "-" )
  ifelse((nrow(temp11) != 0), Master_File$Time11[i] <- temp11$count[match(Master_File$ray_practice_id[i], temp11$ray_practice_id)], Master_File$Time11[i] <- "-")
  
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- "-")
  ifelse((nrow(temp3) != 0),Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)], Master_File$Time3[i] <- "-" )
  ifelse((nrow(temp6) != 0), Master_File$Time6[i] <- temp6$count[match(Master_File$ray_practice_id[i], temp6$ray_practice_id)],Master_File$Time6[i] <- "-"  )
}

write.csv(Master_File, "../Ray Usage Attributes/Tab_Feedback.csv")

# Chronos Issues #####################################################################################
CIssues_before_June2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/7585fce0-5f82-6b86-d2a0-fec6839fb6c1")
CIssues_after_June2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/fd63101b-aa0b-13b2-0628-21676a9d04aa")
CIssues <- rbind(CIssues_before_June2016, CIssues_after_June2016)

write.csv(CIssues, "../Ray Usage Attributes/Usage Specific/Usage Distribution/CIssues.csv")

CIssues$TAT <- as.Date(CIssues$modified_on) - as.Date(CIssues$created_on)

CIssues$Reference <- Month_Mapping$Reference[match(CIssues$date, Month_Mapping$Date)]

Master_File <- Master_File[c("ray_practice_id", "Type", "Reference")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]
##
temp <- count(CIssues$ray_practice_id)
Master_File$No_of_Chronos_Issues <- temp$freq[match(Master_File$ray_practice_id, temp$x)]
Master_File$No_of_Chronos_Issues[is.na(Master_File$No_of_Chronos_Issues)] <- 0
##
temp <- CIssues[c("ray_practice_id", "source")]
temp <- count(temp)
temp <- unique(temp)
temp2 <- aggregate(freq~ray_practice_id+source, data = temp, FUN = sum)
##
temp3 <- subset(temp2, temp2$source == "CALL")
Master_File$Call_Issues <- temp3$freq[match(Master_File$ray_practice_id, temp3$ray_practice_id)]
Master_File$Call_Issues[is.na(Master_File$Call_Issues)] <- 0
##
temp3 <- subset(temp2, temp2$source == "CHAT")
Master_File$Chat_Issues <- temp3$freq[match(Master_File$ray_practice_id, temp3$ray_practice_id)]
Master_File$Chat_Issues[is.na(Master_File$Chat_Issues)] <- 0
##
temp3 <- subset(temp2, temp2$source == "Chronos")
Master_File$Chronos_Issues <- temp3$freq[match(Master_File$ray_practice_id, temp3$ray_practice_id)]
Master_File$Chronos_Issues[is.na(Master_File$Chronos_Issues)] <- 0
##
temp3 <- subset(temp2, temp2$source == "EMAIL")
Master_File$Email_Issues <- temp3$freq[match(Master_File$ray_practice_id, temp3$ray_practice_id)]
Master_File$Email_Issues[is.na(Master_File$Email_Issues)] <- 0
##
temp3 <- subset(temp2, temp2$source == "FEEDBACK")
Master_File$Feedback_Issues <- temp3$freq[match(Master_File$ray_practice_id, temp3$ray_practice_id)]
Master_File$Feedback_Issues[is.na(Master_File$Feedback_Issues)] <- 0
##
CIssues <- CIssues[c("ray_practice_id", "date")]
CIssues$temp <- 1
temp <- aggregate(temp~ray_practice_id+date, data = CIssues, FUN = sum)
colnames(temp) <- c("ray_practice_id", "date", "count")
CIssues <- temp

for(i in 1:length(Master_File$ray_practice_id))
{
  print(i)
  id <- Master_File$ray_practice_id[i]
  dt <- Master_File$Reference[i]
  temp1 <- subset(CIssues, CIssues$ray_practice_id == id)
  temp11 <- subset(CIssues, CIssues$ray_practice_id == id)
  temp3 <- subset(CIssues, CIssues$ray_practice_id == id)
  temp33 <- subset(CIssues, CIssues$ray_practice_id == id)
  temp6 <- subset(CIssues, CIssues$ray_practice_id == id)
  temp66 <- subset(CIssues, CIssues$ray_practice_id == id)
  temp1 <- subset(temp1, temp1$Reference == (dt+1))
  temp11 <- subset(temp11, temp11$Reference == (dt-1))
  temp3 <- subset(temp3, temp3$Reference <= (dt+3))
  if(nrow(temp3) == 0) {
    temp3 <- subset(temp3,temp3$Reference <= (dt+3))
  } else {
    temp3 <- aggregate(count~ray_practice_id, temp3, FUN = sum)
  }
  temp33 <- subset(temp33, temp33$Reference >= (dt-3) & temp33$Reference <= (dt-1))
  if(nrow(temp33) == 0) {
    temp33 <- subset(temp33,temp33$Reference <= (dt-3))
  } else {
    temp33 <- aggregate(count~ray_practice_id, temp33, FUN = sum)
  }
  temp6 <- subset(temp6,temp6$Reference <= (dt+6))
  if(nrow(temp6) == 0) {
    temp6 <- subset(temp6,temp6$Reference <= (dt+6))
  } else {
    temp6 <- aggregate(count~ray_practice_id, temp6, FUN = sum)
  }
  temp66 <- subset(temp66, temp66$Reference >= (dt-6) & temp66$Reference <= (dt-1))
  if(nrow(temp66) == 0) {
    temp66 <- subset(temp66,temp66$Reference <= (dt-6))
  } else {
    temp66 <- aggregate(count~ray_practice_id, temp66, FUN = sum)
  }
  ##
  ifelse((nrow(temp66) != 0),  Master_File$Time66[i] <- temp66$count[match(Master_File$ray_practice_id[i], temp66$ray_practice_id)],  Master_File$Time66[i] <- "-")
  ifelse((nrow(temp33) != 0), Master_File$Time33[i] <- temp33$count[match(Master_File$ray_practice_id[i], temp33$ray_practice_id)], Master_File$Time33[i] <- "-" )
  ifelse((nrow(temp11) != 0), Master_File$Time11[i] <- temp11$count[match(Master_File$ray_practice_id[i], temp11$ray_practice_id)], Master_File$Time11[i] <- "-")
  
  ifelse((nrow(temp1) != 0), Master_File$Time1[i] <- temp1$count[match(Master_File$ray_practice_id[i], temp1$ray_practice_id)], Master_File$Time1[i] <- "-")
  ifelse((nrow(temp3) != 0),Master_File$Time3[i] <- temp3$count[match(Master_File$ray_practice_id[i], temp3$ray_practice_id)], Master_File$Time3[i] <- "-" )
  ifelse((nrow(temp6) != 0), Master_File$Time6[i] <- temp6$count[match(Master_File$ray_practice_id[i], temp6$ray_practice_id)],Master_File$Time6[i] <- "-"  )
}

write.csv(Master_File, "../Ray Usage Attributes/Chronos_Issues.csv")

