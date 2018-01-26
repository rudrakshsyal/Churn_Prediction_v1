# Ray Appointments from ABS #####################################################################################
Ray_ABS_Appt_2016 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/aa6fe72c-1e9c-83ba-62fd-7db2b2d5ca5b")
Ray_ABS_Appt_2015 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/e8fe46aa-d138-ff0d-efa7-05117650252b")
Ray_ABS_Appt_2014 <- read.csv("https://www.periscopedata.com/api/practo/chart/csv/d8f96169-a5d8-6f71-4986-a3ca6d3d0259")
Ray_ABS_Appt <- rbind(Ray_ABS_Appt_2014, Ray_ABS_Appt_2015, Ray_ABS_Appt_2016)

Ray_ABS_Appt$Reference <- Month_Mapping$Reference[match(Ray_ABS_Appt$date, Month_Mapping$Date)]
Ray_ABS_Appt$Upsell <- Master_File$Upsell[match(Ray_ABS_Appt$ray_practice_id, Master_File$ray_practice_id)]
# Ray ABS Appt #
Ray_ABS_Appt <- Ray_ABS_Appt[order(Ray_ABS_Appt$ray_practice_id, Ray_ABS_Appt$date),]
Ray_ABS_Appt$Indent[2:length(Ray_ABS_Appt$ray_practice_id)] <- Ray_ABS_Appt$count[1:length(Ray_ABS_Appt$ray_practice_id)-1]
Ray_ABS_Appt$Indent[is.na(Ray_ABS_Appt$Indent)] <- 0
Ray_ABS_Appt$Indent[Ray_ABS_Appt$Indent > 131] <- 131
Ray_ABS_Appt$count[Ray_ABS_Appt$count > 131] <- Ray_ABS_Appt$Indent[Ray_ABS_Appt$count > 131]
Ray_ABS_Appt$Indent <- NULL
# Ray ABS Appt #
Master_File <- Master_File[c("ray_practice_id", "Type", "Reference", "Upsell")]
Master_File <- Master_File[order(Master_File$ray_practice_id),]

Ray_ABS_Appt$Hunting <- Master_File$Reference[match(Ray_ABS_Appt$ray_practice_id, Master_File$ray_practice_id)]
Ray_ABS_Appt <- subset(Ray_ABS_Appt,!is.na(Ray_ABS_Appt$Hunting))
Ray_ABS_Appt <- subset(Ray_ABS_Appt, Ray_ABS_Appt$Hunting <= Ray_ABS_Appt$Reference)
Ray_ABS_Appt <- subset(Ray_ABS_Appt, Ray_ABS_Appt$Upsell >= Ray_ABS_Appt$Reference)
Ray_ABS_Appt$Relative_Month <- Ray_ABS_Appt$Upsell - Ray_ABS_Appt$Reference + 1

temp1 <- aggregate(count~ray_practice_id, data = Ray_ABS_Appt, FUN = max)
temp2 <- aggregate(count~ray_practice_id, data = Ray_ABS_Appt, FUN = mean)
temp3 <- aggregate(count~ray_practice_id, data = Ray_ABS_Appt, FUN = sum)
temp4 <- aggregate(count~ray_practice_id+Reference, data = Ray_ABS_Appt, FUN = sum)
temp4 <- subset(temp4, temp4$Reference <= 30)
temp4 <- cast(temp4, ray_practice_id~Reference, sum, value = "count")

Master_File <- merge(Master_File, temp4, by = "ray_practice_id", all.x = T, all.y = F)
Master_File[is.na(Master_File)] <- 0
colnames(Master_File) <- c("ray_practice_id", "Type", "Reference", "Upsell", "jan - 15","feb - 15","mar - 15","apr - 15","may - 15","jun - 15","jul - 15","aug - 15","sep - 15","oct - 15","nov - 15","dec - 15","jan - 16","feb - 16","mar - 16","apr - 16","may - 16", "jun - 16")
Master_File$Max <- temp1$count[match(Master_File$ray_practice_id, temp1$ray_practice_id)]
Master_File$Avg <- temp2$count[match(Master_File$ray_practice_id, temp2$ray_practice_id)]
Master_File$Sum <- temp3$count[match(Master_File$ray_practice_id, temp3$ray_practice_id)]
Master_File[is.na(Master_File)] <- 0
Master_File$`Log_Avg` <- log(Master_File$Avg + 1)
Master_File$`Sqrt_Avg` <- sqrt(Master_File$Avg)

##
temp11 <- Ray_ABS_Appt
temp11$Helper1 <- temp11$Upsell - temp11$Reference
temp22 <- subset(temp11, temp11$Helper1 == 0)
temp11$Helper2 <- temp22$Helper1[match(temp11$ray_practice_id, temp22$ray_practice_id)]
temp33 <- subset(temp11, is.na(temp11$Helper2))
temp44 <- as.data.frame(unique(temp33$ray_practice_id))
colnames(temp44) <- c("ray_practice_id")
temp33 <- temp33[order(temp33$ray_practice_id, -temp33$Reference),]
temp44$Upsell <- temp33$Upsell[match(temp44$ray_practice_id, temp33$ray_practice_id)]
temp44$Reference <- temp33$Reference[match(temp44$ray_practice_id, temp33$ray_practice_id)]
temp44$date <- temp33$date[match(temp44$ray_practice_id, temp33$ray_practice_id)]
temp44$count <- 0
temp44$days <- 0
temp44$Hunting <- temp33$Hunting[match(temp44$ray_practice_id, temp33$ray_practice_id)]
temp44$Relative_Month <- temp33$Relative_Month[match(temp44$ray_practice_id, temp33$ray_practice_id)]
temp44$Reference <- temp44$Upsell
Ray_ABS_Appt <- rbind(Ray_ABS_Appt, temp44)
Ray_ABS_Appt <- Ray_ABS_Appt[order(Ray_ABS_Appt$ray_practice_id,Ray_ABS_Appt$Reference),]
Ray_ABS_Appt$Relative_Month <- Ray_ABS_Appt$Upsell - Ray_ABS_Appt$Reference + 1

  data <- Ray_ABS_Appt

data <- data[order(data$ray_practice_id, -data$Relative_Month),]
data$refdiff <- 0
data[(1:(nrow(data)-1)),9] <- data[(2:nrow(data)),5]
data$refdiff <- data$refdiff - data$Reference
# data$temp[2:length(data$ray_practice_id)] <- data$refdiff[1:length(data$ray_practice_id)-1]
# data$temp2[1:length(data$ray_practice_id)] <- data$ray_practice_id[2:length(data$ray_practice_id)]
# data$refdiff[data$ray_practice_id != data$temp2] <- data$temp[data$ray_practice_id != data$temp2]
# data$temp <- data$temp2 <- NULL
data$final <- 0
data$decay <- 0

prac <- as.data.frame(unique(data$ray_practice_id))
df <- data[1,]
i=1
for(i in 1:nrow(prac))
{
  id <- prac[i,1]
  temp <- subset(data,data$ray_practice_id==id)
  temp$final[1] <- temp$count[1]
  temp$decay[1] <- temp$count[1]*(0.5^temp$refdiff[1])
  if(nrow(temp)>1){
  for(j in 2:nrow(temp))
  {
    temp$final[j] <- temp$count[j]+temp$decay[j-1]
    temp$decay[j] <- temp$final[j]*(0.5^temp$refdiff[j])
  }}
  df <- rbind(df, temp)
}

df <- subset(df, df$Relative_Month == 1)
Master_File$Decay_1Month <- df$final[match(Master_File$ray_practice_id, df$ray_practice_id)]
Master_File[is.na(Master_File)] <- 0

data2 <- Ray_ABS_Appt

data2 <- data2[order(data2$ray_practice_id, -data2$Relative_Month),]
data2$refdiff <- 0
data2[(1:(nrow(data2)-1)),9] <- data2[(2:nrow(data2)),5]
data2$refdiff <- data2$refdiff - data2$Reference
data2$final <- 0
data2$decay <- 0

prac <- as.data.frame(unique(data2$ray_practice_id))
df <- data2[1,]
i=1
for(i in 1:nrow(prac))
{
  id <- prac[i,1]
  temp <- subset(data2,data2$ray_practice_id==id)
  temp$final[1] <- temp$count[1]
  temp$decay[1] <- temp$count[1]*(0.5^(temp$refdiff[1]/3))
  if(nrow(temp)>1){
    for(j in 2:nrow(temp))
    {
      temp$final[j] <- temp$count[j]+temp$decay[j-1]
      temp$decay[j] <- temp$final[j]*(0.5^(temp$refdiff[j]/3))
    }}
  df <- rbind(df, temp)
}

df <- subset(df, df$Relative_Month == 1)
Master_File$Decay_3Month <- df$final[match(Master_File$ray_practice_id, df$ray_practice_id)]
Master_File[is.na(Master_File)] <- 0

temp <- subset(Ray_ABS_Appt, Ray_ABS_Appt$Relative_Month <= 3)
temp <- aggregate(count~ray_practice_id, data = temp, FUN = sum) 
Master_File$Cumulative_Last3Months <- temp$count[match(Master_File$ray_practice_id, temp$ray_practice_id)]
Master_File[is.na(Master_File)] <- 0

Master_File$Avg <- as.numeric(Master_File$Avg)
Master_File$`Categ_Avg` <- 'Low'
Master_File$`Categ_Avg`[Master_File$Avg > 13] <- 'Medium'
Master_File$`Categ_Avg`[Master_File$Avg > 111] <- 'High'

Master_File$Decay_1Month <- as.numeric(Master_File$Decay_1Month)
Master_File$`Categ_Decay_1Month` <- 'Low'
Master_File$`Categ_Decay_1Month`[Master_File$Decay_1Month > 50] <- 'Medium'
Master_File$`Categ_Decay_1Month`[Master_File$Decay_1Month > 200] <- 'High'

Master_File$Decay_3Month <- as.numeric(Master_File$Decay_3Month)
Master_File$`Categ_Decay_3Month` <- 'Low'
Master_File$`Categ_Decay_3Month`[Master_File$Decay_3Month > 50] <- 'Medium'
Master_File$`Categ_Decay_3Month`[Master_File$Decay_3Month > 200] <- 'High'

Master_File$Categ_Avg_High[Master_File$Categ_Avg == "High"] <- 1
Master_File$Categ_Avg_High[is.na(Master_File$Categ_Avg_High)] <- 0

Master_File$Categ_Decay_1Month_High[Master_File$Categ_Decay_1Month == "High"] <- 1
Master_File$Categ_Decay_1Month_High[is.na(Master_File$Categ_Decay_1Month_High)] <- 0


write.csv(Master_File, "../Secondary_Variables2.csv")
