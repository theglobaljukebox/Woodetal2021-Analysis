#This code performs inter-rater reliability analyses for Cantometric training codings from CompMusic Lab students and with Global Jukebox codings

#This code requires installing and loading the following packages:
#Install:
install.packages("irr")
install.packages("psych")
install.packages("googlesheets4")
install.packages("lsr")
install.packages("pwr")


#Load:
library(irr)
library(psych)
library(googlesheets4)
library(lsr)
library(pwr)

options(stringsAsFactors = FALSE)

#Read full Cantometrics data from Google Sheets:
(c <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1AjynK9mMQTw58B_B8b_ZIip3fyUm-aoV7Pp21HziBb0/edit?ts=5edaae90#gid=974103515")))

#Read Consensus Tape data from Google Sheets
(HD <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1vCA6P92w71z1lAIz_EMDrp-qplUkqpC_aGdXLc0OfQM/gid=400900714&output=csv",sheet=4)))
(JT <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1vCA6P92w71z1lAIz_EMDrp-qplUkqpC_aGdXLc0OfQM/gid=400900714&output=csv",sheet=5)))
(GC <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1vCA6P92w71z1lAIz_EMDrp-qplUkqpC_aGdXLc0OfQM/gid=400900714&output=csv",sheet=6)))
(SS <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1vCA6P92w71z1lAIz_EMDrp-qplUkqpC_aGdXLc0OfQM/gid=400900714&output=csv",sheet=2)))
(YO <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1vCA6P92w71z1lAIz_EMDrp-qplUkqpC_aGdXLc0OfQM/gid=400900714&output=csv",sheet=7)))
(YY <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1vCA6P92w71z1lAIz_EMDrp-qplUkqpC_aGdXLc0OfQM/gid=400900714&output=csv",sheet=3)))

#Reduce to only 30 songs of interest:
(HD<-HD[19:48,])
(JT<-JT[19:48,])
(GC<-GC[19:48,])
(YO<-YO[19:48,])
(SS<-SS[19:48,])
(YY<-YY[19:48,])

#fix (potential) formatting bugs
HD[,c(2:38,40)]<-as.numeric(as.character(unlist(HD[,c(2:38,40)])))
JT[,c(2:38,40)]<-as.numeric(as.character(unlist(JT[,c(2:38,40)])))
GC[,c(2:38,40)]<-as.numeric(as.character(unlist(GC[,c(2:38,40)])))
YO[,c(2:38,40)]<-as.numeric(as.character(unlist(YO[,c(2:38,40)])))
SS[,c(2:38,40)]<-as.numeric(as.character(unlist(SS[,c(2:38,40)])))
YY[,c(2:38,40)]<-as.numeric(as.character(unlist(YY[,c(2:38,40)])))


#Compare JT reliability vs. Cantometrics
#merge with overlapping coding IDs
m<-merge(JT,c,by="canto_coding_id")

#convert bit codings to 1-13, ignoring multicodings
m[43:79]<-ifelse(m[43:79]==2,1,ifelse(m[43:79]==4,2,ifelse(m[43:79]==8,3,ifelse(m[43:79]==16,4,ifelse(m[43:79]==32,5,ifelse(m[43:79]==64,6,ifelse(m[43:79]==128,7,ifelse(m[43:79]==256,8,ifelse(m[43:79]==512,9,ifelse(m[43:79]==1024,10,ifelse(m[43:79]==2048,11,ifelse(m[43:79]==4096,12,ifelse(m[43:79]==8192,13,NA)))))))))))))

#Count # of NA values
sum(is.na(m[43:79])) #22

##Calculate all weighted Kappas for each of the 37 lines
out <- matrix(NA, nrow=0, ncol=2)
for(i in 1:37){
  rates<-cbind(i,psych::cohen.kappa(m[,c(i+2,i+42)])$weighted.kappa)
  out <- rbind(out,rates)
}
colnames(out)<-c("LineNumber","JT")

#Repeat for other raters:
#HD
m<-merge(HD,c,by="canto_coding_id")
m[43:79]<-ifelse(m[43:79]==2,1,ifelse(m[43:79]==4,2,ifelse(m[43:79]==8,3,ifelse(m[43:79]==16,4,ifelse(m[43:79]==32,5,ifelse(m[43:79]==64,6,ifelse(m[43:79]==128,7,ifelse(m[43:79]==256,8,ifelse(m[43:79]==512,9,ifelse(m[43:79]==1024,10,ifelse(m[43:79]==2048,11,ifelse(m[43:79]==4096,12,ifelse(m[43:79]==8192,13,NA)))))))))))))
out2 <- matrix(NA, nrow=0, ncol=1)
for(i in 1:37){
  rates<-psych::cohen.kappa(m[,c(i+2,i+42)])$weighted.kappa
  out2 <- rbind(out2,rates)
}
colnames(out2)<-"HD"
out<-cbind(out,out2)

#GC
m<-merge(GC,c,by="canto_coding_id")
m[43:79]<-ifelse(m[43:79]==2,1,ifelse(m[43:79]==4,2,ifelse(m[43:79]==8,3,ifelse(m[43:79]==16,4,ifelse(m[43:79]==32,5,ifelse(m[43:79]==64,6,ifelse(m[43:79]==128,7,ifelse(m[43:79]==256,8,ifelse(m[43:79]==512,9,ifelse(m[43:79]==1024,10,ifelse(m[43:79]==2048,11,ifelse(m[43:79]==4096,12,ifelse(m[43:79]==8192,13,NA)))))))))))))
out2 <- matrix(NA, nrow=0, ncol=1)
for(i in 1:37){
  rates<-psych::cohen.kappa(m[,c(i+2,i+42)])$weighted.kappa
  out2 <- rbind(out2,rates)
}
colnames(out2)<-"GC"
out<-cbind(out,out2)

#SS
m<-merge(SS,c,by="canto_coding_id")
m[43:79]<-ifelse(m[43:79]==2,1,ifelse(m[43:79]==4,2,ifelse(m[43:79]==8,3,ifelse(m[43:79]==16,4,ifelse(m[43:79]==32,5,ifelse(m[43:79]==64,6,ifelse(m[43:79]==128,7,ifelse(m[43:79]==256,8,ifelse(m[43:79]==512,9,ifelse(m[43:79]==1024,10,ifelse(m[43:79]==2048,11,ifelse(m[43:79]==4096,12,ifelse(m[43:79]==8192,13,NA)))))))))))))
out2 <- matrix(NA, nrow=0, ncol=1)
for(i in 1:37){
  rates<-psych::cohen.kappa(m[,c(i+2,i+42)])$weighted.kappa
  out2 <- rbind(out2,rates)
}
colnames(out2)<-"SS"
out<-cbind(out,out2)

#YO
m<-merge(YO,c,by="canto_coding_id")
m[43:79]<-ifelse(m[43:79]==2,1,ifelse(m[43:79]==4,2,ifelse(m[43:79]==8,3,ifelse(m[43:79]==16,4,ifelse(m[43:79]==32,5,ifelse(m[43:79]==64,6,ifelse(m[43:79]==128,7,ifelse(m[43:79]==256,8,ifelse(m[43:79]==512,9,ifelse(m[43:79]==1024,10,ifelse(m[43:79]==2048,11,ifelse(m[43:79]==4096,12,ifelse(m[43:79]==8192,13,NA)))))))))))))
out2 <- matrix(NA, nrow=0, ncol=1)
for(i in 1:37){
  rates<-psych::cohen.kappa(m[,c(i+2,i+42)])$weighted.kappa
  out2 <- rbind(out2,rates)
}
colnames(out2)<-"YO"
out<-cbind(out,out2)

#YY
m<-merge(YY,c,by="canto_coding_id")
m[43:79]<-ifelse(m[43:79]==2,1,ifelse(m[43:79]==4,2,ifelse(m[43:79]==8,3,ifelse(m[43:79]==16,4,ifelse(m[43:79]==32,5,ifelse(m[43:79]==64,6,ifelse(m[43:79]==128,7,ifelse(m[43:79]==256,8,ifelse(m[43:79]==512,9,ifelse(m[43:79]==1024,10,ifelse(m[43:79]==2048,11,ifelse(m[43:79]==4096,12,ifelse(m[43:79]==8192,13,NA)))))))))))))
out2 <- matrix(NA, nrow=0, ncol=1)
for(i in 1:37){
  rates<-psych::cohen.kappa(m[,c(i+2,i+42)])$weighted.kappa
  out2 <- rbind(out2,rates)
}
colnames(out2)<-"YY"
out<-cbind(out,out2)

#Print as csv
write.csv(out,"WeightedKappacantoIRR.csv")
d<-read.csv("WeightedKappacantoIRR.csv",header=TRUE,row.names=2)
#Remove dummy column
d<-d[,2:7]
d$Kappa<-rowMeans(d) #add column of means

###Exploratory analysis and power analysis in preparation for doing inter-rater reliability analysis of Global Jukebox data

#Hypothesis 1 power analysis/test
cohensD(d$Kappa, mu = 0) # d = 1.705228
t.test(d$Kappa, alternative="greater") #t = 10.372, df = 36, p-value = 1.159e-12
pwr.t.test(n = 37, d =  , sig.level = 0.025, power = 0.95, type = c("one.sample"), alternative=c("greater")) # d= 0.6091639

#Hypothesis 2 power analysis/test
cohensD(d[c(1, 3:7, 10:11, 13:14, 17:18, 21, 23:25, 29, 32:33, 35),7],d[c(2,8:9,12,15:16,20, 22,26:28,30:31,34, 36:37),7]) # d=1.732657
t.test(d[c(1, 3:7, 10:11, 13:14, 17:18, 21, 23:25, 29, 32:33, 35),7],d[c(2,8:9,12,15:16,20, 22,26:28,30:31,34, 36:37),7], alternative="greater") # t = 5.1353, df = 31.505, p-value = 6.982e-06
pwr.t.test(n = 17, d = , sig.level = 0.025, power = 0.95, type = c("two.sample"), alternative=c("greater")) # d =  1.275456 (i.e., at least 7 preferred and 7 non-preferred features)

##########
######pre-registered code for confirmatory analyses using PES's codings for 3 randomly selected Cantometric codings:

#Read PES cantometrics (including ID rows)
(PES <- read.csv("PESCodings.csv",header=TRUE))

#Reduce to only 30 songs of interest:
(PES<-PES[19:48,])

#fix formatting bugs
PES[,c(2:38,40)]<-as.numeric(as.character(unlist(PES[,c(2:38,40)])))

#merge those with overlapping coding IDs
m<-merge(PES,c,by="canto_coding_id")

#convert bit codings to 1-13, ignoring multicodings
m[43:79]<-ifelse(m[43:79]==2,1,ifelse(m[43:79]==4,2,ifelse(m[43:79]==8,3,ifelse(m[43:79]==16,4,ifelse(m[43:79]==32,5,ifelse(m[43:79]==64,6,ifelse(m[43:79]==128,7,ifelse(m[43:79]==256,8,ifelse(m[43:79]==512,9,ifelse(m[43:79]==1024,10,ifelse(m[43:79]==2048,11,ifelse(m[43:79]==4096,12,ifelse(m[43:79]==8192,13,NA)))))))))))))

#Count # of NA values
sum(is.na(m[43:79]))

##Calculate all weighted Kappas for each of the 37 lines
out <- matrix(NA, nrow=0, ncol=2)
for(i in 1:37){
  rates<-cbind(i,psych::cohen.kappa(m[,c(i+2,i+42)])$weighted.kappa)
  out <- rbind(out,rates)
}
colnames(out)<-c("LineNumber","Kappa")

write.csv(out,"PESIRR.csv")
d<-read.csv("PESIRR.csv",header=TRUE,row.names=2)

###Hypothesis testing

#Hypothesis 1 power analysis/test
t.test(d$Kappa, alternative="greater")

#Hypothesis 2 power analysis/test
t.test(d[c(1, 3:7, 10:11, 13:14, 17:18, 21, 23:25, 29, 32:33, 35),2],d[c(2,8:9,12,15:16,20, 22,26:28,30:31,34, 36:37),2], alternative="greater") 