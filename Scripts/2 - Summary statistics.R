#Import libraries
library(plyr)
library(pastecs)
library(factoextra)
library(dplyr)
library(psy)
library(ivreg)
library(stargazer)
library(lmtest)
library(sandwich)
library(modelsummary)
library(systemfit)
library(multcomp)
library(sensemakr)

#Set working Directory
######
setwd("")
######

#Load user-written functions
source("Scripts/FunctionMatrixStatDesc.R")

#Set seed
set.seed(123)

#Import data
mydata=readRDS("Data/treatedData.RDS")

#Descriptive statistics 
round(table(mydata$female)/sum(table(mydata$female)),2)
mean(mydata$age, na.rm=TRUE)
sd(mydata$age, na.rm=TRUE)

#Treatment assignment
table(mydata$treatmentLongSurvey,mydata$treatmentVideo)
round(table(mydata$treatmentLongSurvey,mydata$treatmentVideo)/sum(table(mydata$treatmentLongSurvey,mydata$treatmentVideo))*100,1)

#Look at time difference in NOVID for with and without the long survey
t1=median(mydata[mydata$treatmentVideo==0 & mydata$treatmentLongSurvey==0, ]$Duration..in.seconds., na.rm=TRUE) #Time NOVID-SHORT
t2=median(mydata[mydata$treatmentVideo==0 & mydata$treatmentLongSurvey==1, ]$Duration..in.seconds., na.rm=TRUE) #Time NOVID-LONG
(t2-t1)/60 #Time due to the long survey

t1=median(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0, ]$Duration..in.seconds., na.rm=TRUE) #Time NOVID-SHORT
t2=median(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1, ]$Duration..in.seconds., na.rm=TRUE) #Time NOVID-LONG
(t2-t1)/60 #Time due to the long survey

####DEPENDENT VARIABLES####

#Cronbach's alpha for donations
varDonations=c("Don_AssiettesVegetale","Don_L214","Don_SPA","Don_Welfarm","Don_Pet_Intensive","Don_Pet_VegMenus","Don_Pet_ProtectAct")
cronbach(mydata[,varDonations])

#Cronbach's alpha for emotions
vecEmotionsRev=c("Fear","Sadness","Anger","Surprise","Disgust","RevertedHappiness")
cronbach(mydata[,vecEmotionsRev])


####ANALYSIS OF DONATIONS####

#Stat Des by treatment for donations
StatDes=matrix(data=NA,nrow=4,ncol=2)
rownames(StatDes)=c("NOVID-SHORT","VID-SHORT","NOVID-LONG","VID-LONG")
colnames(StatDes)=c("Mean","SE")
StatDes[1,1]=round(stat.desc(mydata[mydata$treatmentVideo==0 & mydata$treatmentLongSurvey==0,]$proAnimals)["mean"],3)
StatDes[1,2]=round(stat.desc(mydata[mydata$treatmentVideo==0 & mydata$treatmentLongSurvey==0,]$proAnimals)["SE.mean"],3)
StatDes[2,1]=round(stat.desc(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$proAnimals)["mean"],3)
StatDes[2,2]=round(stat.desc(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$proAnimals)["SE.mean"],3)
StatDes[3,1]=round(stat.desc(mydata[mydata$treatmentVideo==0 & mydata$treatmentLongSurvey==1,]$proAnimals)["mean"],3)
StatDes[3,2]=round(stat.desc(mydata[mydata$treatmentVideo==0 & mydata$treatmentLongSurvey==1,]$proAnimals)["SE.mean"],3)
StatDes[4,1]=round(stat.desc(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$proAnimals)["mean"],3)
StatDes[4,2]=round(stat.desc(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$proAnimals)["SE.mean"],3)
StatDes

#Stat Des by treatment for proanimals
proAnimTable=matMeanConditions(mydata,"proAnimals","treatmentVideo","treatmentLongSurvey")
write.csv(proAnimTable, "Tables/proAnimTable.csv")

#Donations by treatment MEANS and SE
DonationsStat=matrix(data=NA, ncol=8, nrow=7)
DonationsStat[1,1:4]=matMeanConditions(mydata,"Don_AssiettesVegetale","treatmentVideo","treatmentLongSurvey")[,1]
DonationsStat[2,1:4]=matMeanConditions(mydata,"Don_L214","treatmentVideo","treatmentLongSurvey")[,1]
DonationsStat[3,1:4]=matMeanConditions(mydata,"Don_SPA","treatmentVideo","treatmentLongSurvey")[,1]
DonationsStat[4,1:4]=matMeanConditions(mydata,"Don_Welfarm","treatmentVideo","treatmentLongSurvey")[,1]
DonationsStat[5,1:4]=matMeanConditions(mydata,"Don_Pet_Intensive","treatmentVideo","treatmentLongSurvey")[,1]
DonationsStat[6,1:4]=matMeanConditions(mydata,"Don_Pet_VegMenus","treatmentVideo","treatmentLongSurvey")[,1]
DonationsStat[7,1:4]=matMeanConditions(mydata,"Don_Pet_ProtectAct","treatmentVideo","treatmentLongSurvey")[,1]
DonationsStat[1,5:8]=matMeanConditions(mydata,"Don_AssiettesVegetale","treatmentVideo","treatmentLongSurvey")[,2]
DonationsStat[2,5:8]=matMeanConditions(mydata,"Don_L214","treatmentVideo","treatmentLongSurvey")[,2]
DonationsStat[3,5:8]=matMeanConditions(mydata,"Don_SPA","treatmentVideo","treatmentLongSurvey")[,2]
DonationsStat[4,5:8]=matMeanConditions(mydata,"Don_Welfarm","treatmentVideo","treatmentLongSurvey")[,2]
DonationsStat[5,5:8]=matMeanConditions(mydata,"Don_Pet_Intensive","treatmentVideo","treatmentLongSurvey")[,2]
DonationsStat[6,5:8]=matMeanConditions(mydata,"Don_Pet_VegMenus","treatmentVideo","treatmentLongSurvey")[,2]
DonationsStat[7,5:8]=matMeanConditions(mydata,"Don_Pet_ProtectAct","treatmentVideo","treatmentLongSurvey")[,2]
DonationsStat
write.csv(DonationsStat, "Tables/donationsStat.csv")

#Impact on each donation possibility
DonationsMatrix=matTreatmentDiff(mydata[mydata$treatmentLongSurvey==0,],c(varDonations,"proAnimals"),"treatmentVideo")
DonationsMatrix
DonationsMatrix[,2]-DonationsMatrix[,1]

#Test the outcome neutral test for which estimator to use
wilcox.test(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$proAnimals,mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$proAnimals)
mean(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$proAnimals) - mean(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$proAnimals)

#Extensive margin
matTreatmentDiff_ShareZeros(mydata[mydata$treatmentLongSurvey==0,], varDonations,"treatmentVideo")

####EMOTIONS####

#Difference in emotions with/without video
vecEmotions=c("Fear","Sadness","Happiness","Anger","Surprise","Disgust")
matTreatmentDiff(mydata[mydata$treatmentLongSurvey==0,],c(vecEmotions,"emotionsAverage"),"treatmentVideo")

#Descriptive by treatment for emotions
StatDes=matrix(data=NA,nrow=4,ncol=2)
rownames(StatDes)=c("NOVID-SHORT","VID-SHORT","NOVID-LONG","VID-LONG")
colnames(StatDes)=c("Mean","SE")
StatDes[1,1]=round(stat.desc(mydata[mydata$treatmentVideo==0 & mydata$treatmentLongSurvey==0,]$emotionsAverage)["mean"],3)
StatDes[1,2]=round(stat.desc(mydata[mydata$treatmentVideo==0 & mydata$treatmentLongSurvey==0,]$emotionsAverage)["SE.mean"],3)
StatDes[2,1]=round(stat.desc(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$emotionsAverage)["mean"],3)
StatDes[2,2]=round(stat.desc(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$emotionsAverage)["SE.mean"],3)
StatDes[3,1]=round(stat.desc(mydata[mydata$treatmentVideo==0 & mydata$treatmentLongSurvey==1,]$emotionsAverage)["mean"],3)
StatDes[3,2]=round(stat.desc(mydata[mydata$treatmentVideo==0 & mydata$treatmentLongSurvey==1,]$emotionsAverage)["SE.mean"],3)
StatDes[4,1]=round(stat.desc(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$emotionsAverage)["mean"],3)
StatDes[4,2]=round(stat.desc(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$emotionsAverage)["SE.mean"],3)
StatDes

write.csv(StatDes, "Tables/emotionsTable.csv")

#Difference in donations due to COOL-OFF in absence of videos.
matTreatmentDiff(mydata[mydata$treatmentVideo==0,],c(vecEmotions,"proAnimals"),"treatmentLongSurvey")

#Difference in emotions with/without cool-off period
matTreatmentDiff(mydata[mydata$treatmentVideo==1,],c(vecEmotions,"emotionsAverage"),"treatmentLongSurvey")

#Difference in dependent variables with/without cool-off period
matTreatmentDiff(mydata[mydata$treatmentVideo==1,],c(vecEmotions,"proAnimals"),"treatmentLongSurvey")

#Emotions by treatment
EmotionsStat=matrix(data=NA, ncol=8, nrow=6)
EmotionsStat[1,1:4]=matMeanConditions(mydata,"Fear","treatmentVideo","treatmentLongSurvey")[,1]
EmotionsStat[2,1:4]=matMeanConditions(mydata,"Sadness","treatmentVideo","treatmentLongSurvey")[,1]
EmotionsStat[3,1:4]=matMeanConditions(mydata,"Anger","treatmentVideo","treatmentLongSurvey")[,1]
EmotionsStat[4,1:4]=matMeanConditions(mydata,"Surprise","treatmentVideo","treatmentLongSurvey")[,1]
EmotionsStat[5,1:4]=matMeanConditions(mydata,"Disgust","treatmentVideo","treatmentLongSurvey")[,1]
EmotionsStat[6,1:4]=matMeanConditions(mydata,"RevertedHappiness","treatmentVideo","treatmentLongSurvey")[,1]
EmotionsStat[1,5:8]=matMeanConditions(mydata,"Fear","treatmentVideo","treatmentLongSurvey")[,2]
EmotionsStat[2,5:8]=matMeanConditions(mydata,"Sadness","treatmentVideo","treatmentLongSurvey")[,2]
EmotionsStat[3,5:8]=matMeanConditions(mydata,"Anger","treatmentVideo","treatmentLongSurvey")[,2]
EmotionsStat[4,5:8]=matMeanConditions(mydata,"Surprise","treatmentVideo","treatmentLongSurvey")[,2]
EmotionsStat[5,5:8]=matMeanConditions(mydata,"Disgust","treatmentVideo","treatmentLongSurvey")[,2]
EmotionsStat[6,5:8]=matMeanConditions(mydata,"RevertedHappiness","treatmentVideo","treatmentLongSurvey")[,2]
EmotionsStat[6,1:4]=EmotionsStat[6,1:4]+8
EmotionsStat
write.csv(EmotionsStat, "Tables/emotionsStat.csv")

#Impact on emotions of the video
m=matTreatmentDiff(mydata[mydata$treatmentLongSurvey==0,],c(vecEmotions),"treatmentVideo")
m
m=cbind(m,m[,2]-m[,1])
colnames(m)[4]="Difference"
m

#Impact on emotions of the cool-off
m=matTreatmentDiff(mydata[mydata$treatmentVideo==1,],c(vecEmotions),"treatmentLongSurvey")
m
m=cbind(m,m[,2]-m[,1])
colnames(m)[4]="Difference"
m


#MORAL CONCERNS
MoralConcernsStat=matrix(data=NA, ncol=8, nrow=5)
MoralConcernsStat[1,1:4]=matMeanConditions(mydata,"Moral.concern.animal_1","treatmentVideo","treatmentLongSurvey")[,1]
MoralConcernsStat[2,1:4]=matMeanConditions(mydata,"Moral.concern.animal_2","treatmentVideo","treatmentLongSurvey")[,1]
MoralConcernsStat[3,1:4]=matMeanConditions(mydata,"Moral.concern.animal_3","treatmentVideo","treatmentLongSurvey")[,1]
MoralConcernsStat[4,1:4]=matMeanConditions(mydata,"Moral.concern.animal_4","treatmentVideo","treatmentLongSurvey")[,1]
MoralConcernsStat[5,1:4]=matMeanConditions(mydata,"Moral.concern.animal_5","treatmentVideo","treatmentLongSurvey")[,1]
MoralConcernsStat[1,5:8]=matMeanConditions(mydata,"Moral.concern.animal_1","treatmentVideo","treatmentLongSurvey")[,2]
MoralConcernsStat[2,5:8]=matMeanConditions(mydata,"Moral.concern.animal_2","treatmentVideo","treatmentLongSurvey")[,2]
MoralConcernsStat[3,5:8]=matMeanConditions(mydata,"Moral.concern.animal_3","treatmentVideo","treatmentLongSurvey")[,2]
MoralConcernsStat[4,5:8]=matMeanConditions(mydata,"Moral.concern.animal_4","treatmentVideo","treatmentLongSurvey")[,2]
MoralConcernsStat[5,5:8]=matMeanConditions(mydata,"Moral.concern.animal_5","treatmentVideo","treatmentLongSurvey")[,2]
MoralConcernsStat
write.csv(MoralConcernsStat, "Tables/MoralConcernsStat.csv")

#Morally wrong
StatDes=matrix(data=NA,nrow=2,ncol=2)
rownames(StatDes)=c("VID-SHORT","VID-LONG")
colnames(StatDes)=c("Mean","SE")
StatDes[1,1]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Morally.wrong_1))["mean"],3)
StatDes[1,2]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Morally.wrong_1))["SE.mean"],3)
StatDes[2,1]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Morally.wrong_1))["mean"],3)
StatDes[2,2]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Morally.wrong_1))["SE.mean"],3)
StatDes


#Share
StatDes=matrix(data=NA,nrow=2,ncol=2)
rownames(StatDes)=c("VID-SHORT","VID-LONG")
colnames(StatDes)=c("Mean","SE")
StatDes[1,1]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Sharing))["mean"],3)
StatDes[1,2]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Sharing))["SE.mean"],3)
StatDes[2,1]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Sharing))["mean"],3)
StatDes[2,2]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Sharing))["SE.mean"],3)
StatDes

ci95(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Sharing))
ci95(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Sharing))

round(stat.desc(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Sharing-1)["nbr.null"]/stat.desc(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Sharing)["nbr.val"],3)
round(stat.desc(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Sharing-1)["nbr.null"]/stat.desc(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Sharing)["nbr.val"],3)

round(sum(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Sharing==7)/stat.desc(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Sharing)["nbr.val"],3)
round(sum(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Sharing==7)/stat.desc(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Sharing)["nbr.val"],3)

#Truth by treatment
TruthStat=matrix(data=NA, ncol=4, nrow=7)
TruthStat[1,1]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Truth_1))["mean"],3)
TruthStat[2,1]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Truth_2))["mean"],3)
TruthStat[3,1]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Truth_3))["mean"],3)
TruthStat[4,1]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Truth_4))["mean"],3)
TruthStat[5,1]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Truth_5))["mean"],3)
TruthStat[6,1]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Truth_6))["mean"],3)
TruthStat[7,1]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Truth_7))["mean"],3)
TruthStat[1,2]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Truth_1))["mean"],3)
TruthStat[2,2]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Truth_2))["mean"],3)
TruthStat[3,2]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Truth_3))["mean"],3)
TruthStat[4,2]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Truth_4))["mean"],3)
TruthStat[5,2]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Truth_5))["mean"],3)
TruthStat[6,2]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Truth_6))["mean"],3)
TruthStat[7,2]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Truth_7))["mean"],3)
TruthStat[1,3]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Truth_1))["SE.mean"],3)
TruthStat[2,3]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Truth_2))["SE.mean"],3)
TruthStat[3,3]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Truth_3))["SE.mean"],3)
TruthStat[4,3]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Truth_4))["SE.mean"],3)
TruthStat[5,3]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Truth_5))["SE.mean"],3)
TruthStat[6,3]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Truth_6))["SE.mean"],3)
TruthStat[7,3]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$Truth_7))["SE.mean"],3)
TruthStat[1,4]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Truth_1))["SE.mean"],3)
TruthStat[2,4]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Truth_2))["SE.mean"],3)
TruthStat[3,4]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Truth_3))["SE.mean"],3)
TruthStat[4,4]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Truth_4))["SE.mean"],3)
TruthStat[5,4]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Truth_5))["SE.mean"],3)
TruthStat[6,4]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Truth_6))["SE.mean"],3)
TruthStat[7,4]=round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$Truth_7))["SE.mean"],3)
TruthStat
write.csv(TruthStat, "Tables/TruthStat.csv")

#Focus on perception of the video:
round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1,]$Truth_1))["mean"],3)
round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1,]$Truth_2))["mean"],3)
round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1,]$Truth_3))["mean"],3)
round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1,]$Truth_4))["mean"],3)
round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1,]$Truth_5))["mean"],3)
round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1,]$Truth_6))["mean"],3)
round(stat.desc(as.numeric(mydata[mydata$treatmentVideo==1,]$Truth_7))["mean"],3)

ci95(as.numeric(mydata[mydata$treatmentVideo==1,]$Truth_1))
ci95(as.numeric(mydata[mydata$treatmentVideo==1,]$Truth_2))
ci95(as.numeric(mydata[mydata$treatmentVideo==1,]$Truth_3))
ci95(as.numeric(mydata[mydata$treatmentVideo==1,]$Truth_4))
ci95(as.numeric(mydata[mydata$treatmentVideo==1,]$Truth_5))
ci95(as.numeric(mydata[mydata$treatmentVideo==1,]$Truth_6))
ci95(as.numeric(mydata[mydata$treatmentVideo==1,]$Truth_7))

#PMJ by treatment
PMJStat=matrix(data=NA, ncol=8, nrow=10)
PMJStat[1,1:4]=matMeanConditions(mydata,"ProMeat_1","treatmentVideo","treatmentLongSurvey")[,1]
PMJStat[2,1:4]=matMeanConditions(mydata,"ProMeat_2","treatmentVideo","treatmentLongSurvey")[,1]
PMJStat[3,1:4]=matMeanConditions(mydata,"ProMeat_3","treatmentVideo","treatmentLongSurvey")[,1]
PMJStat[4,1:4]=matMeanConditions(mydata,"ProMeat_4","treatmentVideo","treatmentLongSurvey")[,1]
PMJStat[5,1:4]=matMeanConditions(mydata,"ProMeat_5","treatmentVideo","treatmentLongSurvey")[,1]
PMJStat[6,1:4]=matMeanConditions(mydata,"ProMeat_6","treatmentVideo","treatmentLongSurvey")[,1]
PMJStat[7,1:4]=matMeanConditions(mydata,"ProMeat_7","treatmentVideo","treatmentLongSurvey")[,1]
PMJStat[8,1:4]=matMeanConditions(mydata,"ProMeat_8","treatmentVideo","treatmentLongSurvey")[,1]
PMJStat[9,1:4]=matMeanConditions(mydata,"ProMeat_9","treatmentVideo","treatmentLongSurvey")[,1]
PMJStat[10,1:4]=matMeanConditions(mydata,"ProMeat_10","treatmentVideo","treatmentLongSurvey")[,1]
PMJStat[1,5:8]=matMeanConditions(mydata,"ProMeat_1","treatmentVideo","treatmentLongSurvey")[,2]
PMJStat[2,5:8]=matMeanConditions(mydata,"ProMeat_2","treatmentVideo","treatmentLongSurvey")[,2]
PMJStat[3,5:8]=matMeanConditions(mydata,"ProMeat_3","treatmentVideo","treatmentLongSurvey")[,2]
PMJStat[4,5:8]=matMeanConditions(mydata,"ProMeat_4","treatmentVideo","treatmentLongSurvey")[,2]
PMJStat[5,5:8]=matMeanConditions(mydata,"ProMeat_5","treatmentVideo","treatmentLongSurvey")[,2]
PMJStat[6,5:8]=matMeanConditions(mydata,"ProMeat_6","treatmentVideo","treatmentLongSurvey")[,2]
PMJStat[7,5:8]=matMeanConditions(mydata,"ProMeat_7","treatmentVideo","treatmentLongSurvey")[,2]
PMJStat[8,5:8]=matMeanConditions(mydata,"ProMeat_8","treatmentVideo","treatmentLongSurvey")[,2]
PMJStat[9,5:8]=matMeanConditions(mydata,"ProMeat_9","treatmentVideo","treatmentLongSurvey")[,2]
PMJStat[10,5:8]=matMeanConditions(mydata,"ProMeat_10","treatmentVideo","treatmentLongSurvey")[,2]
PMJStat
write.csv(PMJStat, "Tables/PMJStat.csv")

matMeanConditions(mydata,"PMJ","treatmentVideo","treatmentLongSurvey")

ci95(mydata[mydata$treatmentVideo==0 & mydata$treatmentLongSurvey==0,]$PMJ)
ci95(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$PMJ)
ci95(mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$PMJ)

#Additional statistics for the paper of PMJ
wilcox.test(mydata[mydata$treatmentVideo==0 & mydata$treatmentLongSurvey==0,]$PMJ,mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$PMJ)
wilcox.test(mydata[mydata$treatmentVideo==0 & mydata$treatmentLongSurvey==0,]$PMJ,mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$PMJ)

#Difference between NOVID-SHORT & VID-SHORT for PMJ
wilcox.test(mydata[mydata$treatmentVideo==0 & mydata$treatmentLongSurvey==0,]$PMJ,mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==0,]$PMJ)

#Difference between NOVID-SHORT & VID-LONG for PMJ
wilcox.test(mydata[mydata$treatmentVideo==0 & mydata$treatmentLongSurvey==0,]$PMJ,mydata[mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,]$PMJ)




