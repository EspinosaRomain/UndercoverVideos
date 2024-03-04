#Import libraries
library(plyr)
library(dplyr)
library(factoextra)
library(psy)

#Set working Directory
######
setwd("")
######

#Load user-written functions
source("Scripts/userFunctions.R")

#Set seed
set.seed(123)

#Import data
mydata=readRDS("Data/anonymData.RDS")

#Treatment dummy for being in the treatment with the video
mydata$treatmentVideo=ifelse(mydata$Group=="Video1" | mydata$Group=="Video2",1,0)

#Treatment dummy for long survey
mydata$treatmentLongSurvey=ifelse(mydata$shortSurvey=="Yes",0,1)

#Distrib of participants who passed quota, consent, and sound test
t=table(mydata$treatmentLongSurvey,mydata$treatmentVideo)
t
round(table(mydata$treatmentLongSurvey,mydata$treatmentVideo)/sum(table(mydata$treatmentLongSurvey,mydata$treatmentVideo))*100,1)

#Treatment dummy for being in the treatment with the video
table(mydata$Group)
mydata$treatmentVideo=ifelse(mydata$Group=="Video1" | mydata$Group=="Video2",1,0)

#Treatment dummy for long survey
mydata$treatmentLongSurvey=ifelse(mydata$shortSurvey=="Yes",0,1)

table(mydata$treatmentVideo,mydata$treatmentLongSurvey)
#We have  (short-noVid),  (short-vid),  (long-novid),  (long-vid)

#Cross treatment dummy
mydata$videoLong=ifelse(mydata$treatmentVideo==1 & mydata$treatmentLongSurvey==1,1,0)

#Socio-demographics
mydata$female=ifelse(mydata$Gender=="Femme",1,0)
mydata$age=as.numeric(mydata$Age)

#Generate PMJ (sum of individual factors)
mydata$PMJ=0
vecProMeat=c("ProMeat_1","ProMeat_2","ProMeat_3","ProMeat_4",
             "ProMeat_5","ProMeat_6","ProMeat_7","ProMeat_8","ProMeat_9","ProMeat_10")
i=1
for(j in vecProMeat){
  mydata[[j]]=likertToNumeric(mydata[[j]])
  if(i==1) mydata$PMJ=mydata[[j]]
  if(i>1) mydata$PMJ=mydata[[j]]+mydata$PMJ
  i=i+1
}
mydata$PMJ=mydata$PMJ/70

#Generate sum of moral concerns
mydata$sumConcerns=0
vecConcerns=c("Moral.concern.animal_1","Moral.concern.animal_2","Moral.concern.animal_3","Moral.concern.animal_4",
             "Moral.concern.animal_5")
for(j in vecConcerns){
  mydata[[j]]=likertToNumeric(mydata[[j]])
  mydata$sumConcerns=mydata$sumConcerns+mydata[[j]]
}
mydata$sumConcerns=mydata$sumConcerns/50

#Recode emotions into numeric variables
vecEmotions=c("Fear","Sadness","Happiness","Anger","Surprise","Disgust")
for(j in vecEmotions){
  mydata[[j]]=likertToNumericEmotions(mydata[[j]])
}

#Recode sharing
mydata$Sharing=likertToNumericSharing(mydata$Sharing)

#PCA on emotions
pca_emotions=prcomp(mydata[,vecEmotions], center = TRUE,scale. = TRUE)
summary(pca_emotions)
var_pca_emotionsVar<- get_pca_var(pca_emotions)
var_pca_emotionsVar$cor[,1:3]
mydata$emotionsPCA<- -get_pca_ind(pca_emotions)$coord[,1]

#Negative-emotion score
mydata$RevertedHappiness=-mydata$Happiness
vecEmotionsReduced=c("Fear","Sadness","Anger","Surprise","Disgust","RevertedHappiness")
cronbach(mydata[,vecEmotionsReduced])
mydata$emotionsAverage=(rowSums(mydata[,vecEmotionsReduced])+2)/36

#Food: animal-based consumption
mydata$redMeat=foodFreqIntoNumeric(mydata$Food.consumption_1)
mydata$whiteMeat=foodFreqIntoNumeric(mydata$Food.consumption_2)
mydata$fish=foodFreqIntoNumeric(mydata$Food.consumption_3)
mydata$eggs=foodFreqIntoNumeric(mydata$Food.consumption_4)
mydata$dairy=foodFreqIntoNumeric(mydata$Food.consumption_5)
mydata$vegetables=foodFreqIntoNumeric(mydata$Food.consumption_6)
mydata$legumes=foodFreqIntoNumeric(mydata$Food.consumption_7)
mydata$fruits=foodFreqIntoNumeric(mydata$Food.consumption_8)
mydata$starchy=foodFreqIntoNumeric(mydata$Food.consumption_9)
foodVar=c("redMeat","whiteMeat","fish","eggs","dairy","vegetables","legumes","fruits","starchy")

#PCA on food variables
pca_foodVar=prcomp(mydata[,foodVar], center = TRUE,scale. = TRUE)
summary(pca_foodVar)
var_pca_foodVar<- get_pca_var(pca_foodVar)
var_pca_foodVar$cor[,1:3]
mydata$ABC<- -get_pca_ind(pca_foodVar)$coord[,2]

#Rename Dependent Variables
#depVar=c("Don_assos_1","Don_assos_2","Don_assos_3","Don_assos_10","Don_petitions_1","Don_petitions_2","Don_petitions_3")
mydata=mydata %>% rename(Don_AssiettesVegetale = Don.assos_1,
                         Don_L214=Don.assos_2,
                         Don_SPA=Don.assos_3,
                         Don_Welfarm=Don.assos_10,
                         Don_Pet_Intensive=Don.petitions_1,
                         Don_Pet_VegMenus=Don.petitions_2,
                         Don_Pet_ProtectAct=Don.petitions_3)
depVar=c("Don_AssiettesVegetale","Don_L214","Don_SPA","Don_Welfarm","Don_Pet_Intensive","Don_Pet_VegMenus","Don_Pet_ProtectAct")
for(j in depVar){
  mydata[[j]]=as.numeric(mydata[[j]])
}

mydata$donsAsso=(mydata$Don_AssiettesVegetale+mydata$Don_L214+mydata$Don_SPA+mydata$Don_Welfarm)/4
mydata$donsPetitions=(mydata$Don_Pet_Intensive+mydata$Don_Pet_VegMenus+mydata$Don_Pet_ProtectAct)/3

#Generate dependent variable: proAnimals
depVar=c("Don_AssiettesVegetale","Don_L214","Don_SPA","Don_Welfarm","Don_Pet_Intensive","Don_Pet_VegMenus","Don_Pet_ProtectAct")
cronbach(mydata[,depVar])
mydata$proAnimals=rowSums(mydata[,depVar])/7/100

#Recode truth variables
vecTruth=c("Truth_1","Truth_2","Truth_3","Truth_4",
              "Truth_5","Truth_6","Truth_7")
for(j in vecTruth){
  mydata[[j]]=likertToNumericTruth(mydata[[j]])
}

#Recode reactance
mydata$Eat.Meat..motivation_1=as.numeric(mydata$Eat.Meat..motivation_1)

#Save data
saveRDS(mydata,"Data/treatedData.RDS")