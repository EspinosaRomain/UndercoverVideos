#Import libraries
library(MASS)
library(scales)    

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

#Set parameters
B=1000 #Bootstrap for estimating SE

#Function to estimate SE
estimateDistributionDeltaM=function(B_funct,data_funct){
  
  #Create subdata for bootstrapping 
  data_SHORT_NOVID_funct=data_funct[data_funct$treatmentVideo==0 & data_funct$treatmentLongSurvey==0,]
  data_SHORT_VID_funct=data_funct[data_funct$treatmentVideo==1 & data_funct$treatmentLongSurvey==0,]
  data_LONG_VID_funct=data_funct[data_funct$treatmentVideo==1 & data_funct$treatmentLongSurvey==1,]
  
  #Store the results
  storeDelta_TT_funct=rep(NA,B_funct)
  storeDelta_M_funct=rep(NA,B_funct)
  
  #Loop to estimate SEs
  for(b_funct in 1:B_funct){
    
    #Print loop number
    print(b_funct)
    
    #Draw data with replacement
    bootdata_SHORT_NOVID_funct=data_SHORT_NOVID_funct[sample(nrow(data_SHORT_NOVID_funct), nrow(data_SHORT_NOVID_funct), replace=TRUE), ]
    bootdata_SHORT_VID_funct=data_SHORT_VID_funct[sample(nrow(data_SHORT_VID_funct), nrow(data_SHORT_VID_funct), replace=TRUE), ]
    bootdata_LONG_VID_funct=data_LONG_VID_funct[sample(nrow(data_LONG_VID_funct), nrow(data_LONG_VID_funct), replace=TRUE), ]
    databoot_funct=rbind(bootdata_SHORT_NOVID_funct,bootdata_SHORT_VID_funct,bootdata_LONG_VID_funct)
    
    #Compute the total treatment 
    storeDelta_TT_funct[b_funct]=mean(databoot_funct[databoot_funct$treatmentVideo==1 & databoot_funct$treatmentLongSurvey==0,]$proAnimals)-mean(databoot_funct[databoot_funct$treatmentVideo==0 & databoot_funct$treatmentLongSurvey==0, ]$proAnimals)
    
    #Compute the mediated treatment effect
    delta_boot_funct=mean(databoot_funct[databoot_funct$treatmentVideo==1 & databoot_funct$treatmentLongSurvey==0,]$proAnimals)-mean(databoot_funct[databoot_funct$treatmentVideo==1 & databoot_funct$treatmentLongSurvey==1, ]$proAnimals)
    q_boot_funct=(mean(databoot_funct[databoot_funct$treatmentVideo==1 & databoot_funct$treatmentLongSurvey==0,]$emotionsAverage)-
                    mean(databoot_funct[databoot_funct$treatmentVideo==1 & databoot_funct$treatmentLongSurvey==1,]$emotionsAverage))/
      (mean(databoot_funct[databoot_funct$treatmentVideo==1 & databoot_funct$treatmentLongSurvey==0,]$emotionsAverage)-
         mean(databoot_funct[databoot_funct$treatmentVideo==0 & databoot_funct$treatmentLongSurvey==0,]$emotionsAverage))
    Delta_M_boot_funct=delta_boot_funct/q_boot_funct
    
    #Store the result
    storeDelta_M_funct[b_funct]=Delta_M_boot_funct
  }
  
  return(list(storeDelta_TT=storeDelta_TT_funct,storeDelta_M=storeDelta_M_funct))
  
}

#Estimate the distribution
results=estimateDistributionDeltaM(B_funct=B,data_funct=mydata)
distribDeltaM=results$storeDelta_M
distribDeltaTT=results$storeDelta_TT

quantile(distribDeltaM, c(0.95)) #The 95th percentile
quantile(distribDeltaM, c(0.025, 0.975)) #The 95th percentile

quantile(distribDeltaTT, c(0.025, 0.975)) #The 95th percentile

##Check the 5% percentile for the mediated Treatment
FifthPerc=quantile(distribDeltaM, c(0.05))
mylabel=paste0("5th percentile=\n",round(FifthPerc,3))

#Plot distribution of Delta_M boostrapped
mygraph=ggplot(data=data.frame(distribDeltaM), aes(distribDeltaM))+
  geom_histogram(aes(y=after_stat(density)))+theme_minimal()+
  stat_function(fun=dnorm, args=list(mean = mean(data.frame(distribDeltaM)$distribDeltaM, na.rm=TRUE), 
                                     sd=sd(data.frame(distribDeltaM)$distribDeltaM, na.rm  = TRUE)), 
                colour="black",size=1)+labs(x="Distribution of the bootstrapped mediated treatment effect", y="Density")+
  geom_vline(xintercept = FifthPerc, size=1, color="#E77D73")+
  geom_label(aes(x = -0.012, y = 25, label = mylabel), fill = "white", color="#E77D73", label.size = 1, size=4)+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.background = element_blank())
mygraph

pdf(file = "Figures/Delta_M.pdf", width = 5, height = 5)
mygraph
dev.off()

mean(distribDeltaM) #Mean effect
t=ecdf(distribDeltaM)
t(0) #Get the percentile of the distrib where it is equal to 0.
1-t(0.03)

##Check the 5% percentile for the total Treatment
FifthPerc=quantile(distribDeltaTT, c(0.05))
mylabel=paste0("5th percentile=\n",round(FifthPerc,3))

#Plot distribution of Delta_M boostrapped
mygraph=ggplot(data=data.frame(distribDeltaTT), aes(distribDeltaTT))+
  geom_histogram(aes(y=after_stat(density)))+theme_minimal()+
  stat_function(fun=dnorm, args=list(mean = mean(data.frame(distribDeltaTT)$distribDeltaTT, na.rm=TRUE), 
                                     sd=sd(data.frame(distribDeltaTT)$distribDeltaTT, na.rm  = TRUE)), 
                colour="black",size=1)+labs(x="Distribution of the bootstrapped total treatment effect", y="Density")+
  geom_vline(xintercept = FifthPerc, size=1, color="#79BA6D")+
  #geom_label(aes(x = -0.055, y = 8.3, label = mylabel), fill = "white", color="black")+
  geom_label(aes(x = 0.016, y = 25, label = mylabel), fill = "white", color="#79BA6D", label.size = 1, size=4)+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.background = element_blank())
mygraph

pdf(file = "Figures/Delta_TT.pdf", width = 5, height = 5)
mygraph
dev.off()

mean(distribDeltaTT) #Mean effect
t=ecdf(distribDeltaTT)
t(0) #Get the percentile of the distrib where it is equal to 0.


#Share of total effect size explained by the mediated effect
dfShare=data.frame(shareExplained=distribDeltaM/distribDeltaTT)
dfShare$shareExplained=ifelse(dfShare$shareExplained<0,0,dfShare$shareExplained)
dfShare$shareExplained=ifelse(dfShare$shareExplained>1,1,dfShare$shareExplained)

Perc=quantile(dfShare$shareExplained, c(0.05,0.25,0.5,0.75,0.95))
mylabel=paste0("5th percentile=",round(Perc[1],3))
mylabel=paste0(mylabel,"\n25th percentile=",round(Perc[2],3))
mylabel=paste0(mylabel,"\n50th percentile=",round(Perc[3],3))
mylabel=paste0(mylabel,"\n75th percentile=",round(Perc[4],3))
mylabel=paste0(mylabel,"\n95th percentile=",round(Perc[5],3))


mygraph=ggplot(dfShare, aes(x=shareExplained)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(x="Share of the total effect explained by the mediated effect", y="Density of bootstrap estimates")+
  geom_label(aes(x = 0.65, y = 4, label = mylabel), label.size = NA, hjust = "left")+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.background = element_blank())
mygraph

pdf(file = "Figures/ShareEffect.pdf", width = 5, height = 5)
mygraph
dev.off()

t=ecdf(dfShare$shareExplained)
t(0) #Get the percentile of the distrib where it is equal to 0.

mean(dfShare$shareExplained)
quantile(dfShare$shareExplained, c(0.025,0.975))

