library(pastecs)

matStatDes=function(df_funct,listVar_funct){
  Mat_funct=matrix(data=NA,nrow=length(listVar_funct),ncol=6)
  colnames(Mat_funct)=c("Mean","SD","Median","Min","Max","N")
  rownames(Mat_funct)=listVar_funct
  k_funct=1
  for(j_funct in listVar_funct){
    tempStatDes=stat.desc(df_funct[[j_funct]])
    Mat_funct[k_funct,1]=round(tempStatDes["mean"],3)
    Mat_funct[k_funct,2]=round(tempStatDes["std.dev"],3)
    Mat_funct[k_funct,3]=round(tempStatDes["median"],3)
    Mat_funct[k_funct,4]=round(tempStatDes["min"],3)
    Mat_funct[k_funct,5]=round(tempStatDes["max"],3)
    Mat_funct[k_funct,6]=round(tempStatDes["nbr.val"],3)
    k_funct=k_funct+1
  }
  Mat_funct
}

library(plyr)
matTreatmentDiff=function(df_funct,listVar_funct,treatment_var_funct){
  Mat_funct=matrix(data=NA,nrow=length(listVar_funct),ncol=3)
  colnames(Mat_funct)=c("Control","Treated","Pvalue")
  rownames(Mat_funct)=listVar_funct
  df0=df_funct[df_funct[[treatment_var_funct]]==0,]
  df1=df_funct[df_funct[[treatment_var_funct]]==1,]
  k_funct=1
  for(j_funct in listVar_funct){
    Mat_funct[k_funct,1]=round(mean(df0[[j_funct]]),3)
    Mat_funct[k_funct,2]=round(mean(df1[[j_funct]]),3)
    Mat_funct[k_funct,3]=round(wilcox.test(df0[[j_funct]],df1[[j_funct]],exact=FALSE)$p.value,4)
    k_funct=k_funct+1
  }
  Mat_funct
}


#Stat Des for the four conditions
matMeanConditions=function(df_funct,var_funct,treatment_var_funct1,treatment_var_funct2){
  StatDes_funct=matrix(data=NA,nrow=4,ncol=2)
  rownames(StatDes_funct)=c("NOVID-SHORT","VID-SHORT","NOVID-LONG","VID-LONG")
  colnames(StatDes_funct)=c("Mean","SE")
  
  tmp_data_funct=df_funct[df_funct[[treatment_var_funct1]]==0 & df_funct[[treatment_var_funct2]]==0,]
  StatDes_funct[1,1]=round(stat.desc(tmp_data_funct[[var_funct]])["mean"],3)
  tmp_data_funct=df_funct[df_funct[[treatment_var_funct1]]==0 & df_funct[[treatment_var_funct2]]==0,]
  StatDes_funct[1,2]=round(stat.desc(tmp_data_funct[[var_funct]])["SE.mean"],3)
  
  tmp_data_funct=df_funct[df_funct[[treatment_var_funct1]]==1 & df_funct[[treatment_var_funct2]]==0,]
  StatDes_funct[2,1]=round(stat.desc(tmp_data_funct[[var_funct]])["mean"],3)
  tmp_data_funct=df_funct[df_funct[[treatment_var_funct1]]==1 & df_funct[[treatment_var_funct2]]==0,]
  StatDes_funct[2,2]=round(stat.desc(tmp_data_funct[[var_funct]])["SE.mean"],3)
  
  tmp_data_funct=df_funct[df_funct[[treatment_var_funct1]]==0 & df_funct[[treatment_var_funct2]]==1,]
  StatDes_funct[3,1]=round(stat.desc(tmp_data_funct[[var_funct]])["mean"],3)
  tmp_data_funct=df_funct[df_funct[[treatment_var_funct1]]==0 & df_funct[[treatment_var_funct2]]==1,]
  StatDes_funct[3,2]=round(stat.desc(tmp_data_funct[[var_funct]])["SE.mean"],3)
  
  tmp_data_funct=df_funct[df_funct[[treatment_var_funct1]]==1 & df_funct[[treatment_var_funct2]]==1,]
  StatDes_funct[4,1]=round(stat.desc(tmp_data_funct[[var_funct]])["mean"],3)
  tmp_data_funct=df_funct[df_funct[[treatment_var_funct1]]==1 & df_funct[[treatment_var_funct2]]==1,]
  StatDes_funct[4,2]=round(stat.desc(tmp_data_funct[[var_funct]])["SE.mean"],3)
  
  StatDes_funct
}

#Function to compare shares of zeros
matTreatmentDiff_ShareZeros=function(df_funct,listVar_funct,treatment_var_funct){
  Mat_funct=matrix(data=NA,nrow=length(listVar_funct),ncol=4)
  colnames(Mat_funct)=c("Control","Treated","Difference","pvalue")
  rownames(Mat_funct)=listVar_funct
  df0=df_funct[df_funct[[treatment_var_funct]]==0,]
  df1=df_funct[df_funct[[treatment_var_funct]]==1,]
  k_funct=1
  for(j_funct in listVar_funct){
    Mat_funct[k_funct,1]=round(stat.desc(df0[[j_funct]])["nbr.null"]/stat.desc(df0[[j_funct]])["nbr.val"],3)
    Mat_funct[k_funct,2]=round(stat.desc(df1[[j_funct]])["nbr.null"]/stat.desc(df1[[j_funct]])["nbr.val"],3)
    Mat_funct[k_funct,3]=Mat_funct[k_funct,1]-Mat_funct[k_funct,2]
    Mat_funct[k_funct,4]=round(prop.test(x = c(stat.desc(df0[[j_funct]])["nbr.val"]-stat.desc(df0[[j_funct]])["nbr.null"],stat.desc(df1[[j_funct]])["nbr.val"]-stat.desc(df1[[j_funct]])["nbr.null"]), n = c(stat.desc(df0[[j_funct]])["nbr.val"], stat.desc(df1[[j_funct]])["nbr.val"]))$p.value,4)
    k_funct=k_funct+1
  }
  Mat_funct
}

#Confidence interval
ci95=function(var_funct){
  return(list(lb=stat.desc(var_funct)["mean"]-stat.desc(var_funct)["CI.mean.0.95"], ub=stat.desc(var_funct)["mean"]+stat.desc(var_funct)["CI.mean.0.95"]))
}