likertToNumeric=function(var_funct){
  tmp_var=var_funct
  tmp_var=ifelse(tmp_var=="1. Pas du tout d'accord",1,tmp_var)
  tmp_var=ifelse(tmp_var=="7. Tout à fait d'accord",7,tmp_var)
  as.numeric(tmp_var)
}

likertToNumericEmotions=function(var_funct){
  tmp_var=var_funct
  tmp_var=ifelse(tmp_var=="1. Non, pas du tout",1,tmp_var)
  tmp_var=ifelse(tmp_var=="7. Oui, tout à fait",7,tmp_var)
  as.numeric(tmp_var)
}

foodFreqIntoNumeric=function(var_funct){
  tmp_var=var_funct
  tmp_var=ifelse(tmp_var=="Jamais",1,tmp_var)
  tmp_var=ifelse(tmp_var=="Quelques fois par an",2,tmp_var)
  tmp_var=ifelse(tmp_var=="Quelques fois par mois",3,tmp_var)
  tmp_var=ifelse(tmp_var=="Quelques fois par semaine",4,tmp_var)
  tmp_var=ifelse(tmp_var=="Presque à chaque repas",5,tmp_var)
  as.numeric(tmp_var)
}

likertToNumericSharing=function(var_funct){
  tmp_var=var_funct
  tmp_var=ifelse(tmp_var=="1. Pas du tout probable",1,tmp_var)
  tmp_var=ifelse(tmp_var=="7. Très probable",7,tmp_var)
  as.numeric(tmp_var)
}

likertToNumericTruth=function(var_funct){
  tmp_var=var_funct
  tmp_var=ifelse(tmp_var=="Pas du tout d'accord",1,tmp_var)
  tmp_var=ifelse(tmp_var=="Pas d'accord",2,tmp_var)
  tmp_var=ifelse(tmp_var=="Plutôt pas d'accord",3,tmp_var)
  tmp_var=ifelse(tmp_var=="Ni d'accord, ni pas d'accord",4,tmp_var)
  tmp_var=ifelse(tmp_var=="Plutôt d'accord",5,tmp_var)
  tmp_var=ifelse(tmp_var=="D'accord",6,tmp_var)
  tmp_var=ifelse(tmp_var=="Tout à fait d'accord",7,tmp_var)
  as.numeric(tmp_var)
}