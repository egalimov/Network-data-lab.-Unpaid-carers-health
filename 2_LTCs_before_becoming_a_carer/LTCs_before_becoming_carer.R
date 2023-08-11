""
#author: Evgeny Galimov
""

library(dplyr)
library(stringr)
library(tibble)

#data = read.csv('S://NDL/MouleshWorkings/Extracts2/0_final_table_LTCs_first_date_adjusted.csv')

# Load the table with LTCs and flags if they appeared after a carer was identified
data_clean = read.csv('S://NDL/MouleshWorkings/Extracts2/0_final_table_LTCs_first_date.csv')

diseases = c("Date.of.Diagnosis.Asthma", "Date.of.Diagnosis.AtrialFibrillation","Date.of.Diagnosis.Cancer",
             "Date.of.Diagnosis.CKD","Date.of.Diagnosis.COPD","Date.of.Diagnosis.Dementia",
             "Date.of.Diagnosis.Depression", "Date.of.Diagnosis.Diabetes","Date.of.Diagnosis.Epilepsy",
             "Date.of.Diagnosis.HeartFailure", "Date.of.Diagnosis.Hypertension",
             "Date.of.Diagnosis.LearningDisability",  
             "Date.of.Diagnosis.RheumatoidArthritis", "Date.of.Diagnosis.StrokeTIA",
             "Date.of.Diagnosis.IschaemicHeartDisease", "Date.of.Diagnosis.Anxiety",
             "Date.of.Diagnosis.ParkinsonsDisease", "Date.of.Diagnosis.MultipleSclerosis")

# get needed columns
data_clean_2 = data_clean[,c("PatientKey","group","first_carer_date","age",
                             "Gender","IMDRank","IMDDecile",                   
                             "EthnicCategory","MatchedPatientKey",
                             
                             "Date.of.Diagnosis.Asthma", "Date.of.Diagnosis.AtrialFibrillation","Date.of.Diagnosis.Cancer",
                             "Date.of.Diagnosis.CKD","Date.of.Diagnosis.COPD","Date.of.Diagnosis.Dementia",
                             "Date.of.Diagnosis.Depression", "Date.of.Diagnosis.Diabetes","Date.of.Diagnosis.Epilepsy",
                             "Date.of.Diagnosis.HeartFailure", "Date.of.Diagnosis.Hypertension",
                             "Date.of.Diagnosis.LearningDisability", "Date.of.Diagnosis.MentalHealth", 
                             "Date.of.Diagnosis.RheumatoidArthritis", "Date.of.Diagnosis.StrokeTIA",
                             "Date.of.Diagnosis.IschaemicHeartDisease", "Date.of.Diagnosis.Anxiety",
                             "Date.of.Diagnosis.ParkinsonsDisease", "Date.of.Diagnosis.MultipleSclerosis"
)]      
# function to get flags for carers who got disease after becoming a carer
get_flag_disease_carer = function(first_carer_date, diease_date){
  temp = difftime( first_carer_date,
                   diease_date,
                   units = 'days')
  return(  ifelse(temp<=0,0,1)  )
}



# iterate over diseases and set up columns with flags
for (i in 1:length(diseases)){ 
  #   i = 4
  # data_clean_2 = merge(data_clean_2, data_clean[, c('PatientKey',diseases[i]) ], by = 'PatientKey')
  data_clean_2[,c(diseases[i])] = as.Date( data_clean_2[ ,c(diseases[i]) ] ,format = '%d/%m/%Y')
  
  data_clean_2 = add_column(data_clean_2, !!( paste(diseases[i],'_flag',sep='') ) := 0, .after = diseases[i] )
  
  
  data_clean_2[ ,c(paste(diseases[i],'_flag',sep='') )] = mapply(get_flag_disease_carer, data_clean_2$first_carer_date, data_clean_2[ ,c(diseases[i]) ]  )
  
}

# create a column for carer status
data_clean_2$carer =   ifelse(data_clean_2$MatchedPatientKey == 'carer',data_clean_2$MatchedPatientKey, 'control' )  

write.csv(data_clean_2, 'S://NDL/MouleshWorkings/Extracts2/0_final_table_LTCs_flags_before_carer.csv')

data_clean_2 = read.csv('S://NDL/MouleshWorkings/Extracts2/0_final_table_LTCs_flags_before_carer.csv')


data_clean_2$carer = as.factor(data_clean_2$carer)
data_clean_2$carer = relevel(data_clean_2$carer, ref = 'control')

 
## get summaries for diseases which before carers were identified
ltc_before = setNames( data.frame(matrix(ncol = 5, nrow = 0)), c('LTC','Control_n','LTC_control_n','Carer_n', 'LTC_carer_n')   )
control_n = nrow(data_clean_2[data_clean_2$carer == 'control',])
carer_n = nrow(data_clean_2[data_clean_2$carer == 'carer',])

# iterate over diseases adding % of number of patients with a LTC for controls and carers
for (i in 1:length(diseases)){ 
  aa = data_clean_2[!is.na(data_clean_2[,c(diseases[i])]),]
  #ltc_before[nrow(ltc_before) + 1,] = list('1','2','4','5','6')
  
  ltc_before[nrow(ltc_before) + 1,] = c( c(str_sub(diseases[i],19, nchar(diseases[i]) ) ), control_n, 
                                 nrow(aa[(  (aa[ paste(diseases[i],'_flag',sep='') ] == 1) & (aa$carer=='control')),]) ,
                                 carer_n,
                                 nrow(aa[(  (aa[ paste(diseases[i],'_flag',sep='') ] == 1) & (aa$carer=='carer')),])
                                 )
}

# getting %
ltc_before$Control_n = as.numeric(ltc_before$Control_n)
ltc_before$LTC_control_n = as.numeric(ltc_before$LTC_control_n)
ltc_before$Carer_n = as.numeric(ltc_before$Carer_n)
ltc_before$LTC_carer_n = as.numeric(ltc_before$LTC_carer_n)

ltc_before$LTCprop_control = round(ltc_before$LTC_control_n/ltc_before$Control_n*100, 2)
ltc_before$LTCprop_carer = round(ltc_before$LTC_carer_n/ltc_before$Carer_n*100, 2)

write.csv(ltc_before, 'S://NDL/MouleshWorkings/LTCs_before/0_LTCs_before_carer.csv')


### create a table with % of 0,1,2,3+ LTCs for controls and carers
data = data_clean_2 %>% rename('Asthma'='Date.of.Diagnosis.Asthma_flag',             
                       'AtrialFibrillation'='Date.of.Diagnosis.AtrialFibrillation_flag', 
                       'Cancer'='Date.of.Diagnosis.Cancer_flag',
                       'CKD'='Date.of.Diagnosis.CKD_flag',           
                       'COPD'='Date.of.Diagnosis.COPD_flag',                
                       'Dementia'='Date.of.Diagnosis.Dementia_flag',             
                       'Depression'='Date.of.Diagnosis.Depression_flag',           
                       'Diabetes' = 'Date.of.Diagnosis.Diabetes_flag',             
                       'Epilepsy' = 'Date.of.Diagnosis.Epilepsy_flag',             
                       'HeartFailure'='Date.of.Diagnosis.HeartFailure_flag',         
                       'Hypertension'='Date.of.Diagnosis.Hypertension_flag',         
                       'LearningDisability'='Date.of.Diagnosis.LearningDisability_flag',   
                       'ReumatoidArthritis'='Date.of.Diagnosis.RheumatoidArthritis_flag',  
                       'Stroke' = 'Date.of.Diagnosis.StrokeTIA_flag',          
                       'IschaemicHeartDisease' = 'Date.of.Diagnosis.IschaemicHeartDisease_flag',
                       'Anxiety' = 'Date.of.Diagnosis.Anxiety_flag',
                       'ParkinsonDisease' = 'Date.of.Diagnosis.ParkinsonsDisease_flag',    
                       'MultipleSclerosis' = 'Date.of.Diagnosis.MultipleSclerosis_flag' )

data2 = data[,c('carer','Asthma','AtrialFibrillation','Cancer','CKD','COPD','Dementia','Depression','Diabetes',
                'Epilepsy','HeartFailure','Hypertension','LearningDisability','ReumatoidArthritis',
                'Stroke','IschaemicHeartDisease','Anxiety','ParkinsonDisease','MultipleSclerosis')]

data2[is.na(data2)] <- 0

data2$n_LTCs = rowSums(data2[,c('Asthma','AtrialFibrillation','Cancer','CKD','COPD','Dementia','Depression','Diabetes',
                                'Epilepsy','HeartFailure','Hypertension','LearningDisability','ReumatoidArthritis',
                                'Stroke','IschaemicHeartDisease','Anxiety','ParkinsonDisease','MultipleSclerosis')])


# make df with % of 0,1,2,3+ among carer and controls
ltc_before_dist = setNames( data.frame(matrix(ncol = 5, nrow = 0)), c('Cohort','LTC_0','LTC_1','LTC_2', 'LTC_3plus')   )
ltc_before_dist[nrow(ltc_before_dist) + 1,] = c( 'Control', 
                                                 nrow(data2[((data2$carer=='control')&(data2$n_LTCs==0)),])/control_n*100, 
                                                 nrow(data2[((data2$carer=='control')&(data2$n_LTCs==1)),])/control_n*100,
                                                 nrow(data2[((data2$carer=='control')&(data2$n_LTCs==2)),])/control_n*100,
                                                 nrow(data2[((data2$carer=='control')&(data2$n_LTCs>=3)),])/control_n*100    )
                                               
ltc_before_dist[nrow(ltc_before_dist) + 1,] = c( 'carer', 
                                                 nrow(data2[((data2$carer=='carer')&(data2$n_LTCs==0)),])/carer_n*100, 
                                                 nrow(data2[((data2$carer=='carer')&(data2$n_LTCs==1)),])/carer_n*100,
                                                 nrow(data2[((data2$carer=='carer')&(data2$n_LTCs==2)),])/carer_n*100,
                                                 nrow(data2[((data2$carer=='carer')&(data2$n_LTCs>=3)),])/carer_n*100    )

ltc_before_dist$LTC_0 = round(as.numeric(ltc_before_dist$LTC_0), 2)
ltc_before_dist$LTC_1 = round(as.numeric(ltc_before_dist$LTC_1), 2)
ltc_before_dist$LTC_2 = round(as.numeric(ltc_before_dist$LTC_2), 2)
ltc_before_dist$LTC_3plus = round(as.numeric(ltc_before_dist$LTC_3plus), 2)


write.csv(ltc_before_dist, 'S://NDL/MouleshWorkings/LTCs_before/0_LTCs_before_carer_dist.csv')


