""
#author: Evgeny Galimov
""

library(collections)
library(dplyr)
library(tibble)

# load data with Long-term conditions (LTCs)
data = read.csv('S://NDL/MouleshWorkings/Extracts2/LTC_2.csv')
# load data for our carers + matched cohort
g_data = read.csv('S://NDL/MouleshWorkings/221223_carers_and_matched_final_table.csv')
# join LTCs data only for our carers + matched cohort
g_data2 = merge(g_data, data, by='PatientKey', all.x=T)


# create dictionary with carers start dates
first_time_carer = read.csv('S://NDL/MouleshWorkings/230110_first_time_carer.csv')
# create a dictionary from this table
first_date_dict = dict(keys = first_time_carer$PatientsKey, items = first_time_carer$start_carer_date)

# getting MatchedPatientKey column
matched = read.csv('S://NDL/MouleshWorkings/221223_carers_and_matched_final_table.csv')
matched2 = matched[,c('PatientKey','MatchedPatientKey')]
data = data %>% left_join( matched2, by = 'PatientKey')



########################################
# set up group number  as the patient key for the carer in the group 'carer = 1-5 matched contrils'
data$group = as.numeric(  ifelse(data$MatchedPatientKey != 'carer',data$MatchedPatientKey, data$PatientKey )  )

# adding first_carer_dates
data = add_column(data, first_carer_date = 0, .after =  'group' )

# function to get the date of becoming carer using patient key of the carer with the help of first_date_dict defined above
get_first_date_based_on_group = function(group){
  return( first_date_dict$get(as.integer( group ) ) )
}

data$first_carer_date = mapply(get_first_date_based_on_group, data$group)
data$first_carer_date = as.Date(data$first_carer_date, format = '%Y-%m-%d')

# save a temporary file where first date of being a carer is assigned to a carer and his <5 contols
write.csv(data, 'S://NDL/MouleshWorkings/Extracts2/0_final_table_LTCs_first_date.csv')

data_clean = read.csv('S://NDL/MouleshWorkings/Extracts2/0_final_table_LTCs_first_date.csv')

# getting flags if patients got disease after becoming carer
# list of LTCs
diseases = c("Date.of.Diagnosis.Asthma", "Date.of.Diagnosis.AtrialFibrillation","Date.of.Diagnosis.Cancer",
      "Date.of.Diagnosis.CKD","Date.of.Diagnosis.COPD","Date.of.Diagnosis.Dementia",
      "Date.of.Diagnosis.Depression", "Date.of.Diagnosis.Diabetes","Date.of.Diagnosis.Epilepsy",
      "Date.of.Diagnosis.HeartFailure", "Date.of.Diagnosis.Hypertension",
      "Date.of.Diagnosis.LearningDisability", "Date.of.Diagnosis.MentalHealth", 
      "Date.of.Diagnosis.RheumatoidArthritis", "Date.of.Diagnosis.StrokeTIA",
      "Date.of.Diagnosis.IschaemicHeartDisease", "Date.of.Diagnosis.Anxiety",
      "Date.of.Diagnosis.ParkinsonsDisease", "Date.of.Diagnosis.MultipleSclerosis")

# select columns
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
  return(  ifelse(temp<=0,1,0)  )
}



# create columns with flags for cases when a disease happened after becoming a carer
for (i in 1:length(diseases)){ 
#   i = 4
  data_clean_2[,c(diseases[i])] = as.Date( data_clean_2[ ,c(diseases[i]) ] ,format = '%d/%m/%Y')
  
  data_clean_2 = add_column(data_clean_2, !!( paste(diseases[i],'_flag',sep='') ) := 0, .after = diseases[i] )
  
  
  data_clean_2[ ,c(paste(diseases[i],'_flag',sep='') )] = mapply(get_flag_disease_carer, data_clean_2$first_carer_date, data_clean_2[ ,c(diseases[i]) ]  )
    
}


write.csv(data_clean_2, 'S://NDL/MouleshWorkings/Extracts2/0_final_table_LTCs_first_date_adjusted2.csv')













