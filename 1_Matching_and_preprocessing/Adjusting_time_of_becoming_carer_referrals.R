""
#author: Evgeny Galimov
""

library(dplyr)
library(collections)
library(tibble)



# load data with referrals by specialty 
data =read.csv( 'S://NDL/MouleshWorkings/Extracts2/Referral.txt' ) 
# rename the column
colnames(data)[1] = 'PatientKey'

# create a dictionary to get date of becoming a carer by patient key
first_time_carer = read.csv('S://NDL/MouleshWorkings/230110_first_time_carer.csv')
first_date_dict = dict(keys = first_time_carer$PatientsKey, items = first_time_carer$start_carer_date)

# getting MatchedPatientKey column
matched = read.csv('S://NDL/MouleshWorkings/221223_carers_and_matched_final_table.csv')
matched2 = matched[,c('PatientKey','MatchedPatientKey')]
data = data %>% left_join( matched2, by = 'PatientKey')

# set up group number as patient key of the carer
data$group = as.numeric(  ifelse(data$MatchedPatientKey != 'carer',data$MatchedPatientKey, data$PatientKey )  )

# get date based on patient key
get_first_date_based_on_group = function(group){
  return( first_date_dict$get(as.integer( group ) ) )
}

#get_first_date_based_on_group
data$first_carer_date = mapply(get_first_date_based_on_group, data$group)
data$first_carer_date = as.Date(data$first_carer_date, format = '%Y-%m-%d')

# convert appointmenmt date 
data$AppointmentDate = as.Date(data$AppointmentDate, format = '%Y-%m-%d')

# get flag after carer
data$after_carer = ifelse(data$AppointmentDate>=data$first_carer_date,1,0)

write.csv(data, 'S://NDL/MouleshWorkings/Extracts2/Referral_adjusted.csv')




















