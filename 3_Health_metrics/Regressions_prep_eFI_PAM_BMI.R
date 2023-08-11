""
#author: Evgeny Galimov
""

library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(collections)


#######################################################################################################################
# Preprocessing for eFI, PAM score and BMI

#######################################################################################################################
# eFI
#####
# load table with dates of becoming a carer
first_time_carer = read.csv('S://NDL/MouleshWorkings/230110_first_time_carer.csv')
# create a dictionary from this table
first_date_dict = dict(keys = first_time_carer$PatientsKey, items = first_time_carer$start_carer_date)


############
# data preprocessing for eFI dataset
#####

# loading efi data
efi = read.csv('S://NDL/MouleshWorkings/Extracts2/EFI.txt')

# loading table with carer+matched controls
matched = read.csv('S://NDL/MouleshWorkings/221223_carers_and_matched_final_table.csv')
# joining the the info about carer/controls to efi table
matched2 = matched[,c('PatientKey','MatchedPatientKey')]
efi = efi %>% left_join( matched2, by = 'PatientKey')


# add group - the number of each carer assigned to the carer and his/her controls
efi = efi[order(efi$PatientKey),]
efi = add_column(efi,group = as.numeric(  ifelse(efi$MatchedPatientKey != 'carer',efi$MatchedPatientKey, efi$PatientKey )  ), .after = 'PatientKey' )
efi = efi[order(efi$group),]
# only include groups which contain carer + controls 
efi2 = efi
for (i in efi[!duplicated(efi$group),]$group) {
  t = efi[efi$group == i,]
  if (  (nrow( t[t$MatchedPatientKey != 'carer',] )==0) | (nrow( t[t$MatchedPatientKey == 'carer',] )==0) ){
    efi = efi[!efi$group ==i,]
  }
}


write.csv(efi, 'S://NDL/MouleshWorkings/230110_EFI_adjusted.csv')

efi = read.csv('S://NDL/MouleshWorkings/230110_EFI_adjusted.csv')





#######################################################################################################################
# PAM
#####
# loading PAM data
pam = read.csv('S://NDL/MouleshWorkings/Extracts2/PAM.txt')
# loading table with carer+matched controls
matched = read.csv('S://NDL/MouleshWorkings/221223_carers_and_matched_final_table.csv')
# joining the the info about carer/controls to PAM table
matched2 = matched[,c('PatientKey','MatchedPatientKey')]
pam = pam %>% left_join( matched2, by = 'PatientKey')

# filtering values of Patient Activation Measure score 
pam = pam[pam$SnomedTerm =='Patient Activation Measure score',]

# rename column
colnames(pam)[colnames(pam) == 'NumResult'] = 'PAM_Value'
# remove NA
pam = pam[!is.na(pam$PAM_Value),]



# get scores only
pam_score = pam[pam$SnomedTerm=='Patient Activation Measure score',]

# get groups - the number of each carer assigned to the carer and his/her controls
pam_score = pam_score[order(pam_score$PatientKey),]
pam_score$group = as.numeric(  ifelse(pam_score$MatchedPatientKey != 'carer',pam_score$MatchedPatientKey, pam_score$PatientKey )  )
pam_score = add_column(pam_score,group = as.numeric(  ifelse(pam_score$MatchedPatientKey != 'carer',pam_score$MatchedPatientKey, pam_score$PatientKey )  ), .after = 'PatientKey' )
pam_score = pam_score[order(pam_score$group),]


# keep only groups which contain  carers
valid_groups = pam_score[pam_score$PatientKey == pam_score$group, ]
pam_score_valid_groups = pam_score[ pam_score$group %in% valid_groups[!duplicated(valid_groups$group),]$group, ]


# the function to get first dates 
get_first_carer_date = function(group){
  return( first_date_dict$get(group) )
}


# get the date of becoming a carer for the PAM table
pam_score_valid_groups$first_carer_date = mapply(get_first_carer_date, pam_score_valid_groups$group)
pam_score_valid_groups$first_carer_date = as.Date(pam_score_valid_groups$first_carer_date, format = '%Y-%m-%d')

# remove duplicates
pam_score_valid_groups_u = pam_score_valid_groups[!duplicated(pam_score_valid_groups),]

# only include groups which contain controls
pam_score_valid_groups_u_cntr = pam_score_valid_groups_u
for (i in pam_score_valid_groups_u[!duplicated(pam_score_valid_groups_u$group),]$group) {
  #i=2577
  t = pam_score_valid_groups_u[pam_score_valid_groups_u$group == i,]
  if (nrow( t[t$MatchedPatientKey != 'carer',] )==0 ){
    pam_score_valid_groups_u_cntr = pam_score_valid_groups_u_cntr[!pam_score_valid_groups_u_cntr$group ==i,]
  }
}

# removing NULLs for PAM values
pam_score_valid_groups_u_cntr = pam_score_valid_groups_u_cntr[pam_score_valid_groups_u_cntr$PAM_Value!='NULL',]

# converting Date
pam_score_valid_groups_u_cntr$EventDate = as.Date(pam_score_valid_groups_u_cntr$EventDate, format = '%Y-%m-%d')
# getting flag to see only values for patients after becoming a carer
pam_score_valid_groups_u_cntr$after_carer = ifelse(pam_score_valid_groups_u_cntr$EventDate > pam_score_valid_groups_u_cntr$first_carer_date,1,0)

# removing carers who have PAM value before becoming carers
final_table = pam_score_valid_groups_u_cntr[!( (pam_score_valid_groups_u_cntr$after_carer==0) & (pam_score_valid_groups_u_cntr$MatchedPatientKey=='carer') ),]

# removing all who have PAM value before becoming carers date
final_table = pam_score_valid_groups_u_cntr[!(pam_score_valid_groups_u_cntr$after_carer==0),]

# only include groups which contain carer + controls 
final_table2 = final_table
for (i in final_table2[!duplicated(final_table2$group),]$group) {
  #i=2577
  t = final_table2[final_table2$group == i,]
  if (  (nrow( t[t$MatchedPatientKey != 'carer',] )==0) | (nrow( t[t$MatchedPatientKey == 'carer',] )==0) ){
    final_table2 = final_table2[!final_table2$group ==i,]
  }
}


# just to explore more conveniently
final_table3 = final_table2
# get latest PAM value for each patient - Sort the EventDate descending
# order Event date reverse
final_table3 = final_table3[order(final_table3$group, final_table3$PatientKey, final_table3$EventDate, decreasing=c(F,F,T) ),]

# get the first line to get the latest PAM score 
final_table4 = final_table3[!duplicated(final_table3$PatientKey),]


# Saving the results
write.csv(final_table4, 'S://NDL/MouleshWorkings/Extracts2/PAM_carer_start_adjusted.csv')


######################################################################################################################
# BMI
#####
# loading BMI data
bmi = read.csv('S://NDL/MouleshWorkings/Extracts2/BMI.txt')
# get groups carer + controls
bmi = bmi[order(bmi$PatientKey),]

matched = read.csv('S://NDL/MouleshWorkings/221223_carers_and_matched_final_table.csv')
matched2 = matched[,c('PatientKey','MatchedPatientKey')]
bmi = bmi %>% left_join( matched2, by = 'PatientKey')

# add group - the number of each carer assigned to the carer and his/her controls
bmi = add_column(bmi,group = as.numeric(  ifelse(bmi$MatchedPatientKey != 'carer',bmi$MatchedPatientKey, bmi$PatientKey )  ), .after = 'PatientKey' )
bmi = bmi[order(bmi$group),]



# keep only groups with carers
valid_groups = bmi[bmi$PatientKey == bmi$group, ]
bmi_score_valid_groups = bmi[ bmi$group %in% valid_groups[!duplicated(valid_groups$group),]$group, ]


# get first dates 
get_first_carer_date = function(group){
  return( first_date_dict$get(as.integer(group)) )
}
first_date_dict$get(as.integer(2))
bmi_score_valid_groups$first_carer_date = mapply(get_first_carer_date, bmi_score_valid_groups$group)
bmi_score_valid_groups$first_carer_date = as.Date(bmi_score_valid_groups$first_carer_date, format = '%Y-%m-%d')

bmi_score_valid_groups$EventDate = as.Date(bmi_score_valid_groups$EventDate, format = '%Y-%m-%d')

# remove duplicates
bmi_score_valid_groups_u = bmi_score_valid_groups[!duplicated(bmi_score_valid_groups),]

# rename the column for BMI value
names(bmi_score_valid_groups_u)[names(bmi_score_valid_groups_u) == 'NumResult'] = 'BMI_Value' 

# removing NULLs for BMI values
bmi_score_valid_groups_u = bmi_score_valid_groups_u[bmi_score_valid_groups_u$BMI_Value!='NULL',]

# converting Date
bmi_score_valid_groups_u$EventDate = as.Date(bmi_score_valid_groups_u$EventDate, format = '%Y-%m-%d')

# getting flag to see only values for patients after becoming a carer
bmi_score_valid_groups_u$after_carer = ifelse(bmi_score_valid_groups_u$EventDate > bmi_score_valid_groups_u$first_carer_date,1,0)

# removing all who have BMI value before becoming carers date
bmi_final_table = bmi_score_valid_groups_u[! (bmi_score_valid_groups_u$after_carer==0) ,]

# only include groups which contain carer + controls 
bmi_final_table2 = bmi_final_table
for (i in bmi_final_table2[!duplicated(bmi_final_table2$group),]$group) {
  #i=2577
  t = bmi_final_table2[bmi_final_table2$group == i,]
  if (  (nrow( t[t$MatchedPatientKey != 'carer',] )==0) | (nrow( t[t$MatchedPatientKey == 'carer',] )==0) ){
    bmi_final_table2 = bmi_final_table2[!bmi_final_table2$group ==i,]
  }
}

bmi_final_table3 = bmi_final_table2
# get latest BMI value for each patient - Sort the EventDate descending
# order Event date reverse
bmi_final_table3 = bmi_final_table3[order(bmi_final_table3$group, bmi_final_table3$PatientKey, bmi_final_table3$EventDate, decreasing=c(F,F,T) ),]

# get the first line for - getting latest value of BMI
bmi_final_table4 = bmi_final_table3[!duplicated(bmi_final_table3$PatientKey),]

bmi_final_table4$BMI_Value = as.numeric( bmi_final_table4$BMI_Value)

# removing missing values
bmi_final_table4 = bmi_final_table4[!is.na(bmi_final_table4$BMI_Value),]
# plotting distribution of BMI
hist(bmi_final_table4$BMI_Value, bins =10)

# removing extreme values
bmi_final_table5 = bmi_final_table4[(bmi_final_table4$BMI_Value>=12) & (bmi_final_table4$BMI_Value<=50),]

# save preprocessed data
write.csv(bmi_final_table5, 'S://NDL/MouleshWorkings/Extracts2/BMI_carer_start_adjusted.csv')










