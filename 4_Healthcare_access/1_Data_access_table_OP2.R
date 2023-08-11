
library(dplyr)
library(tidyverse)
library(tibble)
library(collections)

#setwd("S://NDL/3_Soc_care/1_carer_extraction/SP/Latest extraction/2022_06_29 Cohort Extraction")


# create dictionary with carers start dates
first_time_carer = read.csv('S://NDL/MouleshWorkings/230110_first_time_carer.csv')
# create a dictionary from this table
first_date_dict = dict(keys = first_time_carer$PatientsKey, items = first_time_carer$start_carer_date)

# load the data
data = read.csv('S://NDL/MouleshWorkings/Extracts2/Outpatients.txt')
#summary(data$Pre_or_Post)
colnames(data)[1] = "PatientKey"

# add info about carer/control status
matched = read.csv('S://NDL/MouleshWorkings/221223_carers_and_matched_final_table.csv')
matched2 = matched[,c('PatientKey','MatchedPatientKey')]
data = data %>% left_join( matched2, by = 'PatientKey')



#data = merged
# categorizing carer/non carer
carer_fun = function(MatchPatientKey){
  out = 'carer'
  if ( MatchPatientKey != 'carer') {out = 'control'}
  return(out)
}
data$carer = mapply(carer_fun, data$MatchedPatientKey )


colnames(data)[1] = "Patient_DerivedKey"

# get groups carer + controls
data = data[order(data$Patient_DerivedKey),]
#data$group = as.numeric(  ifelse(data$MatchPatientKey != 'carer',data$MatchPatientKey, data$Patient_DerivedKey )  )
data = add_column(data,group = as.numeric(  ifelse(data$MatchedPatientKey != 'carer',data$MatchedPatientKey, data$Patient_DerivedKey )  ), .after = 'Patient_DerivedKey' )
data = data[order(data$group),]


# get first dates 
get_first_carer_date = function(group){
  return( first_date_dict$get(as.integer(group)) )
}
data$first_carer_date = mapply(get_first_carer_date, data$group)
data$first_carer_date = as.Date(data$first_carer_date, format = '%Y-%m-%d')



# reformating date
data = data %>% mutate(AppointmentDate = as.Date(AppointmentDate, format = '%Y-%m-%d'))

a3 = data[,c('Patient_DerivedKey','group','ArrivalDate', 'first_carer_date')]

###############################
# getting flag to see only values for patients after becoming a carer
data$after_carer = ifelse(data$AppointmentDate > data$first_carer_date,1,0)

# removing carers who have PAM value before becoming carers
data_a = data[!( (data$after_carer==0) ),]
data_a = data_a[order(data_a$group, data_a$Patient_DerivedKey),]


data_a2 = data_a


# only include groups which contain carer + controls
data_a2$change_flag = 0
for (i in 2:nrow(data_a2)) {
  cur_g = data_a2$group[i]
  prev_g = data_a2$group[i-1]
  
  cur_car = data_a2$carer[i]
  prev_car = data_a2$carer[i-1]
  
  if ( (cur_g == prev_g) & (cur_car != prev_car) ) {data_a2$change_flag[i] = 1}
  
}
data_a2 = data_a2[order(data_a2$group, -data_a2$change_flag),]
z=0
for (i in 2:nrow(data_a2)) {
 # z=z+1
  cur_g = data_a2$group[i]
  prev_g = data_a2$group[i-1]
  
  if ( (cur_g == prev_g)  ) {data_a2$change_flag[i] = data_a2$change_flag[i-1]}
  
}

data_a2 = data_a2[!data_a2$change_flag==0,]


write.csv(data_a2, 'S://NDL/MouleshWorkings/3_HC_access/OP/OP_adj.csv')


#write.csv(data_a2, 'C://Users/galimove/Documents/221101 AnE v2_adj.csv')
data_a2 = read.csv('S://NDL/MouleshWorkings/3_HC_access/OP/OP_adj.csv')

a2 = data_a2[1:10000,c('group','Patient_DerivedKey','carer','after_carer', 'change_flag')]

###############################

#split 
  # age
  # gender
  # IMDDecile
  # Ethnicity
  # Carer
  # Borough

    # get counts 

# back up data
data_backup = data



data = data_a2
# Create invervals since beginning months, quoters, years
data$m = interval( as.Date("2015-06-01"), data$ArrivalDate ) %/% months(1)
data$q = interval( as.Date("2015-06-01"), data$ArrivalDate ) %/% months(3)
data$y = interval( as.Date("2015-06-01"), data$ArrivalDate ) %/% months(12)

# choose only those 18 and older
data2 = data[data$age > 17,]
# split by age
summary(data$age)
hist(data2$age)



# create age categories
age_cat = function(age){
  out = 'None'
  if ( (age > 17) & (age < 41)) {out = '18-40'}
  if ( (age > 40) & (age < 56)) {out = '41-55'}
  if ( (age > 55) & (age < 71)) {out = '56-70'}
  if ( (age > 70) & (age < 105)) {out = '71-105'}
  return(out)
}

data2$age_cat = mapply(age_cat, data2$age)
summary( as.factor(data2$age_cat) )

data2 = data2[!data2$age_cat == 'None',]

summary(data2$age)
# remove missing data for IMDDecile
data2 = data2[data2$IMDDecile != -2,]
write.csv(data2, 'S://NDL/MouleshWorkings/3_HC_access/OP/OP_adj_fin_agecat.csv')
#write.csv(data_a2, 'C://Users/galimove/Documents/221101 AnE v2_adj.csv')
data = read.csv('S://NDL/MouleshWorkings/3_HC_access/OP/OP_adj_fin_agecat.csv')


# if age categories were not set
write.csv(data2, 'S://NDL/MouleshWorkings/3_HC_access/OP/OP_adj_fin_ageCont.csv')
#write.csv(data_a2, 'C://Users/galimove/Documents/221101 AnE v2_adj.csv')
data_a2 = read.csv('S://NDL/MouleshWorkings/3_HC_access/AnE/AnE_adj_fin_ageCont.csv')



#data2=data

data2 = data_a2
# deduplicate the data
data3 = data2[!duplicated(data2$Patient_DerivedKey),]
#data3 = data2[!duplicated(data2$PatientKey),]




################
### getting counts of visits for groups split by age_cat, Gender, carer, IMDDecile
d_t4 = data2 %>%
  group_by(age_cat, Gender, carer, IMDDecile) %>%
  summarize(count_by_age_catGendercarerIMDDecile = n())

d_t4_Pat = data3 %>%
  group_by(age_cat, Gender, carer, IMDDecile) %>%
  summarize(count_by_age_catGendercarerIMDDecile = n())

colnames(d_t4_Pat)[5] = 'pop'

d_t4_fin = cbind(d_t4, d_t4_Pat$pop)
colnames(d_t4_fin)[6] = 'pop'
colnames(d_t4_fin)[5] = 'visit_counts'

logpop2 = log(d_t4_fin[,6])
d_t4_fin = cbind(d_t4_fin, logpop2)
colnames(d_t4_fin)[6] = 'pop'
colnames(d_t4_fin)[7] = 'logpop2'


write.csv(d_t4_fin ,'S://NDL/MouleshWorkings/3_HC_access/OP/groups_count.csv')

d_t5 = data %>%
  group_by(age_cat, Gender, carer, IMDDecile) %>%
  summarize(visit_counts = n())
d_t5_pop = data %>%
  group_by(age_cat, Gender, carer, IMDDecile) %>%
  summarize(pop = n_distinct(Patient_DerivedKey))
d_t5$pop = d_t5_pop$pop

d_t5$logpop2 = log(d_t5$pop)

write.csv(d_t5 ,'S://NDL/MouleshWorkings/3_HC_access/OP/groups_count2.csv')


#################################
# getting counts of visits for groups split by age_cat, Gender, carer, IMDDecile     | age as cont variable
###########
#########
d_t4 = data2 %>%
  group_by(age, Gender, carer, IMDDecile) %>%
  summarize(count_by_ageGendercarerIMDDecile = n())

d_t4_Pat = data3 %>%
  group_by(age, Gender, carer, IMDDecile) %>%
  summarize(count_by_ageGendercarerIMDDecile = n())

colnames(d_t4_Pat)[5] = 'pop'

d_t4_fin = cbind(d_t4, d_t4_Pat$pop)
colnames(d_t4_fin)[6] = 'pop'
colnames(d_t4_fin)[5] = 'visit_counts'

logpop2 = log(d_t4_fin[,6])
d_t4_fin = cbind(d_t4_fin, logpop2)
colnames(d_t4_fin)[6] = 'pop'
colnames(d_t4_fin)[7] = 'logpop2'


write.csv(d_t4_fin ,'S://NDL/MouleshWorkings/3_HC_access/OP/groups_count_ageCont.csv')
