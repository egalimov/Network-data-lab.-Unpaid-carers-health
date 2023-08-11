
library(dplyr)
library(tidyverse)
library(tibble)
library(collections)

# create dictionary with carers start dates
first_time_carer = read.csv('S://NDL/MouleshWorkings/230110_first_time_carer.csv')
# create a dictionary from this table
first_date_dict = dict(keys = first_time_carer$PatientsKey, items = first_time_carer$start_carer_date)


# load the data
data = read.csv('S://NDL/MouleshWorkings/Extracts2/GP_Events.txt')

data = data %>% mutate(EventDate = as.Date(EventDate, format = '%Y-%m-%d'))
#data = data[order(data$EventDate),]

colnames(data)[1] = "PatientKey"

# add info about carer/control status
matched = read.csv('S://NDL/MouleshWorkings/221223_carers_and_matched_final_table.csv')
matched2 = matched[,c('PatientKey','MatchedPatientKey')]
data = data %>% left_join( matched2, by = 'PatientKey')


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


###############################
# getting flag to see only values for patients after becoming a carer
data$after_carer = ifelse(data$EventDate > data$first_carer_date,1,0)

# removing carers who have PAM value before becoming carers
data= data[!( (data$after_carer==0) ),]
data = data[order(data$group, data$Patient_DerivedKey),]


write.csv(d_temp, 'S://NDL/MouleshWorkings/3_HC_access/GP/GP_adj_part2.csv')


#d_temp = as.data.frame(head(data ,200000 ))


# only include groups which contain carer + controls || USING LAG
# check if group[1] and group[2] are the same   / then we can assign the missed [1] lag
data$group[1]
data$group[2]

data$grouplag = lag(data$group) 
data$grouplag[1] = data$grouplag[2]

# check if carer[1] and carer[2] are the same
data$carer[1]
data$carer[2]
data$carerlag = lag(data$carer) 
data$carerlag[1] = data$carerlag[2]

# function to derive change flag - shows that a group contain at least 1 carer and 1 control
derive_change_flag = function(carer, carerlag, group, grouplag){
  change_flag = 0
  if ( (group == grouplag) & (carer != carerlag) ) {change_flag = 1}
  return(change_flag)
}
data$change_flag = mapply(derive_change_flag, data$carer,data$carerlag,data$group,data$grouplag )

# order change_flag so that if there is value 1 it would be seen
data = data[order(data$group, -data$change_flag),]

# create dictionary for change_flags for each group
# get the first line for 
data_g = data[!duplicated(data$group),]
library("collections")
group_ch_flag_dict = dict(keys = data_g$group, items = data_g$change_flag)

# function to assign change flag (if 1) to the whole group
get_ch_flag_for_group = function(group){
  return( group_ch_flag_dict$get(group) )
}
data$change_flag2 = mapply(get_ch_flag_for_group, data$group)

# filter groups that did not have both carer and controls
data = data[!data_a2$change_flag2==0,]


saveRDS(data, 'S://NDL/MouleshWorkings/3_HC_access/GP/GP_adj_raw.Rda')
#write.csv(data_a2, 'S://NDL/MouleshWorkings/3_HC_access/OP/OP_adj.csv')


#write.csv(data_a2, 'C://Users/galimove/Documents/221101 AnE v2_adj.csv')
data_a2 = readRDS('S://NDL/MouleshWorkings/3_HC_access/GP/GP_adj.csv')

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


data = data_a2
# # Create invervals since beginning months, quoters, years
# data$m = interval( as.Date("2015-06-01"), data$ArrivalDate ) %/% months(1)
# data$q = interval( as.Date("2015-06-01"), data$ArrivalDate ) %/% months(3)
# data$y = interval( as.Date("2015-06-01"), data$ArrivalDate ) %/% months(12)

# select only those 18 and older
data = data[data$age > 17,]
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

data$age_cat = mapply(age_cat, data$age)
summary( as.factor(data2$age_cat) )

# filter those without age category
data = data[!data$age_cat == 'None',]

# remove missing data for IMDDecile
data = data[data$IMDDecile != -2,]


saveRDS(data, 'S://NDL/MouleshWorkings/3_HC_access/GP/GP_adj_fin_agecat.Rda')
data = readRDS('S://NDL/MouleshWorkings/3_HC_access/GP/GP_adj_fin_agecat.Rda')


data2 = data_a2
# deduplicate the data
data3 = data[!duplicated(data$Patient_DerivedKey),]
#data3 = data2[!duplicated(data2$PatientKey),]



################
### getting counts of visits for groups split by age_cat, Gender, carer, IMDDecile
d_t4 = data %>%
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

#d_t4_fin2$carer = as.factor(d_t4_fin2$carer)
#d_t4_fin2$carer = relevel(d_t4_fin2$carer, ref = 'Control')
#d_t4_fin2$Gender = as.factor(d_t4_fin2$Gender)
#d_t4_fin2$age_cat = as.factor(d_t4_fin2$age_cat)


write.csv(d_t4_fin ,'S://NDL/MouleshWorkings/3_HC_access/GP/groups_count.csv')
#write.csv(d_t4_fin, 'AnE/AnE_age_gender_imd.csv')
#write.csv(d_t4_fin, 'C://Users/galimove/Documents/AnE_summary_age_gend_carer_imd.csv')


d_t5 = data %>%
  group_by(age_cat, Gender, carer, IMDDecile) %>%
  summarize(visit_counts = n())
d_t5_pop = data %>%
  group_by(age_cat, Gender, carer, IMDDecile) %>%
  summarize(pop = n_distinct(Patient_DerivedKey))
d_t5$pop = d_t5_pop$pop
d_t5$logpop2 = log(d_t5$pop)

write.csv(d_t5 ,'S://NDL/MouleshWorkings/3_HC_access/GP/groups_count2.csv')

colnames(data)
#################################
# getting counts of visits for groups split by age_cat, Gender, carer, IMDDecile     | age as cont variable
###########
#########
d_t4 = data %>%
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

write.csv(d_t4_fin ,'S://NDL/MouleshWorkings/3_HC_access/GP/groups_count_ageCont.csv')

