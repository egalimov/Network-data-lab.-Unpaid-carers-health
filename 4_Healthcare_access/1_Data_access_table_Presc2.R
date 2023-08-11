
library(dplyr)
library(tidyverse)
library(tibble)
library(collections)
# create dictionary with carers start dates
first_time_carer = read.csv('S://NDL/MouleshWorkings/230110_first_time_carer.csv')


# create a dictionary from this table
first_date_dict = dict(keys = first_time_carer$PatientsKey, items = first_time_carer$start_carer_date)


library(odbc)
con = dbConnect(odbc::odbc(),
                Driver = "SQL Server",
                Server = "NWLPRODSQL01",
                Database = "NWL_WSIC_Deident_Sandbox",
                Trusted_Connection = "True")



data = read.csv('S://NDL/MouleshWorkings/Extracts2/GP_Prescriptions.txt')


#data = data[!(names(data) %in% c('group','group.1','group.2','group.3'))]
data = data %>% mutate(IssueDate = as.Date(IssueDate, format = '%Y-%m-%d'))

data$age = as.numeric(data$age)
summary(data$age)

data$Gender = as.factor(data$Gender)
summary(data$Gender)
data = data[data$Gender != 'Unknown',]
data$Gender = droplevels(data$Gender)
data$IMDDecile = as.numeric(data$IMDDecile)
summary(data$IMDDecile)



summary(data$PatientKey)
unique(data$PatientKey2)
#summary(data$Pre_or_Post)
colnames(data)
summary(data$IssueDate)
summary(as.factor(data$EthnicCategory))

summary(as.numeric(data$age))
summary(as.factor(data$Gender))
summary(data$IMDDecile)


colnames(data)[1] = "PatientKey0"
colnames(data)[14] = "PatientKey"

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
data$after_carer = ifelse(data$IssueDate > data$first_carer_date,1,0)

# removing carers who have PAM value before becoming carers
data = data[!( (data$after_carer==0) ),]

colnames(data)[1] = "PatientKey"
data = data[order(data$group, data$PatientKey),]


# only include groups which contain carer + controls || USING LAG
data$carer = as.factor(data$carer)
data = data[!is.na(data$carer),]
summary(data$carer)

data$group = as.numeric(data$group)
data = data[!is.na(data$group),]
summary(data$group)


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


derive_change_flag = function(carer, carerlag, group, grouplag){
  change_flag = 0
  if ( (group == grouplag) & (carer != carerlag) ) {change_flag = 1}
  return(change_flag)
}
data$change_flag = mapply(derive_change_flag, data$carer,data$carerlag,data$group,data$grouplag )

data = data[order(data$group, -data$change_flag),]


# create dictionary for change_flags for each group
# get the first line for 
data_g = data[!duplicated(data$group),]
library("collections")
group_ch_flag_dict = dict(keys = data_g$group, items = data_g$change_flag)


get_ch_flag_for_group = function(group){
  return( group_ch_flag_dict$get(group) )
}
data$change_flag2 = mapply(get_ch_flag_for_group, data$group)


data = data[!data$change_flag2==0,]

#a2 = data_a2[1:10000,c('group','Patient_DerivedKey','carer','after_carer', 'change_flag')]

summary(data$change_flag2)
saveRDS(data, 'S://NDL/MouleshWorkings/3_HC_access/Presc/Presc_adj_raw.Rda')
data = readRDS('S://NDL/MouleshWorkings/3_HC_access/Presc/Presc_adj_raw.Rda')


####
a3 = data[1:200000,c('PatientKey','group', 'MatchedPatientKey','first_carer_date','IssueDate','after_carer')]
a4 = a5[!duplicated(a5$group),]

a = as.data.frame(head(data ,100000 ))
a5 = a[1:200000,c('PatientKey','group', 'MatchedPatientKey','first_carer_date','IssueDate','after_carer',
                  'grouplag','carerlag','change_flag','change_flag2')]

d_temp = as.data.frame(head(data ,20000 ))

head(data_g[data_g$change_flag==0,])

for (i in a4$group[1:20]){
  a = data[data$group==i,]
  print('-----------------')
  print( paste('Group: ',as.character(i), sep='') )
  print( summary(as.factor(a$carer))   )
  print( summary(a$change_flag)   )
}

summary(data$age)


#write.csv(data_a2, 'C://Users/galimove/Documents/221101 AnE v2_adj.csv')
data_a2 = read.csv('S://NDL/MouleshWorkings/3_HC_access/Presc/Presc_adj.csv')

#a2 = data_a2[1:10000,c('group','Patient_DerivedKey','carer','after_carer', 'change_flag')]

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
# choose only those 18 and older
data = data[data$age > 17,]

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
summary( as.factor(data$age_cat) )

data = data[!data$age_cat == 'None',]

summary(data2$age)
# remove missing data for IMDDecile
data = data[data$IMDDecile != -2,]
hist(data$age)


saveRDS(data, 'S://NDL/MouleshWorkings/3_HC_access/Presc/Presc_adj_fin_agecat.Rda')


#data2 = data_a2
# deduplicate the data
data3 = data[!duplicated(data$PatientKey),]
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


d_t5 = data %>%
  group_by(age_cat, Gender, carer, IMDDecile) %>%
  summarize(visit_counts = n())
d_t5_pop = data %>%
  group_by(age_cat, Gender, carer, IMDDecile) %>%
  summarize(pop = n_distinct(PatientKey))

d_t5$logpop2 = log(d_t5$pop)

write.csv(d_t5 ,'S://NDL/MouleshWorkings/3_HC_access/Presc/groups_count2.csv')

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


write.csv(d_t4_fin ,'S://NDL/MouleshWorkings/3_HC_access/Presc/groups_count_ageCont.csv')

