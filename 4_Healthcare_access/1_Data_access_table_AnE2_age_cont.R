
library(dplyr)
library(tidyverse)
library(tibble)
library(collections)

# create dictionary with carers start dates
first_time_carer = read.csv('S://NDL/MouleshWorkings/230110_first_time_carer.csv')



# create a dictionary from this table
first_date_dict = dict(keys = first_time_carer$PatientsKey, items = first_time_carer$start_carer_date)


# load the data
data = read.csv('S://NDL/3_Soc_care/0_data_stream/SatAnalysis/1_2__data_extraction_and_preparation/220922 AnE v2.csv')
#summary(data$Pre_or_Post)

colnames(data)[1] = "Patient_DerivedKey"


list.files()
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
data = add_column(data,group = as.numeric(  ifelse(data$MatchedPatientKey != 'carer',data$MatchedPatientKey, data$Patient_DerivedKey )  ), .after = 'Patient_DerivedKey' )
data = data[order(data$group),]


# get first dates 
get_first_carer_date = function(group){
  return( first_date_dict$get(as.integer(group)) )
}
data$first_carer_date = mapply(get_first_carer_date, data$group)
data$first_carer_date = as.Date(data$first_carer_date, format = '%Y-%m-%d')


# reformating date
data = data %>% mutate(ArrivalDate = as.Date(ArrivalDate, format = '%Y-%m-%d'))

###############################
# getting flag to see only values for patients after becoming a carer
data$after_carer = ifelse(data$ArrivalDate > data$first_carer_date,1,0)

# removing carers who have events before becoming carers
data_a = data[!( (data$after_carer==0)  ),]

# only include groups which contain carer + controls 
data_a2 = data_a
for (i in data_a2[!duplicated(data_a2$group),]$group) {
  #i=2577
  t = data_a2[data_a2$group == i,]
  if (  (nrow( t[t$MatchedPatientKey != 'carer',] )==0) | (nrow( t[t$MatchedPatientKey == 'carer',] )==0) ){
    data_a2 = data_a2[!data_a2$group ==i,]
  }
}

write.csv(data_a2, 'S://NDL/MouleshWorkings/3_HC_access/AnE/AnE_adj_cont.csv')
#write.csv(data_a2, 'C://Users/galimove/Documents/221101 AnE v2_adj.csv')
data_a2 = read.csv('S://NDL/3_Soc_care/0_data_stream/SatAnalysis/1_2__data_extraction_and_preparation/4_Healthcare_accesss/221101 AnE v2_adj.csv')
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

# remove missing data for IMDDecile
data2 = data2[data2$IMDDecile != -2,]
#write.csv(data2, 'S://NDL/3_Soc_care/0_data_stream/SatAnalysis/1_2__data_extraction_and_preparation/4_Healthcare_accesss/221101 AnE v2_adj_fin.csv')
#write.csv(data_a2, 'C://Users/galimove/Documents/221101 AnE v2_adj.csv')
#data_a2 = read.csv('S://NDL/3_Soc_care/0_data_stream/SatAnalysis/1_2__data_extraction_and_preparation/4_Healthcare_accesss/221101 AnE v2_adj_fin.csv')


# if age categories were not set
write.csv(data2, 'S://NDL/3_Soc_care/0_data_stream/SatAnalysis/1_2__data_extraction_and_preparation/4_Healthcare_accesss/221101 AnE v2_adj_fin_ageCont.csv')
#write.csv(data_a2, 'C://Users/galimove/Documents/221101 AnE v2_adj.csv')
data_a2 = read.csv('S://NDL/3_Soc_care/0_data_stream/SatAnalysis/1_2__data_extraction_and_preparation/4_Healthcare_accesss/221201/221101 AnE v2_adj_fin_ageCont.csv')






# deduplicate the data
data3 = data2[!duplicated(data2$Patient_DerivedKey),]
#data3 = data2[!duplicated(data2$PatientKey),]


#################################
# getting counts of visits for groups split by age_cat, Gender, carer, IMDDecile     | age as cont variable
###########
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

d_t4_fin2$carer = as.factor(d_t4_fin2$carer)
d_t4_fin2$carer = relevel(d_t4_fin2$carer, ref = 'Control')
d_t4_fin2$Gender = as.factor(d_t4_fin2$Gender)
d_t4_fin2$age_cat = as.factor(d_t4_fin2$age_cat)


write.csv(d_t4_fin ,'S://NDL/MouleshWorkings/3_HC_access/AnE/group_counjts_cont.csv')
