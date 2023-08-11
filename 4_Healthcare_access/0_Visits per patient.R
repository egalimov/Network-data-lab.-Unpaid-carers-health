""
#author: Evgeny Galimov
""

library(stringr)

### Get visits per year for A&E

data = read.csv('S://NDL/MouleshWorkings/3_HC_access/AnE/AnE_adj.csv')
data$year  = as.numeric( substr(data$ArrivalDate,1,4) )

# create output table
d_t5 = data %>%
  group_by(carer, year) %>%
  summarize(visit_counts = n())
d_t5_pop = data %>%
  group_by(carer, year) %>%
  summarize(pop = n_distinct(Patient_DerivedKey))
d_t5$pop = d_t5_pop$pop

d_t5$rate = d_t5$visit_counts/d_t5$pop
write.csv(d_t5, 'S://NDL/MouleshWorkings/3_HC_access/AnE/visits_per_year')




## Get visits per year for Outpatients data
data = read.csv('S://NDL/MouleshWorkings/3_HC_access/OP/OP_adj_fin_agecat.csv')

data$year  = as.numeric( substr(data$AppointmentDate,1,4) )

# create output table
d_t5 = data %>%
  group_by(carer, year) %>%
  summarize(visit_counts = n())
d_t5_pop = data %>%
  group_by(carer, year) %>%
  summarize(pop = n_distinct(Patient_DerivedKey))
d_t5$pop = d_t5_pop$pop

d_t5$rate = d_t5$visit_counts/d_t5$pop
write.csv(d_t5, 'S://NDL/MouleshWorkings/3_HC_access/OP/visits_per_year')



## Get visits per year for GP data

data = readRDS('S://NDL/MouleshWorkings/3_HC_access/GP/GP_adj_fin_agecat.Rda')
data = data %>% mutate(year = as.Date(EventDate, format = '%Y'))
data$year = format(data$EventDate, format = '%Y')
data$year2 = as.numeric( as.character(data$year) )

# create output table
d_t5 = data %>%
  group_by(carer, year2) %>%
  summarize(visit_counts = n())
d_t5_pop = data %>%
  group_by(carer, year2) %>%
  summarize(pop = n_distinct(Patient_DerivedKey))
d_t5$pop = d_t5_pop$pop

d_t5$rate = d_t5$visit_counts/d_t5$pop

write.csv(d_t5, 'S://NDL/MouleshWorkings/3_HC_access/GP/visits_per_year')




## Get visits per year for Prescriptions data
data = readRDS('S://NDL/MouleshWorkings/3_HC_access/Presc/Presc_adj_fin_agecat.Rda')
data$year = format(data$IssueDate, format = '%Y')
data$year2 = as.numeric( as.character(data$year) )

# create output table
d_t5 = data %>%
  group_by(carer, year) %>%
  summarize(visit_counts = n())
d_t5_pop = data %>%
  group_by(carer, year) %>%
  summarize(pop = n_distinct(PatientKey))
d_t5$pop = d_t5_pop$pop

d_t5$rate = d_t5$visit_counts/d_t5$pop

write.csv(d_t5, 'S://NDL/MouleshWorkings/3_HC_access/Presc/visits_per_year')

##







