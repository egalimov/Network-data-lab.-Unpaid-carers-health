""
#author: Evgeny Galimov
""

# Scripts to get visits per month time series - for healthcare access
library(dplyr)

#################################################################################
###############         A&E              #####################
#################################################################################
# A&E
events = read.csv('S://NDL/MouleshWorkings/3_HC_access/AnE/AnE_adj.csv')

events_c = events[events$carer=='carer',]

# format the date
events_c = events_c %>% mutate(ArrivalDate = as.Date(ArrivalDate, format = '%Y-%m-%d'))

events_c$yymm = interval( as.Date("2015-02-01"), events_c$ArrivalDate ) %/% months(1)
events_c$Time =  format( as.Date(events_c$ArrivalDate), '%Y-%m')

events_c = events_c[ order(events_c$Time), ]
a = events_c[,c('ArrivalDate','Time')]

# create output table
d_t4 = events_c %>%
  group_by(Time) %>%
  summarize(Visits = n())
d_t4_pop = events_c %>%
  group_by(Time) %>%
  summarize(pop = n_distinct(Patient_DerivedKey))
d_t4 = merge(d_t4, d_t4_pop, by='Time', all.x=TRUE )
min(d_t4$pop)
d_t4$Rate = d_t4$Visits/d_t4$pop


write.csv(d_t4 ,'S://NDL/MouleshWorkings/3_HC_access/AnE/Carer_visits_vs.time.csv')



#################################################################################
###############         OP              #####################
#################################################################################
# OP
events = read.csv('S://NDL/MouleshWorkings/3_HC_access/OP/OP_adj_fin_agecat.csv')

events_c = events[events$carer=='carer',]
colnames(events)
summary(as.factor( events_c$carer)  )
summary(as.factor( events_c$after_carer)  )

# format the date
events_c = events_c %>% mutate(AppointmentDate = as.Date(AppointmentDate, format = '%Y-%m-%d'))


events_c$yymm = interval( as.Date("2015-02-01"), events_c$AppointmentDate ) %/% months(1)
events_c$Time =  format( as.Date(events_c$AppointmentDate), '%Y-%m')


events_c = events_c[ order(events_c$Time), ]
#a = events_c[,c('AppointmentDate','Time')]

# create output table
d_t4 = events_c %>%
  group_by(Time) %>%
  summarize(Visits = n())
d_t4_pop = events_c %>%
  group_by(Time) %>%
  summarize(pop = n_distinct(Patient_DerivedKey))
d_t4 = merge(d_t4, d_t4_pop, by='Time', all.x=TRUE )
min(d_t4$pop)
d_t4$Rate = d_t4$Visits/d_t4$pop


write.csv(d_t4 ,'S://NDL/MouleshWorkings/3_HC_access/OP/Carer_visits_vs.time.csv')





#################################################################################
###############         GP              #####################
#################################################################################
# GPs
events = readRDS('S://NDL/MouleshWorkings/3_HC_access/GP/GP_adj_fin_agecat.Rda')

events_c = events[events$carer=='carer',]
colnames(events)
summary(as.factor( events_c$carer)  )
summary(as.factor( events_c$after_carer)  )

# format the date
events_c = events_c %>% mutate(EventDate = as.Date(EventDate, format = '%Y-%m-%d'))


events_c$yymm = interval( as.Date("2015-02-01"), events_c$EventDate ) %/% months(1)
events_c$Time =  format( as.Date(events_c$EventDate), '%Y-%m')


events_c = events_c[ order(events_c$Time), ]

# create output table
d_t4 = events_c %>%
  group_by(Time) %>%
  summarize(Visits = n())
d_t4_pop = events_c %>%
  group_by(Time) %>%
  summarize(pop = n_distinct(Patient_DerivedKey))
d_t4 = merge(d_t4, d_t4_pop, by='Time', all.x=TRUE )
min(d_t4$pop)
d_t4$Rate = d_t4$Visits/d_t4$pop


write.csv(d_t4 ,'S://NDL/MouleshWorkings/3_HC_access/GP/Carer_visits_vs.time.csv')



#################################################################################
###############         Presc            #####################
#################################################################################
# Presc
events = readRDS('S://NDL/MouleshWorkings/3_HC_access/Presc/Presc_adj_fin_agecat.Rda')

events_c = events[events$carer=='carer',]
colnames(events)
summary(as.factor( events_c$carer)  )
summary(as.factor( events_c$after_carer)  )

# format the date
events_c = events_c %>% mutate(IssueDate = as.Date(IssueDate, format = '%Y-%m-%d'))


events_c$yymm = interval( as.Date("2015-02-01"), events_c$IssueDate ) %/% months(1)
events_c$Time =  format( as.Date(events_c$IssueDate), '%Y-%m')


events_c = events_c[ order(events_c$Time), ]
a = events_c[1:10000,c('IssueDate','Time')]



# create output table
d_t4 = events_c %>%
  group_by(Time) %>%
  summarize(Visits = n())
d_t4_pop = events_c %>%
  group_by(Time) %>%
  summarize(pop = n_distinct(PatientKey))
d_t4 = merge(d_t4, d_t4_pop, by='Time', all.x=TRUE )
min(d_t4$pop)
d_t4$Rate = d_t4$Visits/d_t4$pop


write.csv(d_t4 ,'S://NDL/MouleshWorkings/3_HC_access/Presc/Carer_visits_vs.time.csv')

