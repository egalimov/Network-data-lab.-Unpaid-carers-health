
""
#author: Evgeny Galimov
""

# first date
s_dates = read.csv('S://NDL/MouleshWorkings/1_Carers.csv')

s_dates$EventDate2 = as.Date(s_dates$EventDate, format = '%d/%m/%Y')
head(s_dates)
s_dates = s_dates[ order (s_dates[,1], s_dates[,5]) , ]

# number of unique patients
length ( s_dates[!duplicated(s_dates$PatientKey),]$PatientKey ) 
length ( unique( s_dates$PatientKey) ) 


# create a table with unique patients and corresponding first date of becoming a carer
out_table = setNames( data.frame(matrix(ncol = 2, nrow = 0)), c('PatientsKey','start_carer_date')   )
for (i in s_dates[!duplicated(s_dates$PatientKey),]$PatientKey ) {
  # i=2
  t = s_dates[s_dates$PatientKey == i,]
  out_table[nrow(out_table) + 1,] = c(  i, as.character( t[1, c('EventDate2')] )  )
  
} 
out_table$start_carer_date = as.Date(out_table$start_carer_date, format = '%Y-%m-%d')
out_table$start_carer_date = as.character(out_table$start_carer_date)

out_table$PatientsKey = as.numeric(out_table$PatientsKey)

# create a dictionary from this table
first_date_dict = dict(keys = out_table$PatientsKey, items = out_table$start_carer_date)

write.csv(out_table, 'S://NDL/MouleshWorkings/230110_first_time_carer.csv')
