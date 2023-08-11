
library(magrittr)
library(predtools)
library(caret)
library(pROC)
library(plotROC)
library(dplyr)
library(jtools)
library(arm)

ane_events = read.csv('S://NDL/MouleshWorkings/3_HC_access/AnE/AnE_adj.csv')
ane_events_c = ane_events[ane_events$carer=='carer',]


ane_events$Appts = 1


colnames(ane_events)
all = read.csv('S://NDL/MouleshWorkings/221223_carers_and_matched_final_table.csv')


#all = read.csv('S://NDL/3_Soc_care/0_data_stream/SatAnalysis/1_2__data_extraction_and_preparation/220905_carers_and_matched_final_table_ethn_description.csv')
all_dedup = all[!duplicated(all$PatientKey),]


p = ane_events[!duplicated(ane_events$Patient_DerivedKey),]

a = unique(ane_events$Patient_DerivedKey)


a0 = unique(all$PatientKey)
a02 = all[!duplicated(all$PatientKey),]

a_int = intersect(a, a0)

data_clean = all_dedup[ !all_dedup$PatientKey %in% a, ]

data_clean$Appts = 0
colnames(data_clean)
data_clean$carer = ifelse(data_clean$MatchedPatientKey=='carer','carer','control')

ane_events_un = ane_events[!duplicated(ane_events$Patient_DerivedKey),
              c('Patient_DerivedKey','age','Gender','carer','IMDDecile','Appts')]
colnames(ane_events_un)[1] = 'PatientKey'

count_merged = rbind(ane_events_un, data_clean[, c('PatientKey','age','Gender','carer','IMDDecile','Appts')])
colnames(count_merged)[6] = 'appt' 
count_merged$carer = as.factor(count_merged$carer)
count_merged$Gender = as.factor(count_merged$Gender)
count_merged = count_merged[count_merged$Gender!='Other',] 
count_merged = count_merged[count_merged$Gender!='Unknown',] 
count_merged$Gender = droplevels(count_merged$Gender)

count_merged$visit_bin = ifelse(count_merged$appt>0,1,0)

write.csv(count_merged, 'S://NDL/MouleshWorkings/3_HC_access/AnE/logr_data.csv')


run_and_save_the_model_logr = function(data, out_true, path, form_str, model_name){
  
  if (!file.exists(path)) {dir.create(path)}
  m1 = glm(form_str, data = data, family=binomial)
  sink(file = paste(path,model_name,'_summary.txt', sep = ''))
  # print( summary(m1) )
  print( summ(m1, exp=T, confint=TRUE, digits = 5, ciwidth=0.05 ) )
  
  sink()
  
  
  # conf matrix
  pred = as.factor(ifelse(fitted(m1) >=.5, 1, 0))
  if ( (length(levels(pred))==1) & (levels(pred)[1]==1) ) {levels(pred)[length(levels(pred))+1] = '0'}
  if ( (length(levels(pred))==1) & (levels(pred)[1]==0) ){levels(pred)[length(levels(pred))+1] = '1'}
  
  sink(file = paste(path,model_name,'_conf.matrix.txt', sep = ''))
  print(   confusionMatrix(pred, as.factor(out_true)   )   )
  sink()
  
  binnedplot(fitted(m1), residuals(m1, type = 'response'), 
             nclass = NULL,xlab='Expected values',ylab='Average residual',
             main='Binned residual plot',
             cex.pts=0.8,col.pts=1,col.int='grey')
  b1 = recordPlot()
  png(paste(path,model_name,'_plot1_binned_plot.png', sep = ''), 
      width = 20, height = 15, units = 'cm', res = 200  )
  print(b1)
  dev.off()
  
  # roc auc
  # out_true = efi$efi15 (0,1 - numeric)
  invisible(  plot(    roc(out_true, fitted(m1)   ),print.auc=T, col='red'      )  )
  pauc = recordPlot()
  png(paste(path,model_name,'_plot2_roc_auc.png', sep = ''), 
      width = 20, height = 15, units = 'cm', res = 200  )
  print(pauc)
  dev.off()
  
  
  # calibration plot
  data_plot = data
  #data_plot$pred = predict.glm(m1, type='response')
  #calibration_plot(data = data_plot, obs=out_true, pred=pred,'Calibration plot')
  
  data_plot$y = as.numeric( as.character( out_true ) )
  data_plot$pred = predict.glm(m1, type = 'response')

  c_plot = calibration_plot(data_plot, obs='y',  pred='pred' )
  #c_plot = recordPlot()
  png(paste(path,model_name,'_plot3_calibration_plot.png', sep = ''), 
      width = 20, height = 15, units = 'cm', res = 200  )
  print(c_plot)
  dev.off()  
}


data = count_merged
path = 'S://NDL/MouleshWorkings/3_HC_access/AnE/'
out_true = as.factor(count_merged$visit_bin)
count_merged$carer = relevel(count_merged$carer, ref = 'control')

form_str = as.formula('visit_bin ~ carer + age + Gender + IMDDecile')
model_name = '8_logr_ageCont'

summary(  as.factor( as.character(data$visit_bin)  )  )

data$appt = as.numeric( as.character(data$appt) )
data$carer = as.numeric( as.character(data$carer) )

m1 = glm(visit_bin ~ carer + age + Gender + IMDDecile, data = data, family=binomial)

m1 = glm(pam_cat_above60 ~ carer+Gender+age+IMDDecile, data = final_table4, family=binomial)

run_and_save_the_model_logr(data, out_true, path, form_str, model_name)

m1 = glm(as.factor(appt) ~ carer + age + Gender + IMDDecile, data = data, family=binomial)

summary(count_merged$Appts)

summary(data$appt)



##########################

# cross_tab   carer VS bin_visit
sink(file = paste(path,model_name,'_cross_tab__carerVSbin_visit.txt', sep = ''))
# print( summary(m1) )
print( 'Cross tab for #carer vs bin_counts#'  )
print( table(count_merged$carer, count_merged$visit_bin) )
sink()



# cross_tab   carer VS cumulative_visits
ane_events = read.csv('S://NDL/MouleshWorkings/3_HC_access/AnE/AnE_adj.csv')
ane_events$Appts = 1
ane_events$sum_appts = 0

ane_events_mult_sum = ane_events %>%
  group_by(Patient_DerivedKey) %>%
  summarize(sum_appts = sum(Appts))

p = p %>% left_join( ane_events_mult_sum, by = 'Patient_DerivedKey')

sink(file = paste(path,model_name,'_mean__carerVSmult_visit.txt', sep = ''))
print( 'mean visits per carer' )
sum(p[p$carer=='carer',]$sum_appts)/nrow(p[p$carer=='carer',])
print( 'mean visits per control' )
sum(p[p$carer=='control',]$sum_appts)/nrow(p[p$carer=='control',])
sink()





