library(tidyverse)
library(readxl)
library(data.table)
library(zoo)
library(openxlsx)

options(scipen = 999)

#Read in data####


# refs <- fread('../1_2__data_extraction_and_preparation_2/Revised Codes Data Extract V2/OP Referral data/22_12_06_OP_Referrals_by_Specility.csv')
refs <- fread('S:/NDL/mouleshworkings/Extracts2/Referral_adjusted.csv')


################# Added by EG ######################################
# include only events that happened after becoming a carer
refs <- refs %>% filter(age >= 18)

refs = refs[refs$after_carer==1,]
# only include groups which contain carer + controls 
for (i in refs[!duplicated(refs$group),]$group) {
  t = refs[refs$group == i,]
  if (  (nrow( t[t$MatchedPatientKey != 'carer',] )==0) | (nrow( t[t$MatchedPatientKey == 'carer',] )==0) ){
    refs = refs[!refs$group ==i,]
  }
}

dat <- refs %>%
  mutate(carer = if_else(condition = MatchedPatientKey == 'carer',
                         true =  'Unpaid carer',
                         false =  'Non-carer')) %>%
  select(PatientKey
         ,carer
         ,group
         ,first_carer_date
         ,age
         ,Gender
         ,IMDDecile
         ,EthnicCategory
         ,AppointmentDate
         ,SpecialtyCode
         ,SpecialtyDescription
  )
###################################################################

# 
# 
# # ref_lup <- read_excel('../1_2__data_extraction_and_preparation_2/Revised Codes Data Extract V2/OP Referral data/OP Speciality Code Look Up.xlsx')
# pats <- fread("../1_2__data_extraction_and_preparation/220920_carers_and_matched_final_table_LTCs_first_date_adjusted.csv")
# 
# pats <- pats %>% filter(age >= 18)
# 
# pats <- pats %>%
#   mutate(carer = if_else(condition = MatchedPatientKey == 'carer',
#                          true =  'Unpaid carer',
#                          false =  'Non-carer')) %>%
#   select(PatientKey
#          ,carer
#          ,group
#          ,first_carer_date
#          ,age
#          ,Gender
#          ,IMDDecile
#          ,EthnicCategory
#          ,LADistrictName) 
# 
# refs <- refs %>% 
#   select(
#     PatientKey
#     ,AppointmentDate
#     ,SpecialtyCode
#     ,SpecialtyDescription
#   )
# 
# dat <- left_join(pats,refs, by='PatientKey')


#Exploratoty analysis####

plt_data <- dat %>% group_by(carer,SpecialtyDescription) %>% summarise(n=n())
plt_data <- plt_data %>% filter(n>1000)

plt_ord <- plt_data %>% filter(carer == 'Non-carer') %>% arrange(desc(n))
plt_data$SpecialtyDescription <- factor(plt_data$SpecialtyDescription,levels = plt_ord$SpecialtyDescription)

ggplot(plt_data) + 
  geom_bar(aes(x=carer,y=n,fill=carer),stat='identity',position = 'dodge') + 
  facet_wrap(~SpecialtyDescription)

#ITS analysis####
dat <- dat %>% filter(!is.na(SpecialtyDescription))

model_mnth <- dat %>%
  mutate(ym = as.yearmon(AppointmentDate)) %>%
  mutate(age_group = case_when(age <= 29 ~ '0-29',
                               between(age,30,49) ~ '30-49',
                               between(age,50,69) ~ '50-69',
                               age >= 70 ~ '70+')) %>%
  mutate(covid =  AppointmentDate >= as.Date("2020-03-01")) %>%
  mutate(covid_time = ifelse(AppointmentDate >= as.Date("2020-03-01") & carer == "Unpaid carer",'during','pre')) %>%
  filter(Gender %in% c("Female",'Male')) 

model_qurt <- dat %>%
  mutate(ym = as.yearmon(as.Date(as.yearqtr(AppointmentDate)))) %>%
  mutate(age_group = case_when(age <= 29 ~ '0-29',
                               between(age,30,49) ~ '30-49',
                               between(age,50,69) ~ '50-69',
                               age >= 70 ~ '70+')) %>%
  mutate(covid =  AppointmentDate >= as.Date("2020-03-01")) %>%
  mutate(covid_time = ifelse(AppointmentDate >= as.Date("2020-03-01") & carer == "Unpaid carer",'during','pre')) %>%
  filter(Gender %in% c("Female",'Male')) 


time_series_analysis_data <- function(data = model,target_var = "age_group", category = NULL){
  
  data = data.frame(data)
  data$target <- data[,target_var]
  if(!is.null(category)){
    data <- data %>% filter(target == category)
  }
  
  totals <- nrow(data)
  
  data <- subset(data, !is.na(data$covid_time))
  
  #Make dataset
  data <- data %>% 
    group_by(ym,covid_time,carer,covid) %>% 
    summarise(n=n(),
              unique_patients = length(unique(PatientKey))) %>%
    mutate(datex = as.Date(paste0("01 ",ym), format = "%d %B %Y")) %>%
    ungroup() %>% 
    arrange(datex) %>% 
    mutate(daten = 1:n(),
           target_variable = target_var,
           subcat = category,
           Year = year(datex),
           Month = month(datex),
           # Pandemic = as.numeric(covid_time) - 1,
           Pandemic = covid_time,
           RegPtList = totals,
           unique_patients) %>% 
    select(Year,
           Month,
           date = datex,
           subcat,
           MonthYear = ym,
           daten,
           Pandemic,
           n,
           RegPtList,
           covid,
           unique_patients,
           carer)
  
  return(data)
}
time_series_analysis_model2 <- function(data,seasonal = F){
  
  data$covid_time <- factor(data$Pandemic, levels = c("pre","during"))
  
  if(seasonal){
    model <- MASS::glm.nb(n ~ offset(log(total)) + covid_time + carer + daten + covid + tsModel::harmonic(month,1,12),
                          data = data)
  } else {
    model <- MASS::glm.nb(n ~ offset(log(total)) + covid_time + carer + daten + covid,
                          data = data)
  }
  
  return(model)
}
time_series_analysis_model2_lm <- function(data,seasonal = F){
  
  data$covid_time <- factor(data$Pandemic, levels = c("pre","during"))
  
  if(seasonal){
    model <- lm(n ~ covid_time + carer +  daten + covid + tsModel::harmonic(month,1,12),
                data = data)
  } else {
    model <- lm(n ~  covid_time + carer +  daten + covid,
                data = data)
  }
  
  return(model)
}
time_series_analysis_plot2_lm <- function(data,model,add_stats = F){
  
  data_to_predict_uc <- data %>% filter(covid == T & carer == 'Unpaid carer')
  pre_data_uc <- data %>% filter(covid == F & carer == 'Unpaid carer')
  
  data_to_predict_nc <- data %>% filter(covid == T & carer == 'Non-carer')
  pre_data_nc <- data %>% filter(covid == F & carer == 'Non-carer')
  
  
  #Pre data
  preds_uc <- predict(model, pre_data_uc, se.fit = T)
  preds_uc <- data.frame(n = preds_uc$fit,
                         lwr = preds_uc$fit - (2 * preds_uc$se.fit),
                         upr = preds_uc$fit + (2 * preds_uc$se.fit),
                         date = pre_data_uc$date,
                         cat = 'Unpaid carer')
  
  preds_nc <- predict(model, pre_data_nc, se.fit = T)
  preds_nc <- data.frame(n = preds_nc$fit,
                         lwr = preds_nc$fit - (2 * preds_nc$se.fit),
                         upr = preds_nc$fit + (2 * preds_nc$se.fit),
                         date = pre_data_nc$date,
                         cat = 'Non-carer')
  
  #During data
  
  preds_uc_dur <- predict(model, data_to_predict_uc, se.fit = T)
  preds_uc_dur <- data.frame(n = preds_uc_dur$fit,
                             lwr = preds_uc_dur$fit - (2 * preds_uc_dur$se.fit),
                             upr = preds_uc_dur$fit + (2 * preds_uc_dur$se.fit),
                             date = data_to_predict_uc$date,
                             cat = 'Unpaid carer')
  
  preds_nc_dur <- predict(model, data_to_predict_nc, se.fit = T)
  preds_nc_dur <- data.frame(n = preds_nc_dur$fit,
                             lwr = preds_nc_dur$fit - (2 * preds_nc_dur$se.fit),
                             upr = preds_nc_dur$fit + (2 * preds_nc_dur$se.fit),
                             date = data_to_predict_nc$date,
                             cat = 'non-carer')
  
  p <- ggplot() + 
    #Original data
    geom_point(data = data, mapping = aes(x=date,y=n,color = "p1"), size= 0.5) +
    #Pre-pandemic models
    geom_line(data = preds_uc, mapping = aes(x=date,y=n,color = "p2"), size= 1,linetype=1) +
    geom_ribbon(data = preds_uc, mapping = aes(x = date, y=n, ymin = lwr, ymax = upr), color = "grey50", alpha = 0.2) +
    geom_line(data = preds_nc, mapping = aes(x=date,y=n,color = "p3"), size= 1,linetype=1) +
    geom_ribbon(data = preds_nc, mapping = aes(x = date, y=n, ymin = lwr, ymax = upr), color = "grey50", alpha = 0.2) +
    #Predictions for model without COVID data
    geom_line(data = preds_uc_dur, mapping = aes(x=date,y=n,color = "p4"), size= 1,linetype=2) +
    geom_ribbon(data = preds_uc_dur, mapping = aes(x = date, y=n, ymin = lwr, ymax = upr), color = "grey50", alpha = 0.2) +
    geom_line(data = preds_nc_dur, mapping = aes(x=date,y=n,color = "p5"), size= 1,linetype=2) +
    geom_ribbon(data = preds_nc_dur, mapping = aes(x = date, y=n, ymin = lwr, ymax = upr), color = "grey50", alpha = 0.2) +
    #Set colors
    scale_color_manual(breaks = c("p1","p2","p3","p4",'p5'), 
                       values = c("grey50"
                                  ,"royalblue1","firebrick1"
                                  ,"blue4", "firebrick4"),
                       labels = c("Original data",
                                  "Pre-pandemic values for unpaid carers","Pre-pandemic values for  non-carers",
                                  "Pandemic values for unpaid carers","Pandemic values for  non-carers")) + 
    labs(x= "Date", y= "Number of referrals", color = "") +
    theme_bw() + 
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 2))
  
  if(add_stats){
    msum <- summary(model)
    
    covid_imp <- msum$coefficients[rownames(msum$coefficients) == "covid_timeduring",]
    stat_text_1 <- paste0("Post-pandemic difference:\n",
                          round(covid_imp[1] * 100,2),
                          " (",
                          ifelse(covid_imp[4]<0.001,"p<0.001",
                                 paste0("p = ",round(covid_imp[4],3))),
                          ")")
    
    
    covid_imp <- msum$coefficients[rownames(msum$coefficients) == "covidTRUE",]
    stat_text_2 <- paste0("Overall COVID-19 impact:\n",
                          round(covid_imp[1] * 100,2),
                          " (",
                          ifelse(covid_imp[4]<0.001,"p<0.001",
                                 paste0("p = ",round(covid_imp[4],3))),
                          ")")
    
    covid_imp <- msum$coefficients[rownames(msum$coefficients) == "carerUnpaid carer",]
    stat_text_3 <- paste0("Pre-pandemic difference:\n",
                          round(covid_imp[1] * 100,2),
                          " (",
                          ifelse(covid_imp[4]<0.001,"p<0.001",
                                 paste0("p = ",round(covid_imp[4],3))),
                          ")")
    
    ypos = mean(layer_scales(p)$y$range$range)
    ypos = (ypos + layer_scales(p)$y$range$range[2]) /2
    ypos = (ypos + layer_scales(p)$y$range$range[2]) /2
    
    p <-  p + geom_text(aes(x=as.Date("2021-01-01"), y=ypos,label = stat_text_1)) + ggtitle(unique(data$subcat))
    p <-  p + geom_text(aes(x=as.Date("2019-01-01"), y=ypos,label = stat_text_2)) + ggtitle(unique(data$subcat))
    p <-  p + geom_text(aes(x=as.Date("2017-01-01"), y=ypos,label = stat_text_3)) + ggtitle(unique(data$subcat))
    
  }
  
  return(p)
}
time_series_analysis_plot2 <- function(data,model,add_stats = F){
  
  data_to_predict_uc <- data %>% filter(covid == T & carer == 'Unpaid carer')
  pre_data_uc <- data %>% filter(covid == F & carer == 'Unpaid carer')
  
  data_to_predict_nc <- data %>% filter(covid == T & carer == 'Non-carer')
  pre_data_nc <- data %>% filter(covid == F & carer == 'Non-carer')
  
  
  #Pre data
  preds_uc <- predict(model, pre_data_uc, se.fit = T)
  preds_uc <- data.frame(n = exp(preds_uc$fit),
                         lwr = exp(preds_uc$fit - (2 * preds_uc$se.fit)),
                         upr = exp(preds_uc$fit + (2 * preds_uc$se.fit)),
                         date = pre_data_uc$date,
                         cat = 'Unpaid carer')
  
  preds_nc <- predict(model, pre_data_nc, se.fit = T)
  preds_nc <- data.frame(n = exp(preds_nc$fit),
                         lwr = exp(preds_nc$fit - (2 * preds_nc$se.fit)),
                         upr = exp(preds_nc$fit + (2 * preds_nc$se.fit)),
                         date = pre_data_nc$date,
                         cat = 'Non-carer')
  
  #During data
  
  preds_uc_dur <- predict(model, data_to_predict_uc, se.fit = T)
  preds_uc_dur <- data.frame(n = exp(preds_uc_dur$fit),
                             lwr = exp(preds_uc_dur$fit - (2 * preds_uc_dur$se.fit)),
                             upr = exp(preds_uc_dur$fit + (2 * preds_uc_dur$se.fit)),
                             date = data_to_predict_uc$date,
                             cat = 'Unpaid carer')
  
  preds_nc_dur <- predict(model, data_to_predict_nc, se.fit = T)
  preds_nc_dur <- data.frame(n = exp(preds_nc_dur$fit),
                             lwr = exp(preds_nc_dur$fit - (2 * preds_nc_dur$se.fit)),
                             upr = exp(preds_nc_dur$fit + (2 * preds_nc_dur$se.fit)),
                             date = data_to_predict_nc$date,
                             cat = 'non-carer')
  
  p <- ggplot() + 
    #Original data
    geom_point(data = data, mapping = aes(x=date,y=n,color = "p1"), size= 0.5) +
    #Pre-pandemic models
    geom_line(data = preds_uc, mapping = aes(x=date,y=n,color = "p2"), size= 1,linetype=1) +
    geom_ribbon(data = preds_uc, mapping = aes(x = date, y=n, ymin = lwr, ymax = upr), color = "grey50", alpha = 0.2) +
    geom_line(data = preds_nc, mapping = aes(x=date,y=n,color = "p3"), size= 1,linetype=1) +
    geom_ribbon(data = preds_nc, mapping = aes(x = date, y=n, ymin = lwr, ymax = upr), color = "grey50", alpha = 0.2) +
    #Predictions for model without COVID data
    geom_line(data = preds_uc_dur, mapping = aes(x=date,y=n,color = "p4"), size= 1,linetype=2) +
    geom_ribbon(data = preds_uc_dur, mapping = aes(x = date, y=n, ymin = lwr, ymax = upr), color = "grey50", alpha = 0.2) +
    geom_line(data = preds_nc_dur, mapping = aes(x=date,y=n,color = "p5"), size= 1,linetype=2) +
    geom_ribbon(data = preds_nc_dur, mapping = aes(x = date, y=n, ymin = lwr, ymax = upr), color = "grey50", alpha = 0.2) +
    #Set colors
    scale_color_manual(breaks = c("p1","p2","p3","p4",'p5'), 
                       values = c("grey50"
                                  ,"firebrick1","royalblue1"
                                  ,"firebrick4","blue4"),
                       labels = c("Original data",
                                  "Pre-pandemic values for unpaid carers","Pre-pandemic values for  non-carers",
                                  "Pandemic values for unpaid carers","Pandemic values for  non-carers")) + 
    labs(x= "Date", y= "Number of referrals", color = "") +
    theme_bw() + 
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 2))
  
  if(add_stats){
    msum <- summary(model)
    
    covid_imp <- msum$coefficients[rownames(msum$coefficients) == "covid_timeduring",]
    covid_imp[1] <- exp(covid_imp[1]) -1
    stat_text_1 <- paste0("Post-pandemic difference:\n",
                          round(covid_imp[1] * 100,2),
                          "%, (",
                          ifelse(covid_imp[4]<0.001,"p<0.001",
                                 paste0("p = ",round(covid_imp[4],3))),
                          ")")
    
    covid_imp <- msum$coefficients[rownames(msum$coefficients) == "covidTRUE",]
    covid_imp[1] <- exp(covid_imp[1]) -1
    stat_text_2 <- paste0("Overall COVID-19 impact:\n",
                          round(covid_imp[1] * 100,2),
                          "%, (",
                          ifelse(covid_imp[4]<0.001,"p<0.001",
                                 paste0("p = ",round(covid_imp[4],3))),
                          ")")
    
    covid_imp <- msum$coefficients[rownames(msum$coefficients) == "carerUnpaid carer",]
    covid_imp[1] <- exp(covid_imp[1]) -1
    stat_text_3 <- paste0("Pre-pandemic difference:\n",
                          round(covid_imp[1] * 100,2),
                          "%, (",
                          ifelse(covid_imp[4]<0.001,"p<0.001",
                                 paste0("p = ",round(covid_imp[4],3))),
                          ")")
    
    ypos = mean(layer_scales(p)$y$range$range)
    ypos = (ypos + layer_scales(p)$y$range$range[2]) /2
    ypos = (ypos + layer_scales(p)$y$range$range[2]) /2
    
    p <-  p + geom_text(aes(x=as.Date("2021-01-01"), y=ypos,label = stat_text_1)) + ggtitle(unique(data$subcat))
    p <-  p + geom_text(aes(x=as.Date("2019-01-01"), y=ypos,label = stat_text_2)) + ggtitle(unique(data$subcat))
    p <-  p + geom_text(aes(x=as.Date("2017-01-01"), y=ypos,label = stat_text_3)) + ggtitle(unique(data$subcat))
    
  }
  
  return(p)
}
run_ts_analysis2 <- function(dataset,Target_variable,tag,Seasonal,Linear=F,Add_Stats=F){
  
  dataset <- data.frame(dataset)
  output_list<-list()
  categories <- unique(dataset[,Target_variable])
  all_data <- data_frame()
  
  for(categ in categories){
    message(paste0("Currently calculating ",Target_variable,": ",categ,"\n"))
    
    data_n <-  paste0("data")
    model_n <- paste0("model")
    plot_n <-  paste0("plot")
    
    mdata = time_series_analysis_data(data = dataset, target_var = Target_variable,category = categ)
    mdata <- mdata %>% filter(subcat == categ) %>% mutate(total = sum(as.numeric(n)),
                                                          n = as.numeric(n),
                                                          month = month(date)) 
    mdata$covid_time <- mdata$Pandemic
    
    mdata <- mdata %>% filter(n>=7 & unique_patients >= 7)
    
    if(Linear){
      mmodel <- time_series_analysis_model2_lm(data = mdata,seasonal = Seasonal)
      mplot <- time_series_analysis_plot2_lm(data = mdata, model = mmodel, add_stats = Add_Stats)
    } else {
      mmodel <- time_series_analysis_model2(data = mdata,seasonal = Seasonal)
      mplot <- time_series_analysis_plot2(data = mdata, model = mmodel, add_stats = Add_Stats)
    }
    
    
    outs <- list(mdata,mmodel,mplot)
    names(outs) <- c(data_n,model_n,plot_n)
    
    output_list[[categ]] <- outs
    
  }
  
  return(output_list)
}


specs <- c(
  'CARDIOLOGY'
  ,"OPHTHALMOLOGY"
  ,'TRAUMA & ORTHOPAEDICS'
  ,'GYNAECOLOGY'
  ,'DIAGNOSTIC IMAGING'
  ,'ENT'
  ,'GENERAL SURGERY'
  ,'DERMATOLOGY'
  ,'MIDWIFE EPISODE'
  ,'RESPIRATORY MEDICINE'
  ,'UROLOGY'
  ,'GASTROENTEROLOGY'
  ,'GENERAL MEDICINE'
  ,'BREAST SURGERY'
  ,'NEUROLOGY'
  ,'PHYSIOTHERAPY'
  ,'COLORECTAL SURGERY'
  ,'VASCULAR SURGERY'
  ,'PAIN MANAGEMENT'
  ,'RHEUMATOLOGY'
)

# specs <- c('CARDIOLOGY'
#            ,"OPHTHALMOLOGY"
#            ,'TRAUMA & ORTHOPAEDICS'
#            ,'GYNAECOLOGY'
#            ,'DIAGNOSTIC IMAGING'
#            ,'ENT'
#            ,'GENERAL SURGERY'
#            ,'DERMATOLOGY'
#            ,'MIDWIFE EPISODE'
#            ,'RESPIRATORY MEDICINE'
#            ,'UROLOGY'
#            ,'GASTROENTEROLOGY'
#            ,'GENERAL MEDICINE'
#            ,'BREAST SURGERY'
#            ,'NEUROLOGY'
#            ,'PHYSIOTHERAPY'
#            ,'COLORECTAL SURGERY'
#            ,'VASCULAR SURGERY',
#            'PAIN MANAGEMENT'
#            ,'RHEUMATOLOGY')

tst_month <- model_mnth %>% filter(SpecialtyDescription %in% specs)
tst_quart <- model_qurt %>% filter(SpecialtyDescription %in% specs)

spec_month_seas_nb <- run_ts_analysis2(dataset = tst_month, Target_variable = 'SpecialtyDescription',
                                       tag = 'Speciality', Seasonal = T, Add_Stats = T)
spec_month_nsea_nb <- run_ts_analysis2(dataset = tst_month, Target_variable = 'SpecialtyDescription',
                                       tag = 'Speciality', Seasonal = F, Add_Stats = T)
spec_quart_seas_nb <- run_ts_analysis2(dataset = tst_quart, Target_variable = 'SpecialtyDescription',
                                       tag = 'Speciality', Seasonal = T, Add_Stats = T)
spec_quart_nsea_nb <- run_ts_analysis2(dataset = tst_quart, Target_variable = 'SpecialtyDescription',
                                       tag = 'Speciality', Seasonal = F, Add_Stats = T)

AIC(spec_month_seas_nb$`COLORECTAL SURGERY`$model)
AIC(spec_month_nsea_nb$`COLORECTAL SURGERY`$model)
AIC(spec_quart_nsea_nb$`COLORECTAL SURGERY`$model)
AIC(spec_quart_seas_nb$`COLORECTAL SURGERY`$model)

spec_month_nsea_nb$CARDIOLOGY$plot
spec_month_seas_nb$CARDIOLOGY$plot

# spec_month_seas_ln <- run_ts_analysis2(dataset = tst_month, Target_variable = 'SpecialtyDescription',
#                                     tag = 'Speciality', Seasonal = T, Add_Stats = T, Linear = T)
# spec_month_nsea_ln <- run_ts_analysis2(dataset = tst_month, Target_variable = 'SpecialtyDescription',
#                                     tag = 'Speciality', Seasonal = F, Add_Stats = T, Linear = T)
# spec_quart_seas_ln <- run_ts_analysis2(dataset = tst_quart, Target_variable = 'SpecialtyDescription',
#                                     tag = 'Speciality', Seasonal = T, Add_Stats = T, Linear = T)
# spec_quart_nsea_ln <- run_ts_analysis2(dataset = tst_quart, Target_variable = 'SpecialtyDescription',
#                                     tag = 'Speciality', Seasonal = F, Add_Stats = T, Linear = T)
#  
# AIC(spec_month_seas_ln$`COLORECTAL SURGERY`$model)
# AIC(spec_month_nsea_ln$`COLORECTAL SURGERY`$model)
# AIC(spec_quart_nsea_ln$`COLORECTAL SURGERY`$model)
# AIC(spec_quart_seas_ln$`COLORECTAL SURGERY`$model)
# 
# spec_month_nsea_ln$CARDIOLOGY$plot
# spec_month_seas_ln$CARDIOLOGY$plot


save_model_data <- function(mod=NULL, tab=NULL,is_lm=T,file){
  
  if(is_lm){
    summ <- broom.mixed::tidy(mod$model,conf.int=T,exponentiate=F) 
  } else {
    summ <-  broom.mixed::tidy(mod$model,conf.int=T,exponentiate=T)
  }
  
  #ME model####
  writeData(wb = wb
            ,sheet = tab
            ,x = "Model summary"
            ,startCol = 1
            ,startRow = 1
  )
  writeData(wb = wb
            ,sheet = tab
            ,x = summ
            ,startCol = 1
            ,startRow = 2
  )
  writeData(wb = wb
            ,sheet = tab
            ,x = "Model plot"
            ,startCol = 1
            ,startRow = 15
  )
  print(mod$plot)
  Sys.sleep(2)
  insertPlot(wb = wb
             ,sheet = tab
             ,startCol = 1
             ,startRow = 16
             ,width = 25
             ,height = 10
             ,units = 'cm'
             ,fileType = 'png'
  )
  saveWorkbook(wb,file,overwrite = T)
}


View(spec_quart_nsea_nb$`MIDWIFE EPISODE`$data)
# View(spec_quart_nsea_ln$`MIDWIFE EPISODE`$data)

# xx <- spec_quart_nsea_ln$`MIDWIFE EPISODE`$data %>% 
#   select(date,subcat,carer,n)
# fwrite(xx,'midwife_episode_counts.csv')

# Write model summary data
file = "outputs/referrals_models_nb.xlsx"
file.remove(file)
wb <- createWorkbook()
for(i in specs){
  addWorksheet(wb,i)
  md <- spec_quart_nsea_nb[[i]]
  save_model_data(mod = md,tab = i,is_lm = F,file)
}


# Write model data
file = "outputs/referrals_models_data.xlsx"
file.remove(file)
wb <- createWorkbook()
for(i in specs){
  addWorksheet(wb,i)
  md <- spec_quart_nsea_nb[[i]]$data
  writeData(wb = wb
            ,sheet = i
            ,x = md
            ,startCol = 1
            ,startRow = 1
  )
  saveWorkbook(wb,file,overwrite = T)
}

# file = "outputs/referrals_models_ln.xlsx"
# file.remove(file)
# wb <- createWorkbook()
# for(i in specs){
#   addWorksheet(wb,i)
#   md <- spec_quart_nsea_ln[[i]]
#   save_model_data(mod = md,tab = i,is_lm = F,file)
# }

