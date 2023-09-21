library(tidyverse)
library(readxl) 
library(epitools)
library(data.table)

`%nin%` <- Negate(`%in%`)

#Read in dpopulation data####
file_list <- list.files("ccgpop/",full.names = T)
file_list

read_pop_files <-function(files = file_list, type,Skip){
  
  merged_output <- data_frame()
  
  for(file in files){
    message(file)
    year <- str_extract(file,"[0-9]{4}")
    tab <- paste0("Mid-",year," ",type)
    
    output <- read_excel(
      path = file,
      sheet = tab,
      skip = Skip) %>%
      mutate(type=type,year=year)
    
    merged_output <- bind_rows(output,merged_output)
  }
  merged_output <- merged_output %>%
    mutate(AreaCode = ifelse(is.na(`CCG Code`),`Area Codes`,`CCG Code`),
           AreaName = ifelse(is.na(`CCG Name`),`Area Names`,`CCG Name`)
    ) %>%
    select_at(c("AreaCode","AreaName","year","type","All Ages",1:89,"90+")) %>%
    filter(!is.na(`All Ages`)) %>% filter(!is.na(AreaName))
  
  return(merged_output)
}

male_pop <- read_pop_files(files = file_list,type = "Males",Skip = 5)
female_pop <- read_pop_files(files = file_list,type = "Females",Skip = 5)
person_pop <- read_pop_files(files = file_list,type = "Persons",Skip = 5)


#Read in wsic data####
# data <- read_excel("../SP/LTCs Extraction 29_04_2022.xlsx")
# data <- fread("../EG/2_Unpaid_carers_cohort/control_cohort_not_imputed_1_37944.csv")
# data <- fread("../EG/2_Unpaid_carers_cohort/220526_matched_cohort.csv")
# data <- fread("../1_2__data_extraction_and_preparation/220920_carers_and_matched_final_table_LTCs_first_date_adjusted.csv")
data <- fread('../../../../MouleshWorkings/Extracts2/0_final_table_LTCs_first_date_adjusted2.csv')
data <- data %>% filter(age >=18)

# ltc_ref <- fread("../1_2__data_extraction_and_preparation/220908_carers_and_matched_final_table_LTCs.csv")
# colnames(ltc_ref) <- make.unique(colnames(ltc_ref))
# ltc_ref <- ltc_ref %>% select(-MatchedPatientKey) %>% unique()

# ltc_ref <- read_excel("../SP/LTCs Extraction 29_04_2022.xlsx")
# data <- left_join(data,ltc_ref,  by = c("Gender", "IMDRank", "IMDDecile","age", "EthnicCategory", "PatientKey"))

#Run analysis with 2_unpaid_carers_cohort > control_cohort_not_imputed_1_10000

data <- data %>%
  mutate(age_group = case_when(
    between(age,0,19) ~ "0-19",
    between(age,20,29) ~ "20-29",
    between(age,30,39) ~ "30-39",
    between(age,40,49) ~ "40-49",
    between(age,50,59) ~ "50-59",
    between(age,60,69) ~ "60-69",
    between(age,70,79) ~ "70-79",
    age >= 80 ~ "80+"
  ))

#Calculate yearly populations####
nwl_ccgs <- c(
  "E38000020" #Brent
  ,"E38000031" #Central London
  ,"E38000048" #Ealing
  ,"E38000070" #Hammersmith
  ,"E38000074" #Harrow
  ,"E38000082" #Hillingdon
  ,"E38000084" #Hounslow
  ,"E38000202" #West London
  ,"E38000256" #NW london
)

#Total general populations
total_genpop <- person_pop %>% group_by(year) %>%
  summarise(total_pop = sum(`All Ages`)) %>% mutate(Gender="all")

total_nwlpop <- person_pop %>% filter(AreaCode %in% nwl_ccgs) %>% 
  group_by(year) %>% summarise(nwl_pop = sum(`All Ages`)) %>% mutate(Gender='all')

#Total male population
total_genmale <- male_pop %>% group_by(year) %>%
  summarise(total_pop = sum(`All Ages`)) %>% mutate(Gender="Male")

total_nwlmale <- male_pop %>% filter(AreaCode %in% nwl_ccgs) %>% 
  group_by(year) %>% summarise(nwl_pop = sum(`All Ages`)) %>% mutate(Gender="Male")

#Total female population
total_genfema <- female_pop %>% group_by(year) %>%
  summarise(total_pop = sum(`All Ages`)) %>% mutate(Gender="Female")
total_nwlfema <- female_pop %>% filter(AreaCode %in% nwl_ccgs) %>% 
  group_by(year) %>% summarise(nwl_pop = sum(`All Ages`)) %>% mutate(Gender="Female")

total_populations <- left_join(
  bind_rows(total_genpop,total_genmale,total_genfema),
  bind_rows(total_nwlpop,total_nwlmale,total_nwlfema),
  by = c("year", "Gender")
)
rm(total_genpop,total_genmale,total_genfema,total_nwlpop,total_nwlmale,total_nwlfema)

#Total population by year group
year_genpop <- person_pop %>%
  pivot_longer(cols = all_of(c(1:89,"90+")),names_to = "Age",values_to = 'counts') %>%
  mutate(age_group = case_when(
    between(Age,0,19) ~ "0-19",
    between(Age,20,29) ~ "20-29",
    between(Age,30,39) ~ "30-39",
    between(Age,40,49) ~ "40-49",
    between(Age,50,59) ~ "50-59",
    between(Age,60,69) ~ "60-69",
    between(Age,70,79) ~ "70-79",
    Age >= 80 ~ "80+"
  )) %>% group_by(year,age_group) %>% summarise(total_pop = sum(counts)) %>%
  mutate(Gender='all')

year_nwlpop <- person_pop %>%
  filter(AreaCode %in% nwl_ccgs) %>%
  pivot_longer(cols = all_of(c(1:89,"90+")),names_to = "Age",values_to = 'counts') %>%
  mutate(age_group = case_when(
    between(Age,0,19) ~ "0-19",
    between(Age,20,29) ~ "20-29",
    between(Age,30,39) ~ "30-39",
    between(Age,40,49) ~ "40-49",
    between(Age,50,59) ~ "50-59",
    between(Age,60,69) ~ "60-69",
    between(Age,70,79) ~ "70-79",
    Age >= 80 ~ "80+"
  )) %>% group_by(year,age_group) %>% summarise(nwl_pop = sum(counts)) %>%
  mutate(Gender='all')


#Male population by year
year_genmale <- male_pop %>%
  pivot_longer(cols = all_of(c(1:89,"90+")),names_to = "Age",values_to = 'counts') %>%
  mutate(age_group = case_when(
    between(Age,0,19) ~ "0-19",
    between(Age,20,29) ~ "20-29",
    between(Age,30,39) ~ "30-39",
    between(Age,40,49) ~ "40-49",
    between(Age,50,59) ~ "50-59",
    between(Age,60,69) ~ "60-69",
    between(Age,70,79) ~ "70-79",
    Age >= 80 ~ "80+"
  )) %>% group_by(year,age_group) %>% summarise(total_pop = sum(counts)) %>%
  mutate(Gender="Male")

year_nwlmale <- male_pop %>%
  filter(AreaCode %in% nwl_ccgs) %>%
  pivot_longer(cols = all_of(c(1:89,"90+")),names_to = "Age",values_to = 'counts') %>%
  mutate(age_group = case_when(
    between(Age,0,19) ~ "0-19",
    between(Age,20,29) ~ "20-29",
    between(Age,30,39) ~ "30-39",
    between(Age,40,49) ~ "40-49",
    between(Age,50,59) ~ "50-59",
    between(Age,60,69) ~ "60-69",
    between(Age,70,79) ~ "70-79",
    Age >= 80 ~ "80+"
  )) %>% group_by(year,age_group) %>% summarise(nwl_pop = sum(counts)) %>%
  mutate(Gender="Male")

#Female populations by year
year_genfema <- female_pop %>%
  pivot_longer(cols = all_of(c(1:89,"90+")),names_to = "Age",values_to = 'counts') %>%
  mutate(age_group = case_when(
    between(Age,0,19) ~ "0-19",
    between(Age,20,29) ~ "20-29",
    between(Age,30,39) ~ "30-39",
    between(Age,40,49) ~ "40-49",
    between(Age,50,59) ~ "50-59",
    between(Age,60,69) ~ "60-69",
    between(Age,70,79) ~ "70-79",
    Age >= 80 ~ "80+"
  )) %>% group_by(year,age_group) %>% summarise(total_pop = sum(counts)) %>%
  mutate(Gender="Female")

year_nwlfema <- female_pop %>%
  filter(AreaCode %in% nwl_ccgs) %>%
  pivot_longer(cols = all_of(c(1:89,"90+")),names_to = "Age",values_to = 'counts') %>%
  mutate(age_group = case_when(
    between(Age,0,19) ~ "0-19",
    between(Age,20,29) ~ "20-29",
    between(Age,30,39) ~ "30-39",
    between(Age,40,49) ~ "40-49",
    between(Age,50,59) ~ "50-59",
    between(Age,60,69) ~ "60-69",
    between(Age,70,79) ~ "70-79",
    Age >= 80 ~ "80+"
  )) %>% group_by(year,age_group) %>% summarise(nwl_pop = sum(counts)) %>%
  mutate(Gender="Female")

year_populations <- left_join(
  bind_rows(year_genpop,year_genmale,year_genfema),
  bind_rows(year_nwlpop,year_nwlmale,year_nwlfema),
  by = c("year", "Gender","age_group")
)
rm(year_genpop,year_genmale,year_genfema,year_nwlpop,year_nwlmale,year_nwlfema)


#Calculate counts of LTCs####

ltcs <- c( "Asthma",
           "AtrialFibrillation",
           "Cancer",
           "CKD",
           "Anxiety",
           "COPD",
           "IschaemicHeartDisease",
           "Dementia",
           "Depression",
           "Diabetes",
           "Hypertension",
           "StrokeTIA",
           "Epilepsy",
           "HeartFailure",
           "LearningDisability",
           "MultipleSclerosis",
           "ParkinsonsDisease",
           "RheumatoidArthritis",
           "MentalHealth"
           )

data <- data %>% mutate(
  "Asthma" = Date.of.Diagnosis.Asthma_flag == 1,
  "AtrialFibrillation" = Date.of.Diagnosis.AtrialFibrillation_flag == 1,
  "Cancer" = Date.of.Diagnosis.Cancer_flag == 1,
  "CKD" = Date.of.Diagnosis.CKD_flag == 1,
  "Anxiety" = Date.of.Diagnosis.Anxiety_flag == 1,
  "COPD" = Date.of.Diagnosis.COPD_flag == 1,
  "IschaemicHeartDisease" = Date.of.Diagnosis.IschaemicHeartDisease_flag == 1,
  "Dementia" = Date.of.Diagnosis.Dementia_flag == 1,
  "Depression" = Date.of.Diagnosis.Depression_flag == 1,
  "Diabetes" = Date.of.Diagnosis.Diabetes_flag == 1,
  "Hypertension" = Date.of.Diagnosis.Hypertension_flag == 1,
  "StrokeTIA" = Date.of.Diagnosis.StrokeTIA_flag == 1,
  "Epilepsy" = Date.of.Diagnosis.Epilepsy_flag == 1,
  "HeartFailure" = Date.of.Diagnosis.HeartFailure_flag == 1,
  "LearningDisability" = Date.of.Diagnosis.LearningDisability_flag_flag == 1,
  "MultipleSclerosis" = Date.of.Diagnosis.MultipleSclerosis_flag == 1,
  "ParkinsonsDisease" = Date.of.Diagnosis.ParkinsonsDisease_flag == 1,
  "RheumatoidArthritis" = Date.of.Diagnosis.RheumatoidArthritis_flag == 1,
  "MentalHealth" = Date.of.Diagnosis.MentalHealth_flag == 1
)

tmp <- data.frame(data)

data$`No LTCs` <-  rowSums(tmp[,ltcs],na.rm = T) == 0
data$`1 LTCs` <- rowSums(tmp[,ltcs],na.rm = T) == 1
data$`2 LTCs` <- rowSums(tmp[,ltcs],na.rm = T) == 2
data$`3 LTCs` <- rowSums(tmp[,ltcs],na.rm = T) == 3
data$`3+ LTCs` <- rowSums(tmp[,ltcs],na.rm = T) >= 3
data$`4+ LTCs` <- rowSums(tmp[,ltcs],na.rm = T) >= 4


ltcs <- c(ltcs,'No LTCs','1 LTCs','2 LTCs','3 LTCs','3+ LTCs','4+ LTCs')

ltcs_data <- data %>% 
  mutate(Carer = ifelse(MatchedPatientKey == "carer","carer","not carer")) %>%
  select(-c("age","PatientKey","IMDRank","IMDDecile","EthnicCategory","MatchedPatientKey")) %>%
  mutate_at(.vars = all_of(ltcs), .funs = function(x) as.numeric(x)) %>%
  pivot_longer(cols = all_of(ltcs),names_to = "LTCs",values_to = "value") %>%
  mutate(value = ifelse(is.na(value),0,value)) %>%
  filter(Gender %in% c("Male","Female"))

ltcs_pops <- data %>%
  mutate(Carer = ifelse(MatchedPatientKey == "carer","carer","not carer")) %>%
  group_by(Gender,age_group,Carer) %>% summarise(carer_pop = n())

ltcs_AgeGender <- ltcs_data %>%
  group_by(Gender,age_group,LTCs) %>% summarise(total=sum(value))
ltcs_AgeGenderCarer <- ltcs_data %>%
  group_by(Gender,Carer,age_group,LTCs) %>% summarise(total=sum(value))

testpop <- ltcs_AgeGenderCarer 

per = 100000

dsr_data <- testpop %>% 
  left_join(year_populations %>% filter(year == "2020"), by = c("age_group","Gender")) %>%
  left_join(ltcs_pops, by = c("age_group","Gender","Carer")) %>%
  mutate(nwlrateper = (total/carer_pop) * per) %>%
  group_by(LTCs) %>%
  mutate(TotalCounts = sum(total)) %>%
  filter(TotalCounts > 20) %>%
  mutate(expexcted = (nwlrateper * total_pop)/per)


calculate_ltcs_dsr <- function(dsr_data,Per=per,adjust_p=T){
  require(epitools)
  
  output <- data.frame()
  
  for(ltc in unique(dsr_data$LTCs)){
    message(ltc)
    dsr_data_sub <- dsr_data %>% filter(LTCs == ltc)
    subdata_carer <-  dsr_data_sub %>% filter(Carer == "carer")
    subdata_noncarer <- dsr_data_sub %>% filter(Carer == "not carer")
    
    ds_rate_carer <- ageadjust.direct(
      count = subdata_carer$total,
      pop = subdata_carer$carer_pop,
      rate = subdata_carer$nwlrateper,
      stdpop = subdata_carer$total_pop,
      conf.level = 0.95) %>% t() %>% data.frame() %>%
      mutate(Carer = 'carer')
    
    ds_rate_notcarer <- ageadjust.direct(
      count = subdata_noncarer$total,
      pop = subdata_noncarer$carer_pop,
      rate = subdata_noncarer$nwlrateper,
      stdpop = subdata_noncarer$total_pop,
      conf.level = 0.95) %>% t() %>% data.frame() %>%
      mutate(Carer = 'not carer')
    
    dsr_rates <- bind_rows(ds_rate_notcarer,ds_rate_carer) %>% 
      mutate(LTCs = ltc)
    
    rr_noncare <- c(sum(subdata_noncarer$total),sum(subdata_noncarer$carer_pop))
    rr_care <- c(sum(subdata_carer$total),sum(subdata_carer$carer_pop))
    rr_tab <- rbind(rr_noncare,rr_care)
    
    rr_output <- rateratio(rr_tab,method = 'wald',conf.level = 0.95)
    
    dsr_rates$rate_ratio <- rr_output$measure[,1]
    dsr_rates$rate_ratio_upr <- rr_output$measure[,3]
    dsr_rates$rate_ratio_lwr <- rr_output$measure[,2]
    dsr_rates$rate_ratio_pval <- rr_output$p.value[,2]
    
    output <- bind_rows(output,dsr_rates)
    
    if(adjust_p){
      output$rate_ratio_pval_adjust <- p.adjust(output$rate_ratio_pval,method = "fdr")
    }
    
  }
  return(output)
}

testing <- calculate_ltcs_dsr(dsr_data)
testing
data.table::fwrite(testing,"outputs/AgeSexDSR_LTCs.csv")


###Plot

pdat <- testing %>%
  mutate(
    Carer = ifelse(Carer == "carer", "Unpaid carer", "Not carer"),
    rate_ratio = ifelse(Carer == "Not carer", NA, rate_ratio),
    pvalcol = ifelse(
      rate_ratio_pval_adjust < 0.05,
      'Significant',
      'Not significant'
    ),
    LTCs = case_when(
      LTCs == "Anxiety" ~ "Anxiety",
      LTCs == "Asthma" ~ "Asthma",
      LTCs == "AtrialFibrillation" ~ "Atrial fibrillation",
      LTCs == "Cancer" ~ "Cancer",
      LTCs == "CKD" ~ "Chronic Kidney Disease",
      LTCs == "COPD" ~ "COPD",
      LTCs == "Dementia" ~ "Dementia",
      LTCs == "Depression" ~ "Depression",
      LTCs == "Diabetes" ~ "Diabetes",
      LTCs == "Epilepsy" ~ "Epilepsy",
      LTCs == "HeartFailure" ~ "Heart failure",
      LTCs == "Hypertension" ~ "Hypertension",
      LTCs == "IschaemicHeartDisease" ~ "Ischaemic heart disease",
      LTCs == "LearningDisability" ~ "Learning disability",
      LTCs == "MentalHealth" ~ "Mental health",
      LTCs == "MultipleSclerosis" ~ "Multiple sclerosis",
      LTCs == "ParkinsonsDisease" ~ "Parkinsons disease",
      LTCs == "RheumatoidArthritis" ~ "Rheumatoid arthritis",
      LTCs == "StrokeTIA" ~ "Stroke / Transient ischaemic attack"
    )
  )

pdat %>% filter(Carer == "Unpaid carer") %>% arrange(rate_ratio) %>% select(LTCs) -> pdat_ord
pdat$LTCs <- factor(pdat$LTCs, pdat_ord$LTCs)

coeff <- 0.0003

ggplot(pdat) + 
  geom_bar(aes(x=LTCs, y=adj.rate, fill=Carer), stat='identity', position = 'dodge') +
  geom_pointrange(aes(x=LTCs, y=rate_ratio/coeff, ymin = rate_ratio_lwr/coeff, ymax = rate_ratio_upr/coeff, color = pvalcol)) +
  scale_y_continuous(
    sec.axis = sec_axis(~.*coeff, name = "Age-sex standardised rate ratio")
  ) + 
  labs(
    x = 'Long term conditions',
    y = 'Age-sex standardised rate\nper 100,000 population',
    fill = 'Carer status',
    color = 'Statistical difference'
  ) + 
  scale_fill_manual(values = c("cornflowerblue","sandybrown")) +
  scale_color_manual(
    values = c("grey60","springgreen4"),
    breaks = c("Not significant","Significant"),
    na.value = NA
  ) +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 90, hjust =1)
  )

ggsave(filename = "outputs/LTC_ASSR.png",width = 10,height = 5)

