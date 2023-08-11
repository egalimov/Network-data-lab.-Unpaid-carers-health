library(jtools)
library(MASS)
library(car)
library(tidyr)
library(dplyr)
library(tidyverse)
library(rcompanion)
library(car)
library(fitdistrplus)
library(AER)

#load raw OP dataset
d_raw = read.csv('S://NDL/MouleshWorkings/3_HC_access/AnE/AnE_adj_fin_agecat.csv')

# load age_gender_OP_imd datset
d = read.csv('S://NDL/MouleshWorkings/3_HC_access/AnE/groups_count.csv')

path = 'S://NDL/MouleshWorkings/3_HC_access/AnE/'

colnames(d)[6] = 'visit_counts'
d$counts_per_group = d$visit_counts/d$pop
d$carer = as.factor(d$carer)
d$carer = relevel(d$carer, ref = 'control')
d$Gender = as.factor(d$Gender)
d$IMDDecile = as.numeric(d$IMDDecile)
d$age_cat = as.factor(d$age_cat)

summary(d$age_cat)


d_raw$carer = as.factor(d_raw$carer)
d_raw$carer = relevel(d_raw$carer, ref = 'control')

d_raw$carer = as.factor(d_raw$carer)
d_raw$Gender = as.factor(d_raw$Gender)
d_raw$IMDDecile = as.numeric(d_raw$IMDDecile)
d_raw$age_cat = as.factor(d_raw$age_cat)


d_raw$Appts = 1



data=d_raw
c1='carer'
c2='Gender'
values = 'Appts'
corr_long_table_cat = function(data, c1,c2, values){
  e=0
  a = data %>% pivot_wider(
    names_from = c1, c2,  
    values_from = values,
    values_fn = sum)
  
  a_n = data[!duplicated(data$Patient_DerivedKey),] %>% pivot_wider(
    names_from = c1, c2, 
    values_from = values,
    values_fn = sum)
  
  # get the counts scaled to the population of each group
  a_res = a[,-1]/a_n[,-1]
  a_res = as.data.frame(a_res)
  rownames(a_res) = as.data.frame(a)[,1]
  if (sum(is.na(a_res))>0) {e=1}
  # get CramerV and chisq p
  if (e == 1){return ('missing values')}
  
  if (e == 0){
    chisq_p = chisq.test(as.matrix(a_res))$p.value      
    craverV = cramerV(as.matrix(a_res))
    return(c(craverV, chisq_p))}
  
}
# first var cat binary and second - cont
corr_long_table_cat.b_cont = function(data, c1,c2){
  t1 = ifelse(data[,c1]==levels( as.factor( data[,c1] )  )[1], 0, 1)
  return(  cor( t1, data[,c2], method = c('pearson') ) )
}
# first var cat_ord(>2, recode no numbers before applying)  and second - cont

corr_long_table_cat.nb_cont = function(data, c1,c2){
  return(  cor( data[,c1], data[,c2], method = c('spearman') ) )
}
# corr: both cont
corr_long_table_cont = function(data, c1,c2){
  return(  cor( data[,c1], data[,c2], method = c('pearson') ) )
}
#####
# type1 = cont / cat_org / cat_bin /cat
# type2 = cont / cat
cor_master = function(data,c1,type1,c2,type2, value_for_cat_corr){
  if ((type1 =='cont')&(type2 =='cont')){
    out = corr_long_table_cont(data,c1,c2) }
  if ((type1 =='cat_bin')&(type2 =='cont')){
    out = corr_long_table_cat.b_cont(data,c1,c2)
  }
  if ((type1 =='cat_ord')&(type2 =='cont')){
    out = corr_long_table_cat.nb_cont(data,c1,c2) 
  }
  # when there are cat > 2 and not ordinal
  if ((type1 =='cat_not_ord')&(type2 =='cont')){
    out = corr_long_table_cat(data,c1,c2,values)[1] 
  }
  if ((type1 =='cat')&(type2 =='cat')){
    out = corr_long_table_cat(data,c1,c2,values)[1]
  }
  
  return(out)
}




df = data.frame( Confounder = c('age_cat','Gender','IMDDecile'), 
                 Correlation = c('Cramer V','Cramer V','Point biserial corr'),
                 value = c(cor_master(d_raw,'carer','cat','age_cat','cat', 'Appts'),
                           cor_master(d_raw,'carer','cat','Gender','cat', 'Appts'),
                           -cor_master(d_raw,'carer','cat_bin','IMDDecile','cont', 'Appts')    ) )
write.csv(df, paste(path, '3_corr_carer_vs_others.csv',sep=''))


p5 = ggplot(df, aes(Confounder,Correlation,fill = value))+geom_tile()+
  scale_fill_gradient(low='blue',high='red',limits=c(0,1))+ggtitle('Carer')

png(paste(path, '3_corr_carer_confounders.png', sep=''), width = 15, height = 10, units = 'cm', res = 200  )
print(p5)
dev.off()







## print whole dataset correlations
p1 = ggpairs(d[, c("counts_per_group", "carer", "age_cat", 'Gender', 'IMDDecile')], lower = list(continuous = "smooth"),
             diag = list(continuous = "density"), axisLabels = "show")
png( paste(path,'1_var_inter.png',sep=''), width = 15, height = 10, units = 'cm', res = 200  )
print(p1)
dev.off() 

# plots for controls/carer
p1 = ggplot(d, aes(x = carer, y = counts_per_group)) + stat_summary(fun = 'sum', geom = 'bar') 
p2 = ggplot(d, aes(x = carer, y = counts_per_group)) + stat_summary(fun = 'sum', geom = 'bar') + facet_wrap(~Gender)
p3 = ggplot(d, aes(x = carer, y = counts_per_group)) + stat_summary(fun = 'sum', geom = 'bar') + facet_wrap(~age_cat)
p4 = ggplot(d, aes(x = carer, y = counts_per_group)) + stat_summary(fun = 'sum', geom = 'bar') + facet_wrap(~IMDDecile)

png(paste(path,'2_carer_barplot.png',sep=''), width = 15, height = 10, units = 'cm', res = 200  )
print(p1)
dev.off() 
png(paste(path,'2_carer_gender_barplot.png',sep=''), width = 15, height = 10, units = 'cm', res = 200  )
print(p2)
dev.off() 
png(paste(path,'2_carer_age_barplot.png',sep=''), width = 15, height = 10, units = 'cm', res = 200  )
print(p3)
dev.off() 
png(paste(path,'2_carer_imd_barplot.png',sep=''), width = 15, height = 10, units = 'cm', res = 200  )
print(p4)
dev.off() 


vif_res = as.data.frame(  vif(lm(visit_counts ~ carer + age_cat + Gender + IMDDecile, d))   )
write.csv(vif_res, paste(path,'4_VIF.csv'), sep='')

# check the plot
descdist(d$visit_counts, discrete = TRUE)
dist = recordPlot()
png(paste(path, '4_Cullen_frey_graph.png'), width = 15, height = 10, units = 'cm', res = 200  )
print(dist)
dev.off() 


x = d$visit_counts
png(paste(path, '5_Y_distribution.png'), width = 15, height = 10, units = 'cm', res = 200  )
hist(x)
dev.off() 


op.nb_d <- fitdist(x, "nbinom")
summary(op.nb_d)
plot(op.nb_d)
a = recordPlot()
png(paste(path, '6_nbinom_fit.png', sep=''), width = 15, height = 10, units = 'cm', res = 200  )
print(a)
dev.off()




run_and_save_the_model_nb = function(data, path, form_str, model_name){
  
  if (!file.exists(path)) {dir.create(path)}
  op.nb2 = glm.nb(form_str, data = data)
  sink(file = paste(path,model_name,'_summary.txt', sep = ''))
 # print( summary() )
  print( summ(op.nb2, exp=T, confint=TRUE, digits = 5, ciwidth=0.05 ) )

  sink()
  
  plot(fitted(op.nb2), residuals(op.nb2, type='pearson'))
  p0 = recordPlot()
  png(paste(path,model_name,'_Pearson.res_vs_fit.png', set=''), width = 15, height = 10, units = 'cm', res = 200  )
  print(p0)
  dev.off() 
  
  
  plot(op.nb2, 1)
  p1 = recordPlot()
  plot(op.nb2, 2)   # Std Pearsons QQ
  p2 = recordPlot()
  plot(op.nb2, 3)   # Std Pearsons Scale location
  p3 = recordPlot()
  plot(op.nb2, 5)   # Std Pearsons leverage
  p5 = recordPlot()
  crPlots(op.nb2)
  p6 = recordPlot()
  
  hist(op.nb2$residuals)
  p7 = recordPlot()
  
  
  png(paste(path,model_name,'_res_vs_fit.png', set=''), width = 15, height = 10, units = 'cm', res = 200  )
  print(p1)
  dev.off() 
  
  png(paste(path,model_name,'_Pearson_qq.png', set=''), width = 15, height = 10, units = 'cm', res = 200  )
  print(p2)
  dev.off() 
  
  png(paste(path,model_name,'_Pearson_var_vs_location.png', set=''), width = 15, height = 10, units = 'cm', res = 200  )
  print(p3)
  dev.off() 
  
  png(paste(path,model_name,'_Pearson_leverage.png', set=''), width = 15, height = 10, units = 'cm', res = 200  )
  print(p5)
  dev.off() 
  
  png(paste(path,model_name,'_partial_plots.png', set=''), width = 15, height = 10, units = 'cm', res = 200  )
  print(p6)
  dev.off() 
  
  png(paste(path,model_name,'_resid_hist.png', set=''), width = 15, height = 10, units = 'cm', res = 200  )
  print(p7)
  dev.off() 
  
}  


########################################
#op = read.csv('S://NDL/MouleshWorkings/3_HC_access/OP/groups_count.csv')
form_str = as.formula('visit_counts ~ carer + age_cat + Gender + IMDDecile + offset(log(pop))')
data = d
model_name = '9_nb_age_cat'
#run_and_save_the_model_logr(efi, efi$efi15, path, form_str, '6_logr_efi15')

run_and_save_the_model_nb(data, path, form_str, model_name)


####
# cont age
d = read.csv('S://NDL/MouleshWorkings/3_HC_access/AnE/group_count_cont.csv')

path = 'S://NDL/MouleshWorkings/3_HC_access/AnE/'

colnames(d)[6] = 'visit_counts'
d$counts_per_group = d$visit_counts/d$pop
d$carer = as.factor(d$carer)
d$carer = relevel(d$carer, ref = 'control')
d$Gender = as.factor(d$Gender)
d$IMDDecile = as.numeric(d$IMDDecile)
#d$age_cat = as.factor(d$age_cat)

summary(d$age_cat)


form_str = as.formula('visit_counts ~ carer + age + Gender + IMDDecile + offset(log(pop))')
data = ds
model_name = '9_nb_age_cont'

run_and_save_the_model_nb(data, path, form_str, model_name)

