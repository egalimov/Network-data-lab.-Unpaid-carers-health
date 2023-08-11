# -*- coding: utf-8 -*-
"""
Created on Tue May 24 10:47:12 2022

@author: Evgeny Galimov
"""

import pandas as pd
from datetime import datetime
from datetime import date




path = 'S:/NDL/MouleshWorkings/'

# loading Carer cohort with carers related SNOMED codes 
data = pd.read_csv(path+'1_Carers.csv')

# loading Control cohort with carers related SNOMED codes excluded
oc_raw = pd.read_csv(path + '1_ControlPreMatched.csv')


### Checking missing data for control cohort
# missing data summary
miss_null = oc_raw.isnull()
miss_null_sum = miss_null.sum(axis=0)
miss_null_sum = pd.DataFrame({'col':miss_null_sum.index, 'null':miss_null_sum.values})

miss_na = oc_raw.isna()
miss_na_sum = miss_na.sum(axis=0)
miss_na_sum = pd.DataFrame({'col':miss_na_sum.index, 'na':miss_na_sum.values})

# get table with all columns missing data calculations
miss = pd.merge(miss_null_sum,miss_na_sum, on = ['col'])
# get table only with  columns with missing data 
miss_val = miss[ (miss['na']>0) | (miss['null']>0) ]

# drop missing values for 'IMDDecile', 'IMDRank', 'age', 'EthnicCategory'
oc_raw_c = oc_raw.dropna(subset = ['IMDDecile', 'IMDRank', 'age', 'EthnicCategory'])
oc_raw_c.index = range(len(oc_raw_c))

# shuffle the controls df
oc_raw_c = oc_raw_c.sample(frac=1, random_state=42).reset_index(drop=True)



### Checking missing data for carers cohort
desc = data.describe()

# missing data summary
miss_null = data.isnull()
miss_null_sum = miss_null.sum(axis=0)
miss_null_sum = pd.DataFrame({'col':miss_null_sum.index, 'null':miss_null_sum.values})

miss_na = data.isna()
miss_na_sum = miss_na.sum(axis=0)
miss_na_sum = pd.DataFrame({'col':miss_na_sum.index, 'na':miss_na_sum.values})

# get table with all columns missing data calculations
miss = pd.merge(miss_null_sum,miss_na_sum, on = ['col'])
# get table only with  columns with missing data 
miss_val = miss[ (miss['na']>0) | (miss['null']>0) ]

# drop missing values for 'IMDDecile', 'IMDRank', 'EthnicCategory'
data_c = data.dropna(subset = ['IMDDecile', 'IMDRank', 'EthnicCategory'])
data_c.index = range(len(data_c))



### Creating a dictionary which table with data for 1 carer and 1-5 matched controls

def get_controls(pk, oc_raw_c):
  #i=1
    #pk = 220
    t = data_c[data_c.PatientKey == pk]
    t.index = range(len(t))
    pg = t.loc[0, 'Gender']
    pa = t.loc[0, 'age']
    pe = t.loc[0, 'EthnicCategory']
    pimd = t.loc[0, 'IMDDecile']
    pimdr = t.loc[0, 'IMDRank']
   

    
    # get matches
    sub_t1 = oc_raw_c[(oc_raw_c.Gender == pg) & (oc_raw_c.age == pa) & (oc_raw_c.IMDDecile == pimd) & (oc_raw_c.EthnicCategory == pe)]
    sub_t2 = oc_raw_c[(oc_raw_c.Gender == pg) & (oc_raw_c.age == pa) & (oc_raw_c.IMDDecile == pimd) ]
    sub_t3 = oc_raw_c[(oc_raw_c.Gender == pg) & (oc_raw_c.age == pa) & (oc_raw_c.IMDDecile >= pimd-1) & (oc_raw_c.IMDDecile <= pimd+1) & (oc_raw_c.EthnicCategory == pe)]

    t1 = pd.DataFrame({'PatientKey':[],'age':[],'Gender':[], 'IMDRank':[],'IMDDecile':[],'EthnicCategory':[],'MatchedPatientKey':[]})
    
    

    if sub_t1.shape[0]>4: 
        t1 = sub_t1.sample(n=5, random_state = 10)
        t1 = t1[["PatientKey", "age","Gender", "IMDRank", "IMDDecile", "EthnicCategory"]]
        t1['MatchedPatientKey'] = pk

    if (sub_t1.shape[0]<=4) & (sub_t1.shape[0] > 0): 
        t1 = sub_t1.sample(n=sub_t1.shape[0], random_state = 10)
        t1 = t1[["PatientKey", "age","Gender", "IMDRank", "IMDDecile", "EthnicCategory"]]
        t1['MatchedPatientKey'] = pk  

    if (sub_t1.shape[0]==0): 
        if sub_t2.shape[0]>4: 
            t1 = sub_t2.sample(n=5, random_state = 10)
            t1 = t1[["PatientKey", "age","Gender", "IMDRank", "IMDDecile", "EthnicCategory"]]
            t1['MatchedPatientKey'] = pk

        if (sub_t2.shape[0]<=4) & (sub_t2.shape[0] > 0): 
            t1 = sub_t2.sample(n=sub_t2.shape[0], random_state = 10)
            t1 = t1[["PatientKey", "age","Gender", "IMDRank", "IMDDecile", "EthnicCategory"]]
            t1['MatchedPatientKey'] = pk  
        if (sub_t2.shape[0] == 0): 
            if sub_t3.shape[0]>4: 
                t1 = sub_t3.sample(n=5, random_state = 10)
                t1 = t1[["PatientKey", "age","Gender", "IMDRank", "IMDDecile", "EthnicCategory"]]
                t1['MatchedPatientKey'] = pk
    
            if (sub_t3.shape[0]<=4) & (sub_t3.shape[0] > 0): 
                t1 = sub_t3.sample(n=sub_t3.shape[0], random_state = 10)
                t1 = t1[["PatientKey", "age","Gender", "IMDRank", "IMDDecile", "EthnicCategory"]]
                t1['MatchedPatientKey'] = pk  
            if (sub_t3.shape[0] == 0): 
                print(pk)
                print('t3 = 0')

    init = pd.DataFrame({'PatientKey':[pk],'age':[pa],'Gender':[pg], 'IMDRank':[pimdr],'IMDDecile':[pimd],'EthnicCategory':[pe],'MatchedPatientKey':['carer']})
    init = pd.concat([init, t1], axis = 0, sort = False)
   
    oc_raw_c = oc_raw_c[~oc_raw_c.PatientKey.isin(t1.PatientKey)] 
   # dict_res[pk] = init
    #res = pd.concat([res, init], axis = 0, sort = False)

    return(init, oc_raw_c)


# converting column types to categories and integers to accelerate filtering (10x acceleration)
oc_raw_c['Gender'] = oc_raw_c.Gender.astype('category')
oc_raw_c['EthnicCategory'] = oc_raw_c.EthnicCategory.astype('category')
oc_raw_c['age'] = oc_raw_c.age.astype('int64')
oc_raw_c['IMDDecile'] = oc_raw_c.IMDDecile.astype('int64')

# create a dictionary with carer patient keys both as keys and values (to iterate through the disctionary later) 
data_c_2 = data_c
data_c_2['PatientKey_ind'] = data_c_2['PatientKey']
data_c_2 = data_c_2[['PatientKey','PatientKey_ind']]
dict_res = data_c_2[['PatientKey','PatientKey_ind']].set_index('PatientKey_ind').T.to_dict('records')
dict_res = dict_res[0]

# create a dictionary where for each carer's patients key we have df with carer and matched controls obtained using get_controls function
dict_res2 = {}
count = 1
for k, v in dict_res.items():
    
    temp, oc_raw_c = get_controls(v, oc_raw_c)
    dict_res2[k] = temp

    count+=1


# combine all dfs from dict_res2 for each carer into one df
res_df = pd.DataFrame(columns = ['PatientKey', 'age', 'Gender', 'IMDRank', 'IMDDecile', 'EthnicCategory','MatchedPatientKey'])
# cycle over the created dictionary to create the full resulting df
for key, item in dict_res2.items():
   res_df = pd.concat([res_df, item]) 


# save the final df
res_df.to_csv(path+ str(date.today().strftime('%y%m%d')) +'_carers_and_matched_final_table.csv')








