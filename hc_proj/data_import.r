#Clearing the workspace
rm (list = ls())

#Load Libraries
library(here)
library(readxl)
library(tidyverse)
library(readr)
library(dplyr)
library(data.table)
libary(sqldf)

#Set file path from this file
here()

#****LOAD OLDER FORMAT FILES****

#The folder "excel_claims/pre-2016" houses older format schedules before the system upgrade
#2015 Data is still missing, hence skipped

#list all .xlsx files in the folder with older format schedules
temp = list.files("./excel_claims/pre-2016", pattern = "*.xlsx", full.names = TRUE)

#combine all the files into a list of dfs named old files
old_files = lapply(temp, read_xlsx)

#combine the list of dfs into one long list, view and store it
comb_old = bind_rows(old_files)
view(head(comb_old))
write_csv(comb_old, "./output/pre2016data.csv",append = FALSE, col_names = TRUE)

#remove the old files
rm (list = ls())

#****LOAD NEWER FORMAT FILES****
#The folder "excel_claims/2016+" houses newer format schedules after the system upgrade

#2016 files
#list all .xlsx files in the folder with newer format schedules
temp = list.files("./excel_claims/2016+", pattern = "*.xlsx", full.names = TRUE)

#combine all the files into a list of dfs named old files
new_files = lapply(temp, read_xlsx)

#combine the list of dfs into one long list
comb_new1 = data.table::rbindlist(new_files, use.names = TRUE, fill = TRUE)

#remove #NA rows (ones with totals and the likes)
comb_new = drop_na(comb_new1,`MEMBER NO`)
view(head(comb_new))
write_csv(comb_new, "./output/post2015data.csv",append = FALSE, col_names = TRUE)

#remove the old files
rm (list = ls())

#***COMBINE THE TWO SETS
#Load the first data set (based on values from the old system)
set1 = read.csv("./output/pre2016data.csv")

#Select only specific columns
df_old = sqldf("select 'Plan Code' as plan_code,'Claim Number' as clm_num,'Member Number' as mem_num, 'Beneficiary Number' as ben_num,'Practice Type' as dis,'Date Received' as date_received,'Treatment Date' as treatment_date,'Process Date' as assessment_date,'Account Amount' as amount_claimed,'Paid Amount' as amount_paid from set1")

#Load the second data set (based on values from the new system)
set2a = read.csv("./output/post2015data.csv")

#New system does not provide the total paid column, this section combines the two settlements paid from savings and risk amounts together
set2 = dplyr::mutate(set2a, amount_paid = set2a$PAID.FROM.RISK.AMT + set2a$PAID.FROM.SAVINGS)

#Select only specific columns needed
df_new = sqldf("select 'OPTION NAME' as plan_code,'CLAIM NO' as clm_num,'MEMBER NO' as mem_num, 'INO' as ben_num,'DIS' as dis,'DATE RECEIVED' as date_received,'SERVICE DATE' as treatment_date,'ASSESS DATE' as assessment_date,'AMOUNT CLAIMED' as amount_claimed, 'amount_paid' from set2")

#Combined Data Set
df_global = bind_rows(df_new, df_old)
write_csv(df_global, "./output/alldata.csv",append = FALSE, col_names = TRUE)

#remove the old files
rm (list = ls())