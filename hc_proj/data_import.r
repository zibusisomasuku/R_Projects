#Clearing the workspace
rm (list = ls())

#Load Libraries
library(here)
library(readxl)
library(tidyverse)
library(readr)
library(dplyr)
library(data.table)

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

#Select only specific columns and rename them
df_old = select(set1,Plan.Code,Claim.Number,Member.Number,Beneficiary.Number,Practice.Type,Date.Received,Treatment.Date,Process.Date,Account.Amount,Paid.Amount)

df_old = df_old %>% rename( plan_code = Plan.Code,clm_num = Claim.Number,mem_num = Member.Number,ben_num = Beneficiary.Number, dis = Practice.Type,received_date = Date.Received,treatment_date = Treatment.Date,assess_date = Process.Date,amount_claimed = Account.Amount,amount_paid = Paid.Amount)

#Load the second data set (based on values from the new system)
set2a = read.csv("./output/post2015data.csv")

#New system does not provide the total paid column, this section combines the two settlements paid from savings and risk amounts together
set2 = dplyr::mutate(set2a, amount_paid = set2a$PAID.FROM.RISK.AMT + set2a$PAID.FROM.SAVINGS)

#Select only specific columns needed and rename them
df_new = select(set2,OPTION.NAME,CLAIM.NO,MEMBER.NO,INO,DIS,DATE.RECEIVED,SERVICE.DATE,ASSESS.DATE,AMOUNT.CLAIMED,amount_paid)

df_new = df_new %>% rename( plan_code = OPTION.NAME,clm_num = CLAIM.NO,mem_num = MEMBER.NO,ben_num = INO, dis = DIS,received_date = DATE.RECEIVED,treatment_date = SERVICE.DATE,assess_date = ASSESS.DATE,amount_claimed = AMOUNT.CLAIMED)

#Combined Data Set
df_global = rbindlist(list(df_new, df_old),use.names = TRUE)
write_csv(df_global, "./output/alldata.csv",append = FALSE, col_names = TRUE)

#remove the old files
rm (list = ls())