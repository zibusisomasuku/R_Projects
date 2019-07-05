#Clearing the workspace
rm (list = ls())

#Load Libraries
library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)

#Set Working Directory on R Studio
#You may need to consider other methods like the here() library for other IDEs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#******************************************************************************

#Procedure (done individiually to optimize RAM usage)
#1. Complile file lists that make the year
#2. Create a list of data tables by file for that year
#3. Combine the list of data tables into one data table
#4. remove #NA rows (ones with totals and the likes)
#5. Save the files

#Year 2016
files_2016 = list.files("./excel_claims/2016", full.names = TRUE, pattern =  "*.xlsx")
list_2016 = lapply(files_2016, read_xlsx)
data_2016 = rbindlist(list_2016, use.names = TRUE, fill = TRUE)
data_2016 = drop_na(data_2016, ncol = 1)

#Year 2017
files_2017 = list.files("./excel_claims/2017", full.names = TRUE, pattern =  "*.xlsx")
list_2017 = lapply(files_2017, read_xlsx)
data_2017 = rbindlist(list_2017, use.names = TRUE, fill = TRUE)
data_2017 = drop_na(data_2017, ncol = 1)

#Year 2018
files_2018 = list.files("./excel_claims/2018", full.names = TRUE, pattern =  "*.xlsx")
list_2018 = lapply(files_2018, read_xlsx)
data_2018 = rbindlist(list_2018, use.names = TRUE, fill = TRUE)
data_2018 = drop_na(data_2018, ncol = 1)

#Year 2019
files_2019 = list.files("./excel_claims/2019", full.names = TRUE, pattern =  "*.xlsx")
list_2019 = lapply(files_2019, read_xlsx)
data_2019 = rbindlist(list_2019, use.names = TRUE, fill = TRUE)
data_2019 = drop_na(data_2019, ncol = 1)

#Save the files
save(data_2016, data_2017, data_2018, data_2019, file = "./output/claims_data_raw.RData")

#Clearing the workspace
rm (list = ls())

#******************************************************************************

#Procedure (done individiually to optimize RAM usage)
#1. Load the rds file
#2. Optional: Combine columns to come up with other columns
#3. Rename columns
#4. Select only the wanted columns in a particular order
#5. Format specific columns as factors
#6. Add member attributes fro membership data

#Load Raw Claims Data
load("./output/claims_data_raw.RData")
df2016 = data_2016
df2017 = data_2017
df2018 = data_2018
df2019 = data_2019
rm(data_2016, data_2017, data_2018, data_2019)

#Load 2016 Data
df2016 = mutate(df2016, amount_paid = df2016$`PAID FROM RISK AMT` + df2016$`PAID FROM SAVINGS`)
colnames(df2016)[c(1,2,5,10:14,16,27,28,31,34)] = c("mem_num","ben_num","dis","date_treated","date_assessed","date_received","cl_chargeable_code","cl_chargeable_des","amount_claimed","clm_num","line_num","plan_code","amount_paid")
df2016 = select(df2016, mem_num, ben_num, plan_code, date_treated, date_received, date_assessed,dis,cl_chargeable_code,cl_chargeable_des, clm_num, line_num, amount_claimed,amount_paid)

df2016$mem_num = as.factor(df2016$mem_num)
df2016$ben_num = as.factor(df2016$ben_num)
df2016$plan_code = as.factor(df2016$plan_code)
df2016$date_treated = date(ymd(df2016$date_treated))
df2016$date_received = date(ymd(df2016$date_received))
df2016$date_assessed = date(ymd(df2016$date_assessed))
df2016$dis = as.factor(df2016$dis)
df2016$cl_chargeable_code = as.factor(df2016$cl_chargeable_code)
df2016$cl_chargeable_des = as.factor(df2016$cl_chargeable_des)
df2016$clm_num = as.factor(df2016$clm_num)
df2016$line_num = as.factor(df2016$line_num)
df2016$amount_claimed = as.numeric(as.character(df2016$amount_claimed))
df2016$amount_paid = as.numeric(as.character(df2016$amount_paid))

#Load 2017 Data
df2017 = mutate(df2017, amount_paid = df2017$`PAID FROM RISK AMT` + df2017$`PAID FROM SAVINGS`)
colnames(df2017)[c(1,2,5,11:15,18,29,30,33,36)] = c("mem_num","ben_num","dis","date_treated","date_assessed","date_received","cl_chargeable_code","cl_chargeable_des","amount_claimed","clm_num","line_num","plan_code","amount_paid")
df2017 = select(df2017, mem_num, ben_num, plan_code, date_treated, date_received, date_assessed,dis,cl_chargeable_code,cl_chargeable_des, clm_num, line_num, amount_claimed,amount_paid)

df2017$mem_num = as.factor(df2017$mem_num)
df2017$ben_num = as.factor(df2017$ben_num)
df2017$plan_code = as.factor(df2017$plan_code)
df2017$date_treated = date(ymd(df2017$date_treated))
df2017$date_received = date(ymd(df2017$date_received))
df2017$date_assessed = date(ymd(df2017$date_assessed))
df2017$dis = as.factor(df2017$dis)
df2017$cl_chargeable_code = as.factor(df2017$cl_chargeable_code)
df2017$cl_chargeable_des = as.factor(df2017$cl_chargeable_des)
df2017$clm_num = as.factor(df2017$clm_num)
df2017$line_num = as.factor(df2017$line_num)
df2017$amount_claimed = as.numeric(as.character(df2017$amount_claimed))
df2017$amount_paid = as.numeric(as.character(df2017$amount_paid))

#Load 2018 Data
df2018 = mutate(df2018, amount_paid = df2018$`PAID FROM RISK AMT` + df2018$`PAID FROM SAVINGS`)
colnames(df2018)[c(1,2,5,11:15,18,29,30,33,36)] = c("mem_num","ben_num","dis","date_treated","date_assessed","date_received","cl_chargeable_code","cl_chargeable_des","amount_claimed","clm_num","line_num","plan_code","amount_paid")
df2018 = select(df2018, mem_num, ben_num, plan_code, date_treated, date_received, date_assessed,dis,cl_chargeable_code,cl_chargeable_des, clm_num, line_num, amount_claimed,amount_paid)

df2018$mem_num = as.factor(df2018$mem_num)
df2018$ben_num = as.factor(df2018$ben_num)
df2018$plan_code = as.factor(df2018$plan_code)
df2018$date_treated = date(ymd(df2018$date_treated))
df2018$date_received = date(ymd(df2018$date_received))
df2018$date_assessed = date(ymd(df2018$date_assessed))
df2018$dis = as.factor(df2018$dis)
df2018$cl_chargeable_code = as.factor(df2018$cl_chargeable_code)
df2018$cl_chargeable_des = as.factor(df2018$cl_chargeable_des)
df2018$clm_num = as.factor(df2018$clm_num)
df2018$line_num = as.factor(df2018$line_num)
df2018$amount_claimed = as.numeric(as.character(df2018$amount_claimed))
df2018$amount_paid = as.numeric(as.character(df2018$amount_paid))

#Load 2019 Data
df2019 = mutate(df2019, amount_paid = df2019$`PAID FROM RISK AMT` + df2019$`PAID FROM SAVINGS`)
colnames(df2019)[c(1,2,5,11:15,18,29,30,33,36)] = c("mem_num","ben_num","dis","date_treated","date_assessed","date_received","cl_chargeable_code","cl_chargeable_des","amount_claimed","clm_num","line_num","plan_code","amount_paid")
df2019 = select(df2019, mem_num, ben_num, plan_code, date_treated, date_received, date_assessed,dis,cl_chargeable_code,cl_chargeable_des, clm_num, line_num, amount_claimed,amount_paid)

df2019$mem_num = as.factor(df2019$mem_num)
df2019$ben_num = as.factor(df2019$ben_num)
df2019$plan_code = as.factor(df2019$plan_code)
df2019$date_treated = date(ymd(df2019$date_treated))
df2019$date_received = date(ymd(df2019$date_received))
df2019$date_assessed = date(ymd(df2019$date_assessed))
df2019$dis = as.factor(df2019$dis)
df2019$cl_chargeable_code = as.factor(df2019$cl_chargeable_code)
df2019$cl_chargeable_des = as.factor(df2019$cl_chargeable_des)
df2019$clm_num = as.factor(df2019$clm_num)
df2019$line_num = as.factor(df2019$line_num)
df2019$amount_claimed = as.numeric(as.character(df2019$amount_claimed))
df2019$amount_paid = as.numeric(as.character(df2019$amount_paid))

#Combine all the data
dtlist = list(df2016, df2017, df2018, df2019)
claims_data = rbindlist(dtlist, use.names = TRUE)
claims_data = select(claims_data, mem_num, ben_num, plan_code, date_treated, date_received, date_assessed,dis,cl_chargeable_code,cl_chargeable_des, clm_num, line_num, amount_claimed,amount_paid)
View(head(claims_data))
save(claims_data, file =  "./output/claims_data_cleaned.RData")

#Load the previous data set
rm(list = ls())

#Load the just created data set
load("./output/claims_data_cleaned.RData")

#Import Discipline and Plan Codes
df_dis = read.csv("./csv/discipline.csv")
df_plan = read.csv("./csv/plancodes.csv")

#Merge claims data with displine codes and option names
claims_data = merge(claims_data, df_dis, by = "dis")
claims_data = merge(claims_data, df_plan, by = "plan_code")
claims_data = select(claims_data, mem_num, ben_num, optname, date_treated, date_received, date_assessed, service_type,cl_chargeable_code,cl_chargeable_des, clm_num, line_num, amount_claimed,amount_paid)
save(claims_data, file =  "./output/claims_data_cleaned_final.RData")