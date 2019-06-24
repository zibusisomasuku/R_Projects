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

#Procedure (done individiually to optimize RAM usage)
#1. Load the rds file
#2. Optional: Combine columns to come up with other columns
#3. Rename columns
#4. Select only the wanted columns in a particular order
#5. Format specific columns as factors

tictoc::tic()

#Load 2013 Data
df2013 = readRDS("./output/2013 data.rds")
df2013 = mutate(df2013, amount_paid = df2013$`Pay Member Amount` + df2013$`Pay Vendor Amount`)
colnames(df2013)[c(2:6,9,13:14,16:17,19,22,26)] = c("plan_code","clm_num","line_num","mem_num","ben_num","dis","date_received","date_treated","cl_chargeable_code","cl_chargeable_des","date_assessed","amount_claimed","amount_paid")
df2013 = select(df2013, mem_num, ben_num, plan_code, date_treated, date_received, date_assessed,dis,cl_chargeable_code,cl_chargeable_des, clm_num, line_num, amount_claimed,amount_paid)

df2013$mem_num = as.factor(df2013$mem_num)
df2013$ben_num = as.factor(df2013$ben_num)
df2013$plan_code = as.factor(df2013$plan_code)
df2013$date_treated = date(ymd(df2013$date_treated))
df2013$date_received = date(ymd(df2013$date_received))
df2013$date_assessed = date(ymd(df2013$date_assessed))
df2013$dis = as.factor(df2013$dis)
df2013$cl_chargeable_code = as.factor(df2013$cl_chargeable_code)
df2013$cl_chargeable_des = as.factor(df2013$cl_chargeable_des)
df2013$clm_num = as.factor(df2013$clm_num)
df2013$line_num = as.factor(df2013$line_num)
df2013$amount_claimed = as.numeric(as.character(df2013$amount_claimed))
df2013$amount_paid = as.numeric(as.character(df2013$amount_paid))

#Load 2014 Data
df2014 = readRDS("./output/2014 data.rds")
colnames(df2014)[c(2:6,9,13:14,16:17,19,22,26)] = c("plan_code","clm_num","line_num","mem_num","ben_num","dis","date_received","date_treated","cl_chargeable_code","cl_chargeable_des","date_assessed","amount_claimed","amount_paid")
df2014 = select(df2014, mem_num, ben_num, plan_code, date_treated, date_received, date_assessed,dis,cl_chargeable_code,cl_chargeable_des, clm_num, line_num, amount_claimed,amount_paid)

df2014$mem_num = as.factor(df2014$mem_num)
df2014$ben_num = as.factor(df2014$ben_num)
df2014$plan_code = as.factor(df2014$plan_code)
df2014$date_treated = date(ymd(df2014$date_treated))
df2014$date_received = date(ymd(df2014$date_received))
df2014$date_assessed = date(ymd(df2014$date_assessed))
df2014$dis = as.factor(df2014$dis)
df2014$cl_chargeable_code = as.factor(df2014$cl_chargeable_code)
df2014$cl_chargeable_des = as.factor(df2014$cl_chargeable_des)
df2014$clm_num = as.factor(df2014$clm_num)
df2014$line_num = as.factor(df2014$line_num)
df2014$amount_claimed = as.numeric(as.character(df2014$amount_claimed))
df2014$amount_paid = as.numeric(as.character(df2014$amount_paid))

#Load 2015 Data
df2015 = readRDS("./output/2015 data.rds")
colnames(df2015)[c(2:6,10,12:13,16:19,23)] = c("clm_num","line_num","mem_num","ben_num","dis","plan_code","cl_chargeable_code","cl_chargeable_des","date_treated","date_received","date_assessed","amount_claimed","amount_paid")
df2015$mem_num = as.numeric(as.character(df2015$mem_num))
df2015 = select(df2015, mem_num, ben_num, plan_code, date_treated, date_received, date_assessed,dis,cl_chargeable_code,cl_chargeable_des, clm_num, line_num, amount_claimed,amount_paid)

df2015$mem_num = as.factor(df2015$mem_num)
df2015$ben_num = as.factor(df2015$ben_num)
df2015$plan_code = as.factor(df2015$plan_code)
df2015$date_treated = date(ymd(df2015$date_treated))
df2015$date_received = date(ymd(df2015$date_received))
df2015$date_assessed = date(ymd(df2015$date_assessed))
df2015$dis = as.factor(df2015$dis)
df2015$cl_chargeable_code = as.factor(df2015$cl_chargeable_code)
df2015$cl_chargeable_des = as.factor(df2015$cl_chargeable_des)
df2015$clm_num = as.factor(df2015$clm_num)
df2015$line_num = as.factor(df2015$line_num)
df2015$amount_claimed = as.numeric(as.character(df2015$amount_claimed))
df2015$amount_paid = as.numeric(as.character(df2015$amount_paid))

#Load 2016 Data
df2016 = readRDS("./output/2016 data.rds")
df2016 = mutate(df2016, amount_paid = df2016$`PAID FROM RISK AMT` + df2016$`PAID FROM SAVINGS`)
colnames(df2016)[c(1,2,5,10:14,16,27,28,31,34)] = c("mem_num","ben_num","dis","date_treated","date_assessed","date_received","cl_chargeable_code","cl_chargeable_des","amount_claimed","clm_num","line_num","plan_code","amount_paid")
df2016$mem_num = as.numeric(as.character(df2016$mem_num))
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

#Load 2017/18 Data
df2017_18 = readRDS("./output/2017-18 data.rds")
df2017_18 = mutate(df2017_18, amount_paid = df2017_18$`PAID FROM RISK AMT` + df2017_18$`PAID FROM SAVINGS`)
colnames(df2017_18)[c(1,2,5,11:15,18,29,30,33,36)] = c("mem_num","ben_num","dis","date_treated","date_assessed","date_received","cl_chargeable_code","cl_chargeable_des","amount_claimed","clm_num","line_num","plan_code","amount_paid")
df2017_18$mem_num = as.numeric(as.character(df2017_18$mem_num))
df2017_18 = select(df2017_18, mem_num, ben_num, plan_code, date_treated, date_received, date_assessed,dis,cl_chargeable_code,cl_chargeable_des, clm_num, line_num, amount_claimed,amount_paid)

df2017_18$mem_num = as.factor(df2017_18$mem_num)
df2017_18$ben_num = as.factor(df2017_18$ben_num)
df2017_18$plan_code = as.factor(df2017_18$plan_code)
df2017_18$date_treated = date(ymd(df2017_18$date_treated))
df2017_18$date_received = date(ymd(df2017_18$date_received))
df2017_18$date_assessed = date(ymd(df2017_18$date_assessed))
df2017_18$dis = as.factor(df2017_18$dis)
df2017_18$cl_chargeable_code = as.factor(df2017_18$cl_chargeable_code)
df2017_18$cl_chargeable_des = as.factor(df2017_18$cl_chargeable_des)
df2017_18$clm_num = as.factor(df2017_18$clm_num)
df2017_18$line_num = as.factor(df2017_18$line_num)
df2017_18$amount_claimed = as.numeric(as.character(df2017_18$amount_claimed))
df2017_18$amount_paid = as.numeric(as.character(df2017_18$amount_paid))

#Combine all the data
dtlist = list(df2013,df2014,df2015,df2016,df2017_18)
claims_data = rbindlist(dtlist, use.names = TRUE)
#claims_data = bind_rows(df2013,df2014,df2015,df2016,df2017_18)
claims_data = select(claims_data, mem_num, ben_num, plan_code, date_treated, date_received, date_assessed,dis,cl_chargeable_code,cl_chargeable_des, clm_num, line_num, amount_claimed,amount_paid)
View(head(claims_data))
saveRDS(claims_data, "./output/2013-18 data.rds")

tictoc::toc()

#Load the previous data set
rm(list = ls())

#Load the just created data set
claims_data = readRDS("./output/2013-18 data.rds")

#Import Discipline and Plan Codes
df_dis = read.csv("./csv/discipline.csv")
df_plan = read.csv("./csv/plancodes.csv")

#Merge claims data with displine codes and option names
claims_data = merge(claims_data, df_dis, by = "dis")
claims_data = merge(claims_data, df_plan, by = "plan_code")
claims_data = dplyr::select(claims_data, mem_num, ben_num, optnamefull, date_treated, date_received, date_assessed, service_type,cl_chargeable_code,cl_chargeable_des, clm_num, line_num, amount_claimed,amount_paid)
saveRDS(claims_data, "./output/2013-18 final data.rds")
