#Clear the Workspace
rm(list = ls())

#Load Libraries
library(tidyr)
library(ChainLadder)
library(plyr)
library(data.table)
library(readxl)
library(dplyr)
library(lubridate)
library(janitor)
library(reshape2)

#Set Working Directory on R Studio, **You may need to consider other methods like the here() library for other IDEs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Importing the datasets
load("./output/zwl_claims.RData")
lv_date = ymd("2018-06-30")
lv_date2 = ymd("2018-12-31")

#Back-test
backtest1 =  claims_data %>%
	filter(service_date <= lv_date) %>%
	filter(date_received > lv_date)

backtest1_summary = dcast(backtest1, service_type ~., sum, value.var = "amount_paid")
backtest1_summary
write.csv(backtest1_summary, file = "./output/backtest_midyear2018.csv")

backtest2 =  claims_data %>%
	filter(service_date <= lv_date2) %>%
	filter(date_received > lv_date2) %>%
	mutate(notif_month = format(as.Date(ymd(date_received)), "%Y-%m"))

backtest2_summary = dcast(backtest2, service_type ~ notif_month, sum, value.var = "amount_paid")
backtest2_summary
write.csv(backtest2_summary, file = "./output/backtest_FY2018.csv")

backtest3 =  claims_data %>%
	filter(service_date <= ymd("2018-12-31")) %>%
	filter(date_received > ymd("2018-12-31")) %>%
	filter(date_received < ymd("2019-02-01"))

backtest3_summary = dcast(backtest3, service_type ~ date_received, sum, value.var = "amount_paid")
backtest3_summary
write.csv(backtest3_summary, file = "./output/backtest_jan2019_FY2018.csv")

print("Calculation is done")