#Clear the Workspace
rm(list = ls())

#Load Libraries
library(tidyr)
library(plyr)
library(data.table)
library(readxl)
library(dplyr)
library(lubridate)
library(janitor)
library(reshape2)

#Set Working Directory on R Studio, **You may need to consider other methods like the here() library for other IDEs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#******************************************************************************
#Importing Auxiliary Datasets
df_optnames = read_xlsx("plancodes.xlsx")
df_dis = read_xlsx("discipline.xlsx")

#******************************************************************************
#Importing the claims datasets

#Year 2016
files_2016 = list.files("./claims_16", pattern = "*.xlsx", full.names = TRUE)
claims_16 = lapply(files_2016, read_xlsx)
claims_data16 = ldply(claims_16, drop_na, 1)
claims_data16 = clean_names(claims_data16, case = "snake")
claims_data16 = mutate(claims_data16, amount_paid = paid_from_risk_amt + paid_from_savings)
claims_data16 = select(claims_data16, member_no, service_date, dis, date_received, amount_paid, option_name)
claims_data16$service_date = ymd(claims_data16$service_date)
claims_data16$date_received = ymd(claims_data16$date_received)

#Year 2017
files_2017 = list.files("./claims_17", pattern = "*.xlsx", full.names = TRUE)
claims_17 = lapply(files_2017, read_xlsx)
claims_data17 = ldply(claims_17, drop_na, 1)
claims_data17 = clean_names(claims_data17, case = "snake")
claims_data17 = mutate(claims_data17, amount_paid = paid_from_risk_amt + paid_from_savings)
claims_data17 = select(claims_data17, member_no, service_date, dis, date_received, amount_paid, option_name)
claims_data17$service_date = ymd(claims_data17$service_date)
claims_data17$date_received = ymd(claims_data17$date_received)

#2018 Files
files_2018 = list.files("./claims_18", pattern = "*.xlsx", full.names = TRUE)
claims_18 = lapply(files_2018, read_xlsx)
claims_data18 = ldply(claims_18, drop_na, 1)
claims_data18 = clean_names(claims_data18, case = "snake")
claims_data18 = mutate(claims_data18, amount_paid = paid_from_risk_amt + paid_from_savings)
claims_data18 = select(claims_data18, member_no, service_date, dis, date_received, amount_paid, option_name)
claims_data18$service_date = ymd(claims_data18$service_date)
claims_data18$date_received = ymd(claims_data18$date_received)

#2019 Files
files_2019 = list.files("./claims_19", pattern = "*.xlsx", full.names = TRUE)
claims_19 = lapply(files_2019, read_xlsx, skip = 4)
claims_data19 = ldply(claims_19, drop_na, 1)
claims_data19 = claims_data19 %>%
	select (c("MEMBER", "TREATMENT DATE", "DISCIPLINE", "DATE RECEIVED", "AMOUNT", "PRODUCT"))
names(claims_data19)[1:6] = c("member_no", "service_date", "dis", "date_received", "amount_paid", "option_name")
claims_data19$service_date = ymd(claims_data19$service_date)
claims_data19$date_received = ymd_hms(claims_data19$date_received)

#******************************************************************************
#Merge the datasets
listdf = list(claims_data16, claims_data17, claims_data18, claims_data19)
claims_data = do.call(rbind, listdf)
claims_data = merge(claims_data, df_dis, by = "dis", all.x = TRUE, all.y = FALSE)
claims_data = merge(claims_data, df_optnames, by = "option_name", all.x = TRUE, all.y = FALSE)
claims_data = claims_data %>%
	mutate(treatment_month = format(as.Date(ymd(service_date)), "%Y-%m"))
claims_data = claims_data %>%
	select(optname, service_type, amount_paid, treatment_month, service_date, date_received)
claims_data = mutate(claims_data, dev = (
  year(date_received)*12 + month(date_received) - (
  year(service_date)*12 + month(service_date))
  )
)

#Save the master dataset
save(claims_data, file = "./output/zwl_claims.RData")

#******************************************************************************
#Gold Plan Files
files_au = list.files("./claims_au", pattern = "*.xlsx", full.names = TRUE)
claims_au = lapply(files_au, read_xlsx, skip = 4)
claims_au_n = ldply(claims_au, dplyr::select, c("MEMBER", "TREATMENT DATE", "DISCIPLINE", "DATE RECEIVED", "AMOUNT", "PRODUCT"))
au_data = drop_na(claims_au_n, 1)
au_data = clean_names(au_data, case = "snake")
names(au_data)[1:6] = c("member_no", "service_date", "dis", "date_received", "amount_paid", "option_name")
au_data$date_received = date(ymd_hms(au_data$date_received))

#Merge the two datasets
au_data$service_date = ymd(au_data$service_date)
au_data$date_received = ymd(au_data$date_received)

#Data Wrangling
au_data = mutate(au_data,
  treatment_month = format(as.Date(service_date), "%Y-%m"))
au_data = mutate(au_data, dev = (
  year(date_received)*12 + month(date_received) - (
  year(service_date)*12 + month(service_date))
  )
)
au_data = merge(au_data, df_dis, by.x = "dis", by.y = "dis", all.x = TRUE, all.y =  FALSE)
au_data = merge(au_data, df_optnames, by.x = "option_name", by.y = "option_name", all.x = TRUE, all.y =  FALSE)

#Save the master dataset
save(au_data, file = "./output/usd_claims.RData")