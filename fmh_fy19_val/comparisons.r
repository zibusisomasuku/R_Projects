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
library(erer) #for the write.list function

# Sets Working Directory on R Studio,
# **You may need to consider other methods like the here() library for other IDEs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#******************************************************************************
#	ZWL PLANS, the GOLD PLANS are new, so they are not going to be used for
#	this sensitivity analysis.
#
#******************************************************************************
load("./output/zwl_claims.RData") #Load the dataset

hy18_val_date = date(ymd("2018-06-30")) #Date for the June 2018 valuation
fy18_val_date = date(ymd("2018-12-31")) #Date for the December 2018 Valuation
hy19_val_date = date(ymd("2019-06-30")) #Date for the June 2019 valuation

#Create datasets for each valuation period
hy18data = claims_data %>%
  filter(date_received > date(ymd("2017-12-31"))) %>%
  filter(date_received <= hy18_val_date)

fy18data =  claims_data %>%
	filter(date_received > date(ymd("2017-12-31"))) %>%
	filter(date_received <= fy18_val_date)

hy19data = claims_data %>%
  filter(date_received > date(ymd("2018-12-31"))) %>%
  filter(date_received <= hy19_val_date)

all_data = list(hy18data, fy18data, hy19data)
names(all_data) = c("HY2018 Valuation", "FY2018 Valuation", "HY2019 valuation")

#Calculate Totals
comps_fun = function(my_data){
  counts = dcast(my_data, service_type ~., length, value.var = "amount_paid")
  totals = dcast(my_data, service_type ~., sum, value.var = "amount_paid")
  avg_dev = dcast(my_data, service_type ~., mean, value.var = "dev")
  summary_data = merge(totals, counts, by = "service_type")
  summary_data = merge(summary_data, avg_dev, by = "service_type")
  summary_data = summary_data %>%
    mutate (avg_claim = summary_data[,2]/summary_data[,3])
  names(summary_data) = c("Service Type", "Total Claims Paid", "Total Number of Claims", "Average Report Delay", "Average Claim Amount")
  summary_data
}

claims_summary = lapply(all_data, FUN = comps_fun)
write.list(claims_summary, "./output/claims_summary2.csv", t.name = names(claims_summary))

print("Calculations are done!")