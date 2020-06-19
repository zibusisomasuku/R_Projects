#Story: This file analyses the claims data and summarises it by Month, Service Type and Plan

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
library(erer)

#Set Working Directory on R Studio, **You may need to consider other methods like the here() library for other IDEs
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(here::here("FMH_Valuation"))

#******************************************************************************
#ZWL PLANS
#******************************************************************************
#Load the dataset
load("./output/zwl_claims.RData")

lv_date <- ymd("2018-12-31") #Last Valuation Date, Set 12 months prior the Valuation Date
final_data <- zwl_claims %>%
  filter(date_received > lv_date)

#Service Type
service_type_summary_amount <- dcast(final_data, service_type ~ ., sum, value.var = "amount_paid")
service_type_summary_count <- dcast(final_data, service_type ~ ., length, value.var = "amount_paid")
summary_service <- merge(service_type_summary_amount, service_type_summary_count, by = "service_type", all = TRUE)

#Options
optname_summary_amount <- dcast(final_data, optname ~ ., sum, value.var = "amount_paid")
optname_summary_count <- dcast(final_data, optname ~ ., length, value.var = "amount_paid")
summary_by_optname <- merge(optname_summary_amount, optname_summary_count, by = "optname", all = TRUE)

#Treatment Month
final_data_fl <- zwl_claims %>%
  filter(service_date > lv_date)

month_summary_amount <- dcast(final_data_fl, treatment_month ~ ., sum, value.var = "amount_paid")
month_summary_count <- dcast(final_data_fl, treatment_month ~ ., length, value.var = "amount_paid")
summary_by_month <- merge(month_summary_amount, month_summary_count, by = "treatment_month", all = TRUE)

#Service Type by Plan
summary_by_service_option <- dcast(final_data, service_type ~ optname, sum, value.var = "amount_paid")

#Save
summaries <- list(
  summary_service,
  summary_by_optname,
  summary_by_month,
  summary_by_service_option)

write.list (summaries, file = "./output/zwl_claim_summaries.csv", t.name = c(
  "Summary by Service Type",
  "Summary by Option",
  "Summary by Treatment Month",
  "Summary by Service Type and Option")
)
summaries
rm(summary_service, summary_by_optname, summary_by_month, summary_by_service_option)
rm(summaries)
#******************************************************************************
#USD PLANS
#******************************************************************************
#Load the dataset
load("./output/usd_claims.RData")

final_data <- gold_claims %>%
  filter(date_received > lv_date)

#Service Type
service_type_summary_amount <- dcast(final_data, service_type ~ ., sum, value.var = "amount_paid")
service_type_summary_count <- dcast(final_data, service_type ~ ., length, value.var = "amount_paid")
summary_service <- merge(service_type_summary_amount, service_type_summary_count, by = "service_type", all = TRUE)

#Options
optname_summary_amount <- dcast(final_data, optname ~ ., sum, value.var = "amount_paid")
optname_summary_count <- dcast(final_data, optname ~ ., length, value.var = "amount_paid")
summary_by_optname <- merge(optname_summary_amount, optname_summary_count, by = "optname", all = TRUE)

#Treatment Month
final_data_fl <- gold_claims %>%
  filter(service_date > lv_date)
month_summary_amount <- dcast(final_data_fl, treatment_month ~ ., sum, value.var = "amount_paid")
month_summary_count <- dcast(final_data_fl, treatment_month ~ ., length, value.var = "amount_paid")
summary_by_month <- merge(month_summary_amount, month_summary_count, by = "treatment_month", all = TRUE)

#Service Type by Plan
summary_by_service_option <- dcast(final_data, service_type ~ optname, sum, value.var = "amount_paid")

#Save
summaries_au <- list(
  summary_service,
  summary_by_optname,
  summary_by_month,
  summary_by_service_option)

write.list(summaries_au, file = "./output/usd_claim_summaries.csv", t.name = c(
  "Summary by Service Type",
  "Summary by Option",
  "Summary by Treatment Month",
  "Summary by Service Type and Option")
)
summaries_au