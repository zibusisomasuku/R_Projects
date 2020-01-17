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
library(erer)

#Set Working Directory on R Studio, **You may need to consider other methods like the here() library for other IDEs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load datasets
load("./output/zwl_claims.RData") #load ZWL Dataset
load("./output/usd_claims.RData") #load USD Dataset
usd_claims = gold_claims
rm(gold_claims)

#Mutating Currrency Columns
usd_claims = usd_claims %>%
	mutate(currency = "USD")
zwl_claims =  zwl_claims %>%
	mutate (currency = "ZWL")

#Select columns
usd_claims = usd_claims %>%
	select(treatment_month, currency, service_type, dev, amount_paid)
zwl_claims = zwl_claims %>%
	select( treatment_month,currency, service_type, dev, amount_paid)
	
#Merge the two datasets
combined_data = bind_rows(usd_claims, zwl_claims)

#Summarize the data by treatment month, currency, service type, amount_paid and development
summarized_data = dcast( combined_data, treatment_month + currency + service_type + dev ~ ., sum, value.var = "amount_paid")

#Save to CSV
write.csv(summarized_data, file = "./output/summarized_data.csv")
