#Clear the Workspace
rm(list = ls())

#Load Libraries
suppressMessages(library(tidyr))
suppressMessages(library(plyr))
suppressMessages(library(data.table))
suppressMessages(library(readxl)) #reading Excel files
suppressMessages(library(dplyr)) #working with data tables
suppressMessages(library(lubridate)) #Working with dates
suppressMessages(library(janitor)) #Clean Names
suppressMessages(library(reshape2))

#Set Working Directory on R Studio
#**You may need to consider other methods like the here() library for other IDEs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#******************************************************************************
#Importing Auxiliary Datasets
df_optnames = read_xlsx("plancodes.xlsx")
df_dis = read_xlsx("discipline.xlsx")
df_rates = read_xlsx("exchange_rates.xlsx")
df_rates$date = ymd(df_rates$date)
df_assessment = read_xlsx("assessment_dates.xlsx") # 1 - 8 Jan of year relates to December Previous Year 

#******************************************************************************
#Data from 2019 was formatted differently. This includes:
# GOLD PLANS
# GOLD INDEX PLANS
# SILVER PLANS
# MICROMED
# FMH ZWL PLANS
# FMH NSSA PLANS
# FMH NSSA, ZWL and SILVER PLANS combined into 1

#A function was authoured to take care of these, it is defined below:

claimImport = function(my_path){
claim_files = list.files(my_path, pattern = "*.xlsx", full.names = TRUE)
claims_list = lapply(claim_files, read_xlsx, skip = 4)
claims_data = ldply(claims_list, drop_na, 1)
claims_data = claims_data %>%
	select (c("MEMBER", "TREATMENT DATE", "DISCIPLINE", "DATE RECEIVED", "AMOUNT", "PRODUCT", "ASSESSMENT DATE"))
names(claims_data)[1:7] = c("member_no", "service_date", "dis", "date_received", "amount_paid", "option_name", "assessment_date")
claims_data$service_date = ymd(claims_data$service_date)
claims_data$date_received = ymd_hms(claims_data$date_received)
claims_data$assessment_date = lubridate::date(claims_data$assessment_date)
assign(deparse(substitute(my_path)),claims_data, envir = .GlobalEnv)
}

#List the file paths for the respective datasets
gold_claims = "./claims_au"
micromed_claims = "./micromed"
gold_index = "./gold_index"
claims_2019 = "./claims_19"

#Import the files
claimImport(gold_claims)
claimImport(micromed_claims)
claimImport(gold_index)
claimImport(claims_2019)

#2016 - 2018 Datasets (Specific Method since claims follow a unique pattern)
files_old = list.files("./claims_old", pattern = "*.xlsx", full.names = TRUE)
claims_old = lapply(files_old, read_xlsx)
claims_data_old = ldply(claims_old, drop_na, 1)
claims_data_old = clean_names(claims_data_old, case = "snake")
claims_data_old = mutate(claims_data_old, amount_paid = paid_from_risk_amt + paid_from_savings)
claims_data_old = select(claims_data_old, member_no, service_date, dis, date_received, amount_paid, option_name, assess_date)
claims_data_old$service_date = ymd(claims_data_old$service_date)
claims_data_old$date_received = ymd(claims_data_old$date_received)
claims_data_old$assess_date = ymd(claims_data_old$assess_date)
claims_data_old = rename(claims_data_old, assessment_date = assess_date)
#******************************************************************************
#Merge the ZWL datasets
listdf = list(claims_data_old, claims_2019)
zwl_claims = do.call(rbind, listdf)

wrangleData = function(my_data){
claims = merge(my_data, df_dis, by = "dis", all.x = TRUE, all.y = FALSE)
claims = merge(claims, df_assessment, by = "assessment_date", all.x = TRUE, all.y = FALSE)
claims = merge(claims, df_optnames, by = "option_name", all.x = TRUE, all.y = FALSE)
claims = claims %>%
	mutate(treatment_month = format(as.Date(ymd(service_date)), "%Y-%m"))
claims =  claims %>%
	select(optname, service_type, amount_paid, treatment_month, service_date, date_received, assessment_date)
claims = mutate(claims, dev = (
  year(date_received)*12 + month(date_received) - (
  year(service_date)*12 + month(service_date))
  )
)
assign(deparse(substitute(my_data)),claims, envir = .GlobalEnv)
}

#Data Wrangling using custom function
wrangleData(gold_claims)
write.csv(gold_claims,"gold_claims1.csv")

#Factoring in the Exchange Rates
gold_claims$assessment_date = date(gold_claims$assessment_date)

gold_claims = merge(gold_claims, select(df_rates, date, fml_rate), by.x = "assessment_date", by.y = "date", all.x = TRUE, all.y = FALSE)

gold_claims$fml_rate = as.numeric(gold_claims$fml_rate)

gold_claims = gold_claims %>%
	mutate (amount_paid2 = amount_paid/fml_rate) %>%
	select(optname, service_type, amount_paid2, treatment_month, service_date, date_received, assessment_date, dev) %>%
	rename(amount_paid =  amount_paid2)
	
write.csv(gold_claims, "gold_claims2.csv")

wrangleData(micromed_claims)
wrangleData(gold_index)
wrangleData(zwl_claims)

#Save the master datasets
save(zwl_claims, file = "./output/zwl_claims.RData")
save(gold_claims, file = "./output/usd_claims.RData")
save(micromed_claims, file = "./output/micromed_claims.RData")
save(gold_index, file = "./output/usd_index_claims.RData")