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
library(erer)

#Set Working Directory on R Studio, **You may need to consider other methods like the here() library for other IDEs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#******************************************************************************
#Load the datasets
load("./output/zwl_claims.RData")
load("./output/usd_claims.RData")

lv_date = date(ymd("2018-12-31")) #Last Valuation Date, Set a date 12 months prior the valuation ate
cv_date = date(ymd("2019-12-31")) #Current Valuation Date
#Split the data by service types

final_data = zwl_claims %>%
  filter(service_date > lv_date) %>%
  filter(service_date <= cv_date)
  
claims_splits = split(final_data, final_data$service_type)

#Calculate IBNR Using BCL
cl_function = function(my_data){
  tri_data = select(my_data, dev, treatment_month, amount_paid)
  inc_tri = as.triangle(tri_data, 
                              dev = "dev", 
                              origin = "treatment_month", 
                              value = "amount_paid")
  cum_tri = incr2cum(inc_tri, na.rm = TRUE)
  plot1 = plot(cum_tri,
     main = "Paid Losses vs Development by Accident Month",
     xlab = "Maturity in Months", 
     ylab = "Paid Losses",
     lattice = TRUE)
  cl_estimate = chainladder(cum_tri) #Determine the CL estimates.
  mack_est = MackChainLadder(cum_tri, est.sigma = "Mack")
  CDR(mack_est)
}

all_results = lapply(claims_splits, FUN = cl_function)
write.list(all_results, file = "./output/zwl_results.csv", t.name = names(all_results))

#Split the data by service types
final_data = gold_claims %>%
	filter(service_date > lv_date) %>%
	filter(service_date <= cv_date)

claims_splits = split(final_data, final_data$service_type)

#Calculate IBNR Using BCL
cl_function = function(my_data){
  tri_data = select(my_data, dev, treatment_month, amount_paid)
  inc_tri = as.triangle(tri_data, 
                              dev = "dev", 
                              origin = "treatment_month", 
                              value = "amount_paid")
  cum_tri = incr2cum(inc_tri, na.rm = TRUE)
  plot1 = plot(cum_tri,
     main = "Paid Losses vs Development by Accident Month",
     xlab = "Maturity in Months", 
     ylab = "Paid Losses",
     lattice = TRUE)
  cl_estimate = chainladder(cum_tri) #Determine the CL estimates.
  mack_est = MackChainLadder(cum_tri, est.sigma = "Mack")
  CDR(mack_est)
}

usd_results = lapply(claims_splits, FUN = cl_function)
write.list(usd_results, file = "./output/usd_results.csv", t.name = names(usd_results))

print("Calculations are done!")