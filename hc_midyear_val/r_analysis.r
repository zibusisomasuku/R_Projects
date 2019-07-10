#Clearing the workspace
rm (list = ls())

#Load Libraries
library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(plyr)
library(ggplot2)
library(ChainLadder)

#Set Working Directory on R Studio
#You may need to consider other methods like the here() library for other IDEs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#******************************************************************************
#Data Wrangling
load("claims_data_final.RData")
claims_data = claims_data %>%
				select(mem_num, ben_num, date_treated, date_received, amount_paid, optname, service_type)
claims_data = claims_data %>%
				mutate(quarter_treated = str_c(year(date_treated),str_c("Q",quarter(date_treated), sep = ""), sep = " "))
claims_data$quarter_treated = as.factor(claims_data$quarter_treated)
claims_data = claims_data %>%
				mutate(delay = (year(claims_data$date_received)*4 + quarter(claims_data$date_received))-(year(claims_data$date_treated)*4 + quarter(claims_data$date_treated)))

#******************************************************************************
#Make data sumaries!
summary_claims = claims_data %>%
  						select(optname, service_type, amount_paid) %>%
						group_by(optname, service_type)

#Create Overall Summary of Claims data by Plan
summary_plan = ddply(summary_claims,.(optname), summarise, total_paid = sum(amount_paid), total_claims = length(amount_paid))
total_paidclaims = sum(summary_plan$total_paid)
total_paidcount = sum(summary_plan$total_claims)

summary_plan$total_paid[is.na(summary_plan$total_paid)] = 0
summary_plan$total_claims[is.na(summary_plan$total_claims)] = 0

summary_plan = cbind(summary_plan, PercentageAmount = c((summary_plan$total_paid/total_paidclaims)*100), PercentageCount = c((summary_plan$total_claims/total_paidcount)*100))

summary_plan$PercentageAmount<-as.numeric(round(summary_plan$PercentageAmount,2))
summary_plan$PercentageCount<-as.numeric(round(summary_plan$PercentageCount,2))
print(summary_plan)

#Create Summary of claims data by service type
summary_service = ddply(summary_claims,.(service_type), summarise, total_paid = sum(amount_paid), total_claims = length(amount_paid))
total_paidclaims = sum(summary_service$total_paid)
total_paidcount = sum(summary_service$total_claims)

summary_service = cbind(summary_service, PercentageAmount = c((summary_service$total_paid/total_paidclaims)*100), PercentageCount=c((summary_service$total_claims/total_paidcount)*100))

summary_service$PercentageAmount = as.numeric(round(summary_service$PercentageAmount,2))
summary_service$PercentageCount = as.numeric(round(summary_service$PercentageCount,2))
print(summary_service)

#******************************************************************************

#Plot of Paid Claims paid by Plan
ggplot(data = summary_plan, aes(x = optname, y = total_paid, fill = optname)) + 
     geom_bar(stat = 'identity', position = 'dodge') + labs( x = "Option Names", y = "Total Paid", title = "Summary of Claims Paid by Plan")

##summary of claim counts by plan
count = ddply(summary_claims,.(optname), summarise, total_count = length(amount_paid))

ggplot(data = count, aes(x = optname, y = total_count, fill = optname)) + 
  geom_bar(stat = 'identity', position = 'dodge') + labs( x = "Option Names", y = "Total Count", title = "Summary of Claims by Plan")


triangle_data = claims_data[,c(8,9,5)]
triangle = as.triangle(triangle_data, bycol = TRUE, origin = "quarter_treated", dev = "delay", value = "amount_paid")

#******************************************************************************
#Split data into Service Types

claim_splits = split(claims_data, claims_data$service_type)

#Calculate IBNR using BCL
hc_bcl = function(my.data){

	triangle_data = my.data [,c(8,9,5)]
	triangle = as.triangle(triangle_data, origin = "quarter_treated", dev = "delay", value = "amount_paid")
	cum.triangle = incr2cum(triangle, na.rm = TRUE)
	cum.triangle = cum.triangle[,apply(!is.na(cum.triangle),2,any)]
	full_triangle =  predict(chainladder(cum.triangle))
	ultimate = c(full_triangle[,ncol(full_triangle)])
	current_emerged_val = getLatestCumulative(cum.triangle, na.values = NULL)
	IBNR = ultimate-current_emerged_val
	ATU = ultimate/current_emerged_val
	IBNRExhibit = data.frame(current_emerged_val, ATU, ultimate, IBNR)
	IBNRExhibit = rbind(IBNRExhibit, data.frame(current_emerged_val = sum(current_emerged_val), ATU = NA, ultimate = sum(ultimate), IBNR = sum(IBNR), row.names = "Total"))  
}

for(i in 1:length(claim_splits)){
  hc_bcl(my.data = claim_splits[[i]])
}
reserve_results = lapply(claim_splits, FUN = hc_bcl)

#Print BCL IBNR resultimates
reserve_results

### endofBCLcalcs
