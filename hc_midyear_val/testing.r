#Clearing the workspace
rm (list = ls())

#Load Libraries
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
				select(mem_num, ben_num, date_treated, date_received, amount_paid, optname, service_type) %>%
				mutate(month_treated = str_c(year(date_treated), month(date_treated, label =  TRUE, abbr = TRUE), sep = " "))

claims_data$month_treated = as.factor(claims_data$month_treated)

claims_data = claims_data %>%
				mutate(delay = (year(claims_data$date_received)*4 + quarter(claims_data$date_received))-(year(claims_data$date_treated)*4 + quarter(claims_data$date_treated)))

summary_claims = claims_data %>%
  						select(month_treated, service_type, amount_paid) %>%
						group_by(optname, service_type)


columns_needed = c("file_year", "accident_year", "paid")

merged_data = as_data_frame(matrix(nrow = 0, ncol = length(columns_needed)))
names(merged_data) = columns_needed

extract_needed_excel_data = function(cur_file_path){
  read_excel(cur_file_path) %>%
    select(columns_needed) %>%
    rbind(merged_data)
}


loss_run_data = ldply(file_paths, extract_needed_excel_data)

loss_run_data  = loss_run_data %>% 
  mutate(maturity_in_months = (file_year - accident_year)*12)

merged_triangle = as.triangle(loss_run_data, 
                              dev = "maturity_in_months", 
                              origin = "accident_year", 
                              value = "paid")

as.matrix(merged_triangle)

plot(merged_triangle, 
     main = "Paid Losses vs Maturity by Accident Year",
     xlab = "Maturity in Months", 
     ylab = "Paid Losses")

#******************************************************************************
triangle = matrix(
	nrow = length(unique(claims_data$month_treated)),
	ncol = length(unique(claims_data$delay)),
	dimnames = list(
            Origin = sort(unique(claims_data$month_treated)),
            Dev = sort(unique(claims_data$delay))
            )
	)

triangle[cbind(factor(claims_data$month_treated),claims_data$delay)] = claims_data$amount_paid

print(triangle)


triangle_data = claims_data [,c(8,9,5)]
triangle = as.triangle(triangle_data, origin = "month_treated", dev = "delay", value = "amount_paid")
cum.triangle = incr2cum(triangle, na.rm = TRUE)
cum.triangle = cum.triangle[,apply(!is.na(cum.triangle),2,any)]
full_triangle =  predict(chainladder(cum.triangle))
ultimate = c(full_triangle[,ncol(full_triangle)])
current_emerged_val = getLatestCumulative(cum.triangle, na.values = NULL)
IBNR = ultimate-current_emerged_val
ATU = ultimate/current_emerged_val
IBNRExhibit = data.frame(current_emerged_val, ATU, ultimate, IBNR)
IBNRExhibit = rbind(IBNRExhibit, data.frame(current_emerged_val = sum(current_emerged_val), ATU = NA, ultimate = sum(ultimate),IBNR = sum(IBNR), row.names = "Total"))  