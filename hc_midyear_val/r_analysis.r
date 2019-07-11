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
				select(mem_num, ben_num, date_treated, date_received, amount_paid, optname, service_type) %>%
				mutate(month_treated =
					str_c(year(date_treated), month(date_treated, label =  TRUE, abbr = TRUE), sep = " ")) %>%
				mutate(delay = (year(date_received)*12 + month(date_received))-(year(date_treated)*12 + month(date_treated)))

claims_data$month_treated = as.factor(claims_data$month_treated)
#******************************************************************************
#CHAINLADDER PREDICTIONS

#Split data into Service Types
claims_splits = claims_data %>%
				filter(year(date_treated) > 2017)
claim_splits = split(claims_data, claims_data$service_type)

#Calculate IBNR using BCL
bcl_estimation = function(my_data){
	triangle_data = my_data [,c(8,9,5)]
	triangle = as.triangle(triangle_data, origin = "month_treated", dev = "delay", value = "amount_paid")
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
  bcl_estimation(my_data = claim_splits[[i]])
}
reserve_results = lapply(claim_splits, FUN = bcl_estimation)

#Print BCL IBNR resultimates
reserve_results