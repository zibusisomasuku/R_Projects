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
triangle = matrix(
	nrow = length(unique(claims_data$quarter_treated)),
	ncol = length(unique(claims_data$delay)),
	dimnames = list(
            Origin = sort(unique(claims_data$quarter_treated)),
            Dev = sort(unique(claims_data$delay))
            )
	)

triangle[cbind(factor(claims_data$quarter_treated),claims_data$delay)] = claims_data$amount_paid

print(triangle)