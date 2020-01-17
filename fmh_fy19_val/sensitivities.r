#******************************************************************************
#	This script considers the previous valuation period, that is:
#	the 30 June 2019, and tries to develop
#	age-to-age (ata) factors from the valuation period.
#	The goal is to the demonstrate that, should the ata factors observed
#	in those periods persisted into the current periods, this is how our
#	IBNR value would have been different.
#
#******************************************************************************

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
#	GOLD PLANS
#
#******************************************************************************
load("./output/usd_claims.RData") #Load the dataset

val_date = date(ymd("2019-06-30")) #Date for the June 2019 valuation

#Split the data by service types per valuation period

hy18data = claims_data %>%
  filter(service_date > date(ymd("2017-06-30"))) %>%
  filter(date_received <= hy18_val_date)

fy18data =  claims_data %>%
	filter(service_date > date(ymd("2017-12-31"))) %>%
	filter(date_received <= fy18_val_date)

hy19data = claims_data %>%
  filter(service_date > date(ymd("2018-06-30"))) %>%
  filter(date_received <= hy19_val_date)

hy19data_au = au_data %>%
  filter(service_date > date(ymd("2018-06-30"))) %>%
  filter(date_received <= hy19_val_date)

hy18_splits = split(hy18data, hy18data$service_type)
fy18_splits	= split(fy18data, fy18data$service_type)
hy19_splits = split(hy19data, hy19data$service_type)
hy19au_splits = split(hy19data_au, hy19data_au$service_type)

#Calculate ATA factors
ata_function = function(my_data){
  tri_data = select(my_data, dev, treatment_month, amount_paid)
  inc_tri = as.triangle(tri_data, 
                              dev = "dev", 
                              origin = "treatment_month", 
                              value = "amount_paid")
  cum_tri = incr2cum(inc_tri, na.rm = TRUE)
  #Volume weighted average age-to-age factor
  round(c(attr(ata(cum_tri), "vwtd")), 4)
}

tri_function = function(my_data){
  tri_data = select(my_data, dev, treatment_month, amount_paid)
  inc_tri = as.triangle(tri_data, 
                              dev = "dev", 
                              origin = "treatment_month", 
                              value = "amount_paid")
  #cum_tri = incr2cum(inc_tri, na.rm = TRUE)
}

#ata_hy18 = lapply(hy18_splits, FUN = ata_function)
#write.list(ata_hy18, "./output/ata_hy18.csv", t.name = names(ata_hy18))

#ata_fy18 = lapply(fy18_splits, FUN = ata_function)
#write.list(ata_fy18, "./output/ata_fy18.csv", t.name = names(ata_fy18))

ata_hy19 = lapply(hy19_splits, FUN = ata_function)
write.list(ata_hy19, "./output/ata_hy19.csv", t.name = names(ata_hy19))

ata_hy19au = lapply(hy19au_splits, FUN = ata_function)
write.list(ata_hy19au, "./output/ata_hy19au.csv", t.name = names(ata_hy19au))

tri_hy19 = lapply(hy19_splits, FUN = tri_function)
write.list(tri_hy19, "./output/tri_hy19.csv", t.name = names(tri_hy19))

tri_hy19au = lapply(hy19au_splits, FUN = tri_function)
write.list(tri_hy19au, "./output/tri_hy19au.csv", t.name = names(tri_hy19au))

#********************************************************************
#	Run and save triangles
#
#********************************************************************

#Create Triangles
triangle_fun = function(my_data){
	dcast(my_data, treatment_month ~ dev, sum, value.var = "amount_paid")
}

hy19_triangles = lapply(hy19_splits, FUN = triangle_fun)

write.list(hy19_triangles, file = "./output/hy19_triangles.csv", t.name = names(hy19_triangles))

print("Calculations are done!")