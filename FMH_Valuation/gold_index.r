#******************************************************************************
# Exptrapolating USD Dev Factors for Gold-Index
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
library(reshape2)
library(erer) #for the write.list function

# Sets Working Directory,
# You may need to consider other methods like the here() library for other IDEs
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(here::here("FMH_Valuation")) #Assumes the Root Folder is the Folder Containing FMH Valuation

#******************************************************************************
load("./output/usd_claims.RData") #Load the dataset
load("./output/usd_index_claims.RData") 

val_date <- date(ymd("2019-12-31")) #Date for the current valuation

#Split the data by service types per valuation period

fy2019_gold <- gold_claims %>%
  filter(service_date > date(ymd("2018-12-31"))) %>%
  filter(date_received <= val_date)

fy2019_gold_splits <- split(fy2019_gold, fy2019_gold$service_type)

#Calculate ATA factors
ata_function <- function(my_data){
  tri_data <- select(my_data, dev, treatment_month, amount_paid)
  inc_tri <- as.triangle(tri_data,
                              dev = "dev", 
                              origin = "treatment_month", 
                              value = "amount_paid")
  cum_tri <- incr2cum(inc_tri, na.rm = TRUE)
  #Volume weighted average age-to-age factor
  round(attr(ata(cum_tri), "vwtd"), 5)
}

ata_fy19 <- lapply(fy2019_gold_splits, FUN = ata_function)
write.list(ata_fy19, "./output/ataFactorsGold.csv", t.name = names(ata_fy19))

#Summarize the data by treatment month, currency, service type, amount_paid and development
summarized_gold_index <- dcast(gold_index, treatment_month + service_type + dev ~ ., sum, value.var = "amount_paid")

#Save to CSV
write.csv(summarized_gold_index, file = "./output/SummarisedGoldIndex.csv")

print("Calculations are done!")