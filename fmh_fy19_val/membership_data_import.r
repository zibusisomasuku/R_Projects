#Clear the Workspace
rm(list = ls())

#Load Libraries
library(tidyr)
library(readr)
library(plyr)
library(readxl)
library(data.table)
library(readxl) #reading Excel files
library(dplyr) #working with data tables
library(lubridate) #Working with dates
library(janitor) #Clean Names
library(reshape2)

#Set Working Directory on R Studio
#**You may need to consider other methods like the here() library for other IDEs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#******************************************************************************
#Importing Auxiliary Datasets
df_plan = read_xlsx("plancodes2.xlsx")
#******************************************************************************

mem_files = list.files("./membership/", pattern = "*.xlsx", full.names = TRUE, recursive = TRUE)
mem_list = lapply(mem_files, read_xlsx, skip = 3)
mem_data = ldply(mem_list, drop_na, 1)
mem_data = clean_names(mem_data, case = "snake")
mem_data$birth_date = ymd(mem_data$birth_date)
mem_data$dep_lapse_date = ymd(mem_data$dep_lapse_date)
mem_data = merge(mem_data, df_plan, by.x = "product_name", by.y = "option_name", all.x = TRUE, all.y =  FALSE)

#Save the master datasets
save(mem_data, file = "./output/mem_data.RData")
write.csv(mem_data, "./output/mem_data.csv")