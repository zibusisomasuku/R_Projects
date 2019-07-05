#This is an R code to import a gigantic Excel Data Set into R for easier manipulation and splits. Also author is @zibusisomasuku (Github), and this wouldn't be done without the gracious assistance of fellow colleagues and StackOverflow!

#Load the libraries and start the timer
library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(tictoc)
tic()

#Clear the R Workspace
rm(list = ls())

#Set Working Directory if running on R Studio
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))

#******************************************************************************

#Import the provider specialties file
specialties = read.csv("./discipline.csv")

#Import Claim Files
claim_files = list.files("./Claims", full.names = TRUE, pattern =  "*.xlsx")
claim_list = lapply(claim_files, read_xlsx)
claims_data = rbindlist(claim_list, use.names = TRUE, fill = TRUE)
claims_data = drop_na(claims_data, ncol = 1)

#Merge with Specialties
fmhc_midyear = merge(claims_data, specialties, by.x = "DIS", by.y = "dis", all.x = TRUE, all.y = FALSE)

#Writing to CSV Files
write.csv(filter(fmhc_midyear, service_type == "Ambulance"), "./Splits/Ambulance.csv")
write.csv(filter(fmhc_midyear, service_type == "GP"), "./Splits/General Practitioners.csv")
write.csv(filter(fmhc_midyear, service_type == "Specialist"), "./Splits/Specialists.csv")
write.csv(filter(fmhc_midyear, service_type == "Optical"), "./Splits/Optical.csv")
write.csv(filter(fmhc_midyear, service_type == "Govt/Mission Hosp"), "./Splits/Govt or Mission Hospitals.csv")
write.csv(filter(fmhc_midyear, service_type == "Dental"), "./Splits/Dental.csv")
write.csv(filter(fmhc_midyear, service_type == "Private Hosp"), "./Splits/Private Hospitals.csv")
write.csv(filter(fmhc_midyear, service_type == "Foreign Treatment"), "./Splits/Foreign Treatment.csv")
write.csv(filter(fmhc_midyear, service_type == "Pharmacy"), "./Splits/Pharmacy.csv")

#******************************************************************************
toc()
print("File End!")