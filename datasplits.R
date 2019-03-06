#This is an R code to import a gigantic Excel Data Set into R for easier manipulation and splits. Also author is @zibusisomasuku (Github), and this wouldn't be done without the gracious assistance of fellow colleagues and StackOverflow!

#Start the timer
library(tictoc)
tic()

#Purge the R Workspace
rm(list = ls())

#Load Libraries
library(readxl)
library(rstudioapi)

#Set Working Directory if running on R Studio
#current_path = rstudioapi::getActiveDocumentContext()$path
#setwd(dirname(current_path ))

#Import NSSA and FMHC Claims File

#"First Sheet for FMHC contains Discipline Codes, so we skip and start on 2."

sheets <- excel_sheets("fmhc.xlsx")
fmhc <- read_xlsx("nssa.xlsx")
for (i in 2:length(sheets)) {
	fmhc <- rbind(fmhc, read_xlsx("fmhc.xlsx", i, skip = 4))
}

#Split Data by Service Types

fmhc_splits <- split(fmhc, fmhc$`SERVICE TYPE`)

#Export Each Service Type to a CSV file

write.csv(fmhc_splits$AMBULANCE,file = "Ambulance.csv")
write.csv(fmhc_splits$DENTAL,file = "Dental.csv")
write.csv(fmhc_splits$`FOREIGN TREATMENT`,file = "Foreign Treatment.csv")
write.csv(fmhc_splits$`GOVT/MISSION HOSPITAL`,file = "Gvt or Mission Hospitals.csv")
write.csv(fmhc_splits$GP,file = "GP.csv")
write.csv(fmhc_splits$OPTICIANS,file = "Opticians.csv")
write.csv(fmhc_splits$PHARMACY,file = "Pharmacy.csv")
write.csv(fmhc_splits$`PRIVATE HOSPITALS`,file = "PH.csv")
write.csv(fmhc_splits$SPECIALIST,file = "Specialists.csv")

#End the timer
toc()