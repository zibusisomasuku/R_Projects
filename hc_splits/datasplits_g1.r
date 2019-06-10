#This is an R code to import a gigantic Excel Data Set into R for easier manipulation and splits. Also author is @zibusisomasuku (Github), and this wouldn't be done without the gracious assistance of fellow colleagues and StackOverflow!

#Load the libraries and start the timer
library(tictoc)
tic()
library(readxl)
library(rstudioapi)

#Clear the R Workspace
rm(list = ls())

#Set Working Directory if running on R Studio
#current_path = rstudioapi::getActiveDocumentContext()$path
#setwd(dirname(current_path ))

#**************************************************************************************************

#Insert the provider specialties file
specialties <- read_xlsx(choose.files(caption = "Select Provider Specialties"))

#Insert NSSA Files
nssa_jan <- read_xlsx(choose.files(caption = "Select NSSA Jan 2019"), skip = 4, col_names = TRUE)
nssa_feb <- read_xlsx(choose.files(caption = "Select NSSA Feb 2019"), skip = 4, col_names = TRUE)
nssa_mar <- read_xlsx(choose.files(caption = "Select NSSA Mar 2019"), skip = 4, col_names = TRUE)

#Insert FMHC Files
fmhc_jan <- read_xlsx(choose.files(caption = "Select fmhc Jan 2019"), skip = 4, col_names = TRUE)
fmhc_feb <- read_xlsx(choose.files(caption = "Select fmhc Feb 2019"), skip = 4, col_names = TRUE)
fmhc_mar <- read_xlsx(choose.files(caption = "Select fmhc Mar 2019"), skip = 4, col_names = TRUE)

#Combine the Data Sets
nssa <- rbind(nssa_jan, nssa_feb, nssa_mar)
fmhc <- rbind(fmhc_jan, fmhc_feb, fmhc_mar)
global <- rbind(nssa, fmhc)

#Merge with Specialties_TESTING
#fmhcQ1 <- merge(nssa, specialties, by.x = "DISCIPLINE", by.y = "Discipline", all.x = TRUE)

#Merge with Specialties
fmhcQ1 <- merge(global, specialties[, c("DISCIPLINE","Service Type")], by = "DISCIPLINE", all.x = TRUE)

#Split Data by Service Types
fmhc_splits <- split(fmhcQ1, fmhcQ1$`Service Type`)

#Export Each Service Type to a CSV file
write.csv(fmhc_splits$AMBULANCE,file = "Ambulance.csv")
write.csv(fmhc_splits$DENTAL,file = "Dental.csv")
write.csv(fmhc_splits$`FOREIGN TREATMENT`,file = "Foreign Treatment.csv")
write.csv(fmhc_splits$`GOVT/MISSION HOSPITAL`,file = "Gvt or Mission Hospitals.csv")
write.csv(fmhc_splits$GP,file = "GP.csv")
write.csv(fmhc_splits$OPTICAL,file = "Optical.csv")
write.csv(fmhc_splits$PHARMACY,file = "Pharmacy.csv")
write.csv(fmhc_splits$`PRIVATE HOSPITAL`,file = "PH.csv")
write.csv(fmhc_splits$SPECIALIST,file = "Specialists.csv")

#End the timer
toc()