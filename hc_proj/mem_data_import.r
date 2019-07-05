#Clearing the workspace
rm (list = ls())

#Load Libraries
library(readxl)
library(data.table)
library(dplyr)
library(tidyr)

#Set Working Directory on R Studio
#You may need to consider other methods like the here() library for other IDEs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Procedure (done individiually to optimize RAM usage)
#1. Complile file lists that make the year
#2. Create a list of data tables by file for that year
#3. Combine the list of data tables into one data table

#Year 2016
files_2016 = list.files("./excel_membership/2016", full.names = TRUE, pattern =  "*.xlsx")
list_2016 = lapply(files_2016, read_xlsx)
mem_2016 = rbindlist(list_2016, use.names = TRUE, fill = TRUE)

#Year 2017
files_2017 = list.files("./excel_membership/2017", full.names = TRUE, pattern =  "*.xlsx")
list_2017 = lapply(files_2017, read_xlsx)
mem_2017 = rbindlist(list_2017, use.names = TRUE, fill = TRUE)

#Year 2018
files_2018 = list.files("./excel_membership/2018", full.names = TRUE, pattern =  "*.xlsx")
list_2018 = lapply(files_2018, read_xlsx)
mem_2018 = rbindlist(list_2018, use.names = TRUE, fill = TRUE)

#Year 2019
files_2019 = list.files("./excel_membership/2019", full.names = TRUE, pattern =  "*.xlsx")
list_2019 = lapply(files_2019, read_xlsx)
mem_2019 = rbindlist(list_2019, use.names = TRUE, fill = TRUE)

#Save Everything
save(mem_2016, mem_2017, mem_2018, mem_2019, file = "./output/membership.RData")