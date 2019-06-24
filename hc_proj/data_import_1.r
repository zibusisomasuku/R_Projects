#Clearing the workspace
rm (list = ls())

#Load Libraries
library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(tictoc)

#Set Working Directory on R Studio
#You may need to consider other methods like the here() library for other IDEs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Procedure (done individiually to optimize RAM usage)
#1. Complile file lists that make the year
#2. Create a list of data tables by file for that year
#3. Combine the list of data tables into one data table
#4. remove #NA rows (ones with totals and the likes)
#5. Save the files by
#6. Clear the workspace

#Year 2013
tic()
files_2013 = list.files("./excel_claims/2013", full.names = TRUE, pattern =  "*.xlsx")
list_2013 = lapply(files_2013, read_xlsx)
data_2013 = rbindlist(list_2013, use.names = TRUE, fill = TRUE)
data_2013 = drop_na(data_2013, ncol = 1)
saveRDS(data_2013, "./output/2013 data.rds")
toc()
rm (list = ls())

#Year 2014
tic()
files_2014 = list.files("./excel_claims/2014", full.names = TRUE, pattern =  "*.xlsx")
list_2014 = lapply(files_2014, read_xlsx)
data_2014 = rbindlist(list_2014, use.names = TRUE, fill = TRUE)
data_2014 = drop_na(data_2014, ncol = 1)
saveRDS(data_2014, "./output/2014 data.rds")
toc()
rm (list = ls())

#Year 2015
tic()
files_2015 = list.files("./excel_claims/2015", full.names = TRUE, pattern =  "*.xlsx")
list_2015 = lapply(files_2015, read_xlsx)
data_2015 = rbindlist(list_2015, use.names = TRUE, fill = TRUE)
data_2015 = drop_na(data_2015, ncol = 1)
saveRDS(data_2015, "./output/2015 data.rds")
toc()
rm (list = ls())

#Year 2016
tic()
files_2016 = list.files("./excel_claims/2016", full.names = TRUE, pattern =  "*.xlsx")
list_2016 = lapply(files_2016, read_xlsx)
data_2016 = rbindlist(list_2016, use.names = TRUE, fill = TRUE)
data_2016 = drop_na(data_2016, ncol = 1)
saveRDS(data_2016, "./output/2016 data.rds")
toc()
rm (list = ls())

#Year 2017 and 2018
tic()
files_2017 = list.files("./excel_claims/2017", full.names = TRUE, pattern =  "*.xlsx")
files_2018 = list.files("./excel_claims/2018", full.names = TRUE, pattern =  "*.xlsx")
files_2017_18 = (c(files_2017, files_2018))
list_2017_18 = lapply(files_2017_18, read_xlsx)
data_2017_18 = rbindlist(list_2017_18, use.names = TRUE, fill = TRUE)
data_2017_18 = drop_na(data_2017_18, ncol = 1)
saveRDS(data_2017_18, "./output/2017-18 data.rds")
toc()
rm (list = ls())
