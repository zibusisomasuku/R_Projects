#Clearing the workspace
rm (list = ls())

#Load Libraries
library(openxlsx)

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

#Year 2013 through to 2017
mem_2013 = read.xlsx("./excel_membership/mem_2013.xlsx")
mem_2014 = read.xlsx("./excel_membership/mem_2014.xlsx")
mem_2015 = read.xlsx("./excel_membership/mem_2015.xlsx")
mem_2016 = read.xlsx("./excel_membership/mem_2016.xlsx")
mem_2017 = read.xlsx("./excel_membership/mem_2017.xlsx")
mem_2018 = read.xlsx("./excel_membership/mem_2018.xlsx")

#Save Everything
save(mem_2013,mem_2014,mem_2015,mem_2016,mem_2017, mem_2018, file = "./output/membership.RData")