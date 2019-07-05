#Clearing the workspace
rm (list = ls())
claims_data = readRDS("./output/2013-18 final data.rds")

#Load Libraries
library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(inspectdf)

#Set Working Directory on R Studio
#You may need to consider other methods like the here() library for other IDEs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

show_plot(inspect_types(claims_data))
show_plot(inspect_na(claims_data))
show_plot(inspect_cat(claims_data))
show_plot(inspect_imb(claims_data))
show_plot(inspect_mem(claims_data))
show_plot(inspect_num(claims_data))


