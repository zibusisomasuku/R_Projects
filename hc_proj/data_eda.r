#Clearing the workspace
rm (list = ls())
claims_data = readRDS("./output/2013-18 data.rds")

#Load Libraries
library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(inspectdf)

#Set Working Directory on R Studio
#You may need to consider other methods like the here() library for other IDEs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

