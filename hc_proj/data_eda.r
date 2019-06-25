#Clearing the workspace
rm (list = ls())
claims_data = readRDS("./output/2013-18 final data.rds")

#Load Libraries
library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(inspectdf)
library(DataExplorer)

#Set Working Directory on R Studio
#You may need to consider other methods like the here() library for other IDEs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

plot_str(claims_data)
plot_missing(claims_data)
plot_histogram(claims_data)
plot_density(claims_data)
plot_bar(claims_data)
