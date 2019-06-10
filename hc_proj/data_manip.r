#Clearing the workspace
rm (list = ls())

#Load Libraries
library(here)
library(readxl)
library(tidyverse)
library(readr)
library(dplyr)
library(data.table)
library(sqldf)

#Set file path from this file
here()

#load the alldata file
global_df = read.csv("./output/alldata.csv")

#import the Discipline Codes and Option Name codes
write_csv(distinct(select(global_df, dis)),"./output/discipline.csv",append = FALSE)
write_csv(distinct(select(global_df, plan_code)),"./output/options.csv",append = FALSE)