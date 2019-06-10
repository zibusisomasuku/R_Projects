#Clearing the workspace
rm (list = ls())

#Load Libraries
library(here)
library(readxl)
library(tidyverse)
library(readr)
library(dplyr)

#Set file path from this file
here()

#****LOAD NEWER FORMAT FILES****

#The folder "excel_claims/2016+" houses newer format schedules after the system upgrade

#2016 files
#list all .xlsx files in the folder with newer format schedules
temp = list.files("./excel_claims/2016+", pattern = "*.xlsx", full.names = TRUE)

df = read_xlsx(temp[1])
df = read_xlsx(temp[2])
df = read_xlsx(temp[3])
df = read_xlsx(temp[4])
df = read_xlsx(temp[5])
df = read_xlsx(temp[6])
df = read_xlsx(temp[7])
df = read_xlsx(temp[8])
df = read_xlsx(temp[9])
df = read_xlsx(temp[10])
df = read_xlsx(temp[11])
df = read_xlsx(temp[12])
df = read_xlsx(temp[13])
df = read_xlsx(temp[14])
df = read_xlsx(temp[15])
df = read_xlsx(temp[16])
df = read_xlsx(temp[17])
df = read_xlsx(temp[18])
df = read_xlsx(temp[19])
df = read_xlsx(temp[20])
df = read_xlsx(temp[21])
df = read_xlsx(temp[22])
df = read_xlsx(temp[23])
df = read_xlsx(temp[24])
df = read_xlsx(temp[25])
df = read_xlsx(temp[26])
df = read_xlsx(temp[27])
df = read_xlsx(temp[28])
df = read_xlsx(temp[29])
df = read_xlsx(temp[30])
df = read_xlsx(temp[31])
df = read_xlsx(temp[32])
df = read_xlsx(temp[33])
df = read_xlsx(temp[34])
df = read_xlsx(temp[35])
df = read_xlsx(temp[36])
df = read_xlsx(temp[37])
df = read_xlsx(temp[38])