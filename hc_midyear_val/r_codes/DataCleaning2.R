library(lubridate)
library(ChainLadder)
library(dplyr)
library(readr)
library(tidyr)
library(zoo)
library(reshape2)
library(tidyverse)
clm2017 <- read_delim("psmas/clm2017.txt", "|", 
                      escape_double = FALSE, col_types = cols(`Cost date` = col_date(format = "%d/%m/%Y"), 
                                                              DOB = col_date(format = "%d/%m/%Y"), 
                                                              Shortfall = col_number(), award = col_number(), 
                                                              fee = col_number(), `first-received` = col_date(format = "%d/%m/%Y"), 
                                                              `payment run date` = col_date(format = "%d/%m/%Y"), 
                                                              `treatment date` = col_date(format = "%d/%m/%Y")), 
                      na = "0", trim_ws = TRUE)
clm2018 <- read_delim("psmas/clm2018.txt", "|", 
                      escape_double = FALSE, col_types = cols(`Cost date` = col_date(format = "%d/%m/%Y"), 
                                                              DOB = col_date(format = "%d/%m/%Y"), 
                                                              Shortfall = col_number(), award = col_number(), 
                                                              fee = col_number(), `first-received` = col_date(format = "%d/%m/%Y"), 
                                                              `payment run date` = col_date(format = "%d/%m/%Y"), 
                                                              `treatment date` = col_date(format = "%d/%m/%Y")), 
                      na = "0", trim_ws = TRUE)
summary(clm2017)
summary(clm2018)
clm2017$Duration <- interval(clm2017$`treatment date`, clm2017$`Cost date`) %/% months(1)
#clm2017b <- subset(clm2017, clm2017$Duration <= 23) 
clm2018$Duration <- interval(clm2018$`treatment date`, clm2018$`Cost date`) %/% months(1)
#clm2018b <- subset(clm2018, clm2018$Duration <= 11)
clm <- rbind(clm2017, clm2018)
summary(clm)
rm(clm2017, clm2017b, clm2018, clm2018b)
clm2 <- subset(clm, clm$`claim status` <= 6)
EndOfYear <- dmy("31-12-2018")
clm2b <- filter(clm2, clm2$`Cost date` <= EndOfYear) 
rm(clm, clm2)
clm2b$TreatmentDate <- format(as.Date(clm2b$`treatment date`), "%m-%Y")
clm2b$newTreatmentDate <- as.yearmon(as.character(clm2b$TreatmentDate), "%m-%Y")
clm3 <- subset(clm2b, select = -c(`membership No`, suffix, claim, `claim status`
                                 , scheme, `claim type`, discipline, tariff, `Cost date`,
                                 `first-received`, `treatment date`, `payment run date`,
                                 fee, Shortfall, DOB, Gender, TreatmentDate))
rm(clm2b)
clm4 <- subset(clm3, clm3$Duration >= 0)
rm(clm3)
mergedTriangle = as.triangle(clm4, 
                              dev = "Duration", 
                              origin = "newTreatmentDate", 
                              value = "award")
mergedTriangle2 <- incr2cum(mergedTriangle)
plot(mergedTriangle2, 
     main = "Paid Losses vs Maturity by Accident Year",
     xlab = "Maturity in Months", 
     ylab = "Paid Losses")
CL<-chainladder(mergedTriangle2) #Determine the CL estimates.
predict(CL) #Should display the completed triangle
mack<-MackChainLadder(mergedTriangle2, est.sigma = "Mack")
summary(mack) 
plot(mergedTriangle2, lattice=TRUE)
plot(mack, lattice=TRUE)
plot(mack)
CDR(mack)