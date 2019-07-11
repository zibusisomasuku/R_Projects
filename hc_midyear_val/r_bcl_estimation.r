#Clear the Workspace
rm(list = ls())

#Load Libraries
library(reshape)
library(lubridate)
library(tidyr)
library(ChainLadder)
library(plyr)
library(data.table)
library(data.table)
library(readxl)
library(janitor)
library(ggplot2)
library(dplyr)


#Set Working Directory on R Studio, **You may need to consider other methods like the here() library for other IDEs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Importing the dataset
file_list = list.files("./Claims", pattern = "*.xlsx", full.names = TRUE, recursive =  TRUE)
claims_list = lapply(file_list, read_xlsx)
claims_data = do.call(rbind,claims_list) %>%
              drop_na(ncol = 1)
claims_data = clean_names(claims_data, case = "snake") %>%
              mutate(amount_paid = paid_from_risk_amt + paid_from_savings)
              mutate(treatment_month = str_c(year(service_date), month(service_date, label = TRUE, abbr = TRUE), sep = " "))


xl_file <- "FMHCClaims.xlsx"
wb <- loadWorkbook(xl_file)
sheets <- openxlsx::getSheetNames(xl_file) #check that all sheets have loaded

df <- read.xlsx("FMHCClaims.xlsx", 1)

for (x in 2:10) {
  df <- rbind(df, read.xlsx("FMHCClaims.xlsx", sheet=sheets[x]))
}
#### data frame loaded!

## Make some data sumaries!
summarydf<- df %>%
  select(OPTION.NAME, MAJOR.GROUP, Total.Paid) %>%
group_by(OPTION.NAME, MAJOR.GROUP)
## Create Overall Summary of Claims data by Plan
summarybyplan<-ddply(summarydf,.( Final.Product), summarise, TotalPaid = sum(Total.Paid), TotalCounts= length(Total.Paid))
TotalPaidClaim<-sum(summarybyplan$TotalPaid, na.rm=TRUE)
TotalPaidCount<-sum(summarybyplan$TotalCounts, na.rm=TRUE)
summarybyplan$TotalPaid[is.na(summarybyplan$TotalPaid)]<-0
summarybyplan$TotalCounts[is.na(summarybyplan$TotalCounts)]<-0
summarybyplan<-cbind(summarybyplan, PercentageAmount=c((summarybyplan$TotalPaid/TotalPaidClaim)*100), PercentageCount=c((summarybyplan$TotalCounts/TotalPaidCount)*100))
summarybyplan$PercentageAmount<-as.numeric(round(summarybyplan$PercentageAmount,2))
summarybyplan$PercentageCount<-as.numeric(round(summarybyplan$PercentageCount,2))
print(summarybyplan)

##Create sumary of claims data by service type (major group)

summarybyservice<-ddply(summarydf,.( MAJOR.GROUP), summarise, TotalPaid = sum(Total.Paid), TotalCounts= length(Total.Paid))
TotalPaidClaim<-sum(summarybyservice$TotalPaid)
TotalPaidCount<-sum(summarybyservice$TotalCounts)
summarybyservice<-cbind(summarybyservice, PercentageAmount=c((summarybyservice$TotalPaid/TotalPaidClaim)*100), PercentageCount=c((summarybyservice$TotalCounts/TotalPaidCount)*100))
summarybyservice$PercentageAmount<-as.numeric(round(summarybyservice$PercentageAmount,2))
summarybyservice$PercentageCount<-as.numeric(round(summarybyservice$PercentageCount,2))
print(summarybyservice)

##summary of total claims data by month
monthlysummarydata<- df %>%
  select(OPTION.NAME, Treatment.Month, Total.Paid, MAJOR.GROUP) %>%
  group_by(Final.Product, MAJOR.GROUP, Treatment.Month)
summarybymonth<-ddply(monthlysummarydata,.( Treatment.Month), summarise, TotalPaid = sum(Total.Paid), TotalCounts= length(Total.Paid))
summarybymonth<-cbind(summarybymonth, PercentageAmount=c((summarybymonth$TotalPaid/TotalPaidClaim)*100), PercentageCount=c((summarybymonth$TotalCounts/TotalPaidCount)*100))
summarybymonth$PercentageAmount<-as.numeric(round(summarybymonth$PercentageAmount,2))
summarybymonth$PercentageCount<-as.numeric(round(summarybymonth$PercentageCount,2))
print(summarybymonth)

with(sum,pie(sum$Percentage, labels=paste0(as.character(Final.Product), " ", sum$Percentage, "%"), radius=1))

###PLOTS###
###plot of paid claims paid by plan
ggplot(data = sum, aes(x = Final.Product, y = Total, fill = Final.Product)) + 
     geom_bar(stat = 'identity', position = 'dodge')
##summary of claim counts by plan
count<-ddply(summarydf,.( Final.Product), summarise, Total.Counts = length(Total.Paid)) 
ggplot(data = count, aes(x = Final.Product, y = Total.Counts, fill = Final.Product)) + 
  geom_bar(stat = 'identity', position = 'dodge')

### split data into practice types
listofdata<-split(df, df$MAJOR.GROUP)
### calculate IBNR using BCL
my.function=function(my.data){
  
  triangulationdata=my.data[,c(8,12,14)]
  triangle<-as.triangle(triangulationdata, origin="Treatment.Month", dev="Reporting.Lag", value="Total.Paid")
  cum.triangle=incr2cum(triangle, na.rm=TRUE)
  cum.triangle=cum.triangle[,apply(!is.na(cum.triangle),2,any)] 
  Fulltriangle<- predict(chainladder(cum.triangle))
  Ult<-c(Fulltriangle[,ncol(Fulltriangle)])
  CurrentEv<-getLatestCumulative(cum.triangle, na.values=NULL)
  IBNR=Ult-CurrentEv
  ATU=Ult/CurrentEv
  IBNRExhibit<-data.frame(CurrentEv, ATU, Ult, IBNR)
  IBNRExhibit<-rbind(IBNRExhibit,
                     data.frame(CurrentEv=sum(CurrentEv), ATU=NA, Ult=sum(Ult), IBNR=sum(IBNR),row.names="Total"))
  
  
}
for(i in 1:length(listofdata)){
  my.function(my.data=listofdata[[i]])
}
results.of.all.data.sets <- lapply(listofdata, FUN=my.function)
##print BCL IBNR results
results.of.all.data.sets
### endofBCLcalcs