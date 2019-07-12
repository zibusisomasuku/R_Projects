#Clear the Workspace
rm(list = ls())

#Load Libraries
library(reshape)
library(tidyr)
library(ChainLadder)
library(plyr)
library(data.table)
library(data.table)
library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(janitor)

#Set Working Directory on R Studio, **You may need to consider other methods like the here() library for other IDEs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Importing Auxiliary Datasets
df_optnames = read_xlsx("plancodes.xlsx")
df_dis = read_xlsx("discipline.xlsx")

#Importing the datasets
#2018 Files
files_2018 = list.files("./claims_18", pattern = "*.xlsx", full.names = TRUE)
claims_18 = lapply(files_2018, read_xlsx)
claims_data18 = do.call(rbind, claims_18)
claims_data18 = drop_na(claims_data18, ncol = 1)
claims_data18 = clean_names(claims_data18, case = "snake")
claims_data18 = mutate(claims_data18, amount_paid = paid_from_risk_amt + paid_from_savings)
claims_data18 = select(claims_data18, member_no, service_date, dis, date_received, amount_paid, option_name)
claims_data18$date_received = ymd(claims_data18$date_received)

#2019 Files
files_2019 = list.files("./claims_19", pattern = "*.xlsx", full.names = TRUE)
claims_19 = lapply(files_2019, read_xlsx, skip = 4)
claims_19n = ldply(claims_19, dplyr::select, c("MEMBER", "TREATMENT DATE", "DISCIPLINE", "DATE RECEIVED", "AMOUNT", "PRODUCT"))
claims_data19 = drop_na(claims_19n, ncol = 1)
claims_data19 = clean_names(claims_data19, case = "snake")
names(claims_data19)[1:6] = c("member_no", "service_date", "dis", "date_received", "amount_paid", "option_name")
claims_data19$date_received = date(ymd_hms(claims_data19$date_received))

#Merge the two datasets
claims_data = rbind(claims_data18, claims_data19)
claims_data$service_date = ymd(claims_data$service_date)
claims_data$date_received = ymd(claims_data$date_received)

#Data Wrangling
claims_data = mutate(claims_data,
                                treatment_month = str_c(year(service_date), lubridate::month(service_date, label = TRUE), sep = " "))
claims_data = mutate(claims_data, dev = (
  year(date_received)*12 + month(date_received) - (
  year(service_date)*12 + month(service_date))
  )
)

claims_data = merge(claims_data, df_dis, by.x = "dis", by.y = "dis", all.x = TRUE, all.y =  FALSE)
claims_data = merge(claims_data, df_optnames, by.x = "option_name", by.y = "option_name", all.x = TRUE, all.y =  FALSE)

columns_needed = c("treatment_month", "dev", "amount_paid")

#Data Splits
claims_splits = split(claims_data, claims_data$service_type)
column_vector = match(columns_needed, colnames(claims_data))






























#First select the data set
#Select columns needed
# create dev triangles

#Calculate IBNR Using BCL
bcl_estimate = function(my_data){
  loss_run_data = dplyr::select(my_data, column_vector)
  merged_triangle = as.triangle(loss_run_data, 
                              dev = "dev", 
                              origin = "treatment_month", 
                              value = "amount_paid")
  plot(merged_triangle, 
     main = "Paid Losses vs Maturity by Accident Year",
     xlab = "Development in Months", 
     ylab = "Paid Losses")
  merged_triangle[is.na(merged_triangle)] = 0
  cum_triangle = incr2cum(triangle)



}

### calculate IBNR using BCL
my.function=function(my.data){
  
  triangulationdata = my.data[,match(columns_needed, colnames(claims_data))]
  triangle<-as.triangle(triangulationdata, origin="treatment_month", dev="dev", value="amount_paid")
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