#Clear the Workspace
rm(list = ls())

#Load Libraries
library(tidyr)
library(ChainLadder)
library(plyr)
library(data.table)
library(readxl)
library(dplyr)
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
  treatment_month = format(as.Date(service_date), "%Y-%m"))
claims_data = mutate(claims_data, dev = (
  year(date_received)*12 + month(date_received) - (
  year(service_date)*12 + month(service_date))
  )
)
claims_data = filter(claims_data, service_date > ymd("2018-06-30")) #filter for claims after 30 June 2018
claims_data = merge(claims_data, df_dis, by.x = "dis", by.y = "dis", all.x = TRUE, all.y =  FALSE)
claims_data = merge(claims_data, df_optnames, by.x = "option_name", by.y = "option_name", all.x = TRUE, all.y =  FALSE)
final_data = select(claims_data, treatment_month, dev, amount_paid, optname)

#Split the data by service types
claims_splits = split(final_data, claims_data$service_type)

#Calculate IBNR Using BCL
cl_function = function(my_data){
  inc_tri = as.triangle(my_data, 
                              dev = "dev", 
                              origin = "treatment_month", 
                              value = "amount_paid")
  cum_tri = incr2cum(inc_tri)
  plot(cum_tri,
     main = "Paid Losses vs Development by Accident Month",
     xlab = "Maturity in Months", 
     ylab = "Paid Losses",
     lattice = TRUE)
  cl_estimate = chainladder(cum_tri) #Determine the CL estimates.
  full_tri = predict(cl_estimate)
  ult_val = c(full_tri[,ncol(full_tri)])
  current_ev = getLatestCumulative(cum_tri, na.values = NULL)
  ibnr = ult_val - current_ev
  atu_factor = ult_val/current_ev
  ibnr_calc = data.frame(current_ev, atu_factor, ult_val, ibnr)
  ibnr_calc = rbind(ibnr_calc, data.frame(
                      current_ev = sum(current_ev),
                      atu_factor = NA,
                      ult_val = sum(ult_val),
                      ibnr =sum(ibnr),
                      row.names = "Total"))
  mack_est = MackChainLadder(cum_tri, est.sigma = "Mack")
  summary(mack_est)
  plot(mack_est, lattice = TRUE)
  CDR(mack_est)
}

all_results = lapply(claims_splits, FUN = cl_function)

all_results


mergedTriangle = as.triangle(final_data, 
                              dev = "dev", 
                              origin = "treatment_month", 
                              value = "amount_paid")
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