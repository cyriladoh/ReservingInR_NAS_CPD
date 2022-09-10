
#---------------------------------------
# title: "Creating incremental triangle for valuation from Claims data"
# authors: "Cyril Adoh"
# date: "December, 18 2018"
#---------------------------------------

#library needed for Claims Data manipulation and Triangle Creation
library(tidyverse)
library(lubridate)
library(zoo)
library(ChainLadder)
library(openxlsx)

val_date = ymd(20211231)
options(digits = 15) #Precision: 15 decimal places


#Clean data and add some variables
local_clmdata <- as_tibble(local_clmdata)

#Clean data and add some variables
local_clmdata_renamed <- rename(local_clmdata, plan = "TypeOfPlan", notificationdate = "DateOfClaimsNotification",  claimpaid = "TotalAmount",accidentdate = "DateCareWasAccessed",paymentdate = "DateOfClaimsPayment")

local_clmdata_var <- mutate(local_clmdata_renamed, accyear = year(accidentdate), accmonth_val = month(accidentdate), accmonth = month(accidentdate, label = TRUE, abbr = TRUE), notificationyear = year(notificationdate), notificationmonth_val  = month(notificationdate), notificationmonth = month(notificationdate, label = TRUE, abbr = TRUE), paymentyear = year(paymentdate), paymentmonth_val = month(paymentdate), paymentmonth = month(paymentdate, label = TRUE, abbr = TRUE), accyearmon = paste(accyear,accmonth,sep="-"), accmonyear = paste(accmonth,accyear,sep="-"),  paymentyearmon = paste(paymentyear,paymentmonth,sep = "-"), notificationyearmon = paste(notificationyear,notificationmonth,sep = "-"), devmonth = round(12*as.numeric(as.yearmon(paymentyearmon,"%Y-%b")-as.yearmon(accyearmon,"%Y-%b")))+1, devyr = paymentyear - accyear)

# create lag variables
local_clmdata_var <- mutate(local_clmdata_var, incur2pay_lag = round(as.numeric(ymd(paymentdate)-ymd(accidentdate))/30, digits = 3), incur2report_lag = round(as.numeric(ymd(notificationdate)-ymd(accidentdate))/30, digits = 3), report2pay_lag = round(as.numeric(ymd(paymentdate)-ymd(notificationdate))/30, digits = 3))
rm(local_clmdata_renamed)

#summary statistics
summary(local_clmdata_var)

#reconciliation
local_clmdata_var %>% group_by(paymentyear, plan) %>% summarise(clm = sum(claimpaid, na.rm = T)) 

source("~/Desktop/ReservingInR_NAS_CPD/Reserving_App/ReservingScripts/CoreTriangleCode.R")

#Create incremental triangle
#first the required columns
tri_df <- local_clmdata_var %>% filter(plan == "Product1")

tri_plan1 <- GetIncrementalTriangle(tri_df$accidentdate, tri_df$paymentdate, tri_df$notificationdate, tri_df$claimpaid, CohortType = "Monthly", TriangleType = "Paid", ValueType = "Amount", TriangleShape = "Wide")
