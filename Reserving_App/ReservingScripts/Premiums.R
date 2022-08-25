#---------------------------------------
# title: "Computing exposures, EP, UPR, DAC."
# authors: "Cyril Adoh"
# date: "December, 18 2018"
#---------------------------------------

library(tidyverse)
library(lubridate)
library(Rcpp)
library(openxlsx)
library(readxl)

#valuation date
val_date = ymd(20211231) #ymd(ymd(paste0(year(today()),"-",month(today()),"-",01))-1) #2018-11-30
options(digits = 15) #Precision: 15 decimal places

#local_proddata <- as_tibble(local_proddata)

local_proddata_renamed <- rename(local_proddata, commission = "Commission") #rename columns

#create new variables
local_proddata_var <- mutate(local_proddata_renamed, plan = trimws(plan), in_force = ifelse((enddate >= val_date & startdate <= val_date), ifelse(premium < 0,-1,1),0), startyear = year(startdate), startmonth = month(startdate, label = TRUE, abbr = TRUE), startday = day(startdate), endyear = year(enddate), endmonth = month(enddate, label = TRUE, abbr = TRUE), endday = day(enddate), coverperiod = as.numeric((ymd(enddate) - ymd(startdate))+1), startdate = ymd(startdate), enddate = ymd(enddate))

#Summary statistics for reasonableness check
summary(local_proddata_var)

#Reconciliation to external source
#Premium
gwp <- local_proddata_var %>% group_by(prodyear, plan) %>% summarise(gwp = sum(premium, na.rm = T))

#Data is ready, compute EP, UPR and DAC
sourceCpp("~/Desktop/ReservingInR_NAS_CPD/Reserving_App/ReservingScripts/ep.cpp")
system.time(out <- prod_vals(local_proddata_var, val_date))
local_proddata_var <- cbind(local_proddata_var,out)
local_proddata_var <- local_proddata_var %>% mutate(UPR = start_upr+end_upr, DAC= start_uc + end_uc)
rm(local_proddata_renamed, out)

#Get summarized results
#GWP by plan
GWP_Plan <- local_proddata_var %>% filter(prodyear == year(val_date)) %>% group_by(plan) %>% summarise(GWP = sum(premium,na.rm = TRUE))

#Earned Premium (EP by plan) 
EP_StartYear <- local_proddata_var %>% filter(startyear == year(val_date)) %>% group_by(plan) %>% summarise(start_ep = sum(start_ep,na.rm = TRUE))
EP_EndYear <- local_proddata_var %>% filter(endyear == year(val_date)) %>% group_by(plan) %>% summarise(end_ep = sum(end_ep,na.rm = TRUE))
EP_Plan <- full_join(EP_StartYear,EP_EndYear, by = "plan") 
EP_Plan <- mutate(EP_Plan, EP = rowSums(dplyr::select(EP_Plan, ends_with("ep")), na.rm = TRUE))
EP_Plan <- dplyr::select(EP_Plan, -c(start_ep, end_ep))
rm(EP_StartYear, EP_EndYear)

#Unearned Premium Reserve (UPR by plan)
UPR <- local_proddata_var %>% group_by(plan) %>% summarise(UPR = sum(UPR,na.rm = TRUE))

#Deferred Acquisition Cost (DAC by plan)
DAC <- local_proddata_var %>% group_by(plan) %>% summarise(DAC = sum(DAC,na.rm = TRUE))

#Write results to Reserving Model
#path <- #path to xlsx file
#writeData(wb,"I_GWP",GWP_Plan,  startRow = 3, startCol = 3, colNames = T, keepNA = T)
#wb <- loadWorkbook(path)

#writeData(wb,"I_EarnedPremium", EP_Plan,  startRow = 3, startCol = 3, colNames = T, keepNA = T)

#writeData(wb,"I_UPR", UPR,  startRow = 3, startCol = 3, colNames = T, keepNA = T)

#writeData(wb,"I_DAC", DAC,  startRow = 3, startCol = 3, colNames = T, keepNA = T)


#path <- #path to template
#saveWorkbook(wb, path, overwrite = T)


