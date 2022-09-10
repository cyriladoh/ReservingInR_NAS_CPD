#Begin creating triangle

#Incremental Triangle
GetIncrementalTriangle <- function(lossdates, paymentdates, notificationdates, amountpaid, CohortType = "Annual", TriangleType = "Paid", ValueType = "Amount", TriangleShape = "Long"){
  
  validate <- (length(lossdates) == length(paymentdates)) == (length(notificationdates) == length(amountpaid))
  if(!validate) stop("Error:Data must be of same length")
  
  origin <- GetCohorts(lossdates, CohortType)
  
  if(TriangleType == "Paid"){
    dev <- GevDevPeriod(lossdates, paymentdates, CohortType)
  } else {
    dev <-  GevDevPeriod(lossdates, notificationdates, CohortType)
  }
  
  switch(TriangleShape, 
         Long = {
           res =  GetLongTriangle(origin, dev, amountpaid, ValueType)
         },
         Wide = {
           res = GetWideTriangle(origin, dev, amountpaid, ValueType) 
         })
  
  return(res)
}

#Use this function to get triangle origins
GetCohorts <- function(lossdates, CohortType){
  switch(CohortType,
         Annual = {
           res = year(lossdates)
         },
         BiAnnual = {
           res = paste0(year(lossdates), ifelse(month(lossdates) <= 6, "H1", "H2"))
         },
         Quarterly = {
           res = paste0(year(lossdates), paste0("Q",quarter(lossdates)))
         },
         Monthly = {
           res = as.yearmon(lossdates)
         }
  )
  
  return(res)
}

GevDevPeriod <- function(lossdates, otherdates, CohortType){
  validate = length(lossdates) == length(otherdates)
  if(!validate) stop("Error:Dates must be of same length")
  
  switch(CohortType,
         Annual = {
           res = year(otherdates) - year(lossdates)
         },
         BiAnnual = {
           res = ((year(otherdates)-year(lossdates))*2)+(ceiling(month(otherdates)/6)-ceiling(month(lossdates)/6))
         },
         Quarterly = {
           res = ((year(otherdates)-year(lossdates))*4)+(ceiling(month(otherdates)/3)-ceiling(month(lossdates)/3))
         },
         Monthly = {
           res = ((year(otherdates)-year(lossdates))*12)+(ceiling(month(otherdates))-ceiling(month(lossdates)))
         }
  )
  
  return(res+1)
}

GetLongTriangle <- function(origin, dev, value, ValueType){
  
  validate = (length(origin) == length(dev)) == (length(value) == length(origin))
  if(!validate) stop("Error:Data must be of same length")
  
  longTri <- data.frame(origin = origin, dev = dev, value = value)
  longTri <- longTri %>% group_by(dev, origin) %>% summarise(value = switch (ValueType,
                                                                             Amount = sum(value, na.rm = T),
                                                                             Count = n(),
                                                                             Severity = sum(value, na.rm = T)/n()
  ))
  longTri <- filter(longTri, !is.na(dev)) %>% arrange(dev)
  
  longTri <- longTri %>% rename(origin1 = "origin", dev1 = "dev", value1 = "value")
  longTri <- longTri %>% mutate(key = paste0(origin1,"-",dev1))
  
  #create empty df that will hold the new longTri
  df <- data.frame()
  cohorts <- unique(longTri$origin1)
  n <- length(cohorts)
  for(i in 1:n){
    dev <- rep(i,(n-i+1))
    origin <- cohorts
    df <- rbind(df,data.frame(origin = origin, dev = dev))
    cohorts <- cohorts[-length(cohorts)]
  }
  df <- mutate(df, key = paste0(origin,"-",dev))
  longTri <- left_join(df, longTri, by = "key")
  longTri <- longTri %>% rename(value = "value1")
  longTri <- longTri %>% dplyr::select(-c(origin1, dev1, key))
  longTri <- longTri %>% arrange(dev, origin)
  
  return(longTri)
}


GetWideTriangle <- function(origin, dev, value, ValueType){
  longTri <- GetLongTriangle(origin, dev, value, ValueType)
  aggTriangle <- longTri
  names(aggTriangle) <- c("origin", "dev", "value")
  
  origin_names <- as.character(unique(aggTriangle[, "origin"]))
  dev_names <-   as.character(unique(aggTriangle[, "dev"]))
  
  # reshape into wide format
  tria <- stats::reshape(aggTriangle, 
                         v.names="value", 
                         timevar = "dev", 
                         idvar = "origin", 
                         direction = "wide", 
                         new.row.names = origin_names)[, -1]
  
  matrixTriangle <- as.matrix(tria)
  
  names(dimnames(matrixTriangle)) <- c("origin", "dev")
  
  dimnames(matrixTriangle)[[1]] <- origin_names
  dimnames(matrixTriangle)[[2]] <- dev_names
  
  class(matrixTriangle) <- c("triangle", "matrix")
  return(matrixTriangle)
}

#create df that holds reserves summaries
reserve_df <- function(x, ocr, ep, ielr_s, f, selected_mthd, format = F){
  res_df <- data.frame(acc_cohorts = row.names(x)) %>% mutate(paid_to_date = round(getLatestCumulative(x)), outstanding_reported = round(ocr), incurred_claim = paid_to_date + outstanding_reported, f = cumprod(rev(c(f,1))))
  
  res_df <- res_df %>% mutate(f_inverse =1/f, earned_premium = ep, ielr = ielr_s, CL_Utl = round(paid_to_date*f))
  
  res_df <- res_df %>% mutate(BF_Utl = round((1-f_inverse)*ielr*earned_premium+incurred_claim), LR_Ult = round(ifelse(f == 1, incurred_claim, ielr*earned_premium)))
  
  res_df <- res_df %>% mutate(selected_method = selected_mthd, CL_ibnr = round(CL_Utl - incurred_claim), BF_ibnr = round(BF_Utl - incurred_claim), LR_ibnr = round(min(0,LR_Ult - incurred_claim)), selected_ibnr = ifelse(selected_method == "CL", CL_ibnr, ifelse(selected_method == "BF", BF_ibnr, LR_ibnr)))
  
  if(format){
    res_df <- res_df %>% mutate(paid_to_date = format(round(paid_to_date, digits = 0), big.mark = ",", scientific = F))
    res_df <- res_df %>% mutate(outstanding_reported = format(round(outstanding_reported, digits = 0), big.mark = ",", scientific = F))
    res_df <- res_df %>% mutate(incurred_claim = format(round(incurred_claim, digits = 0), big.mark = ",", scientific = F))
    res_df <- res_df %>% mutate(f = round(f, digits = 3))
    res_df <- res_df %>% mutate(f_inverse = paste0(round(f_inverse*100, digits = 1),"%"))
    res_df <- res_df %>% mutate(earned_premium = format(round(earned_premium, digits = 0), big.mark = ",", scientific = F))
    res_df <- res_df %>% mutate(ielr = paste0(round(ielr*100, digits = 1),"%"))
    res_df <- res_df %>% mutate(CL_Utl = format(round(CL_Utl, digits = 0), big.mark = ",", scientific = F))
    res_df <- res_df %>% mutate(BF_Utl = format(round(BF_Utl, digits = 0), big.mark = ",", scientific = F))
    res_df <- res_df %>% mutate(LR_Ult = format(round(LR_Ult, digits = 0), big.mark = ",", scientific = F))
    res_df <- res_df %>% mutate(CL_ibnr = format(round(CL_ibnr, digits = 0), big.mark = ",", scientific = F))
    res_df <- res_df %>% mutate(BF_ibnr = format(round(BF_ibnr, digits = 0), big.mark = ",", scientific = F))
    res_df <- res_df %>% mutate(LR_ibnr = format(round(LR_ibnr, digits = 0), big.mark = ",", scientific = F))
    res_df <- res_df %>% mutate(selected_ibnr = format(round(selected_ibnr, digits = 0), big.mark = ",", scientific = F))
  }
  
  return(res_df)
}

#set upper triangle with na to zero
upperTri_NoNA <- function(Triangle){
  #set upper triangle with na to zero
  upper <- col(Triangle) <= ncol(Triangle) + 1 - row(Triangle)
  upperna <- which(is.na(Triangle[upper]), arr.ind=TRUE)
  Triangle[upper][upperna] <- 0
  
  return(Triangle)
}

checkWeights <- function(weights, Triangle){
  
  if(is.null(dim(weights))){
    if(length(weights)==1){
      my.weights <- Triangle
      my.weights[!is.na(Triangle)] <- weights
      weights <- my.weights
    }
  }
  
  return(weights)
  
}

lmCL <- function(i, Triangle, period = NULL){
  
  if(is.null(period)) {period <- ncol(Triangle)}
  calPeriods <- (row(Triangle) + col(Triangle) - 1)
  weights <- ifelse(calPeriods <= period, 0, ifelse(calPeriods > ncol(Triangle), NA, 1))
  
  lm(y~x+0, weights=weights[,i]/Triangle[,i],
     data=data.frame(x=Triangle[,i], y=Triangle[,i+1]))
}
