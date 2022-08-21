
#---------------------------------------
# title: "Perform Sensitivity Analysis on the Triangles"
# authors: "Cyril Adoh"
# date: "January, 18 2020
#---------------------------------------

#library needed for Claims Data manipulation and Triangle Creation
library(tidyverse)
library(lubridate)
library(zoo)
library(ChainLadder)

source("../Health R Codes/Reserving/CoreTriangleCode.R")


long_tri = crtLongTriangle(local_clmdata_var)

long_tri1 <- long_tri %>% rename(origin1 = "accmonyear", dev1 = "devmonth", value1 = "claimpaid")

long_tri1 <- long_tri1 %>% mutate(key = paste0(origin1,"-",dev1))
long_tri1 <- long_tri1 %>% select(-c(severity, claimcount))
df <- data.frame()
acc <- unique(long_tri1$origin)
for(i in 0:35){
  dev <- rep(i,(36-i))
  origin <- acc
  df <- rbind(df,data.frame(origin = origin, dev = dev, value2 = 0))
  acc <- acc[-length(acc)]
}
df <- mutate(df, key = paste0(origin,"-",dev))
df1 <- left_join(df, long_tri1, by = "key")
df1 <- df1 %>% mutate(value = ifelse(is.na(value1),value2, value1))
df1$origin1 <- NULL
df1$dev1 <- NULL
df1$key <- NULL
df1$value1 <- NULL
df1$value2 <- NULL




crtTriangle <- function(longTri, origin="origin", dev="dev", value="value", ...){
  
  aggTriangle <- longTri
  names(aggTriangle) <- c(origin, dev, value)
  
  origin_names <- unique(aggTriangle$origin)
  dev_names <-   unique(aggTriangle$dev)
  
  
  # reshape into wide format
  tria <- stats::reshape(aggTriangle, 
                         v.names=value, 
                         timevar = dev, 
                         idvar = origin, 
                         direction = "wide", 
                         new.row.names = origin_names)[, -1]
  
  matrixTriangle <- as.matrix(tria)
  
  names(dimnames(tria)) <- c(origin, dev)
  class(matrixTriangle) <- c("triangle", "matrix")
  dimnames(matrixTriangle)[[1]] <- origin_names
  dimnames(matrixTriangle)[[2]] <- dev_names
  
  #AY <- rownames(tria)
  #tria <- data.frame(AY, tria)
  
  #dimnames(tria)[[1]] <- origin_names
  #dimnames(tria)[[2]] <- c(-1,dev_names)
  
  
  #class(matrixTriangle) <- c("data.frame")
  return(matrixTriangle)
}

tri <- crtTriangle(df1, origin = "origin", dev = "dev", value = "value")
res <- BootChainLadder(incr2cum(tri), R = 10000, process.distr = "od.pois")
mean(res)
quantile(res, c(0.5, 0.6,0.75,0.9,0.95))

