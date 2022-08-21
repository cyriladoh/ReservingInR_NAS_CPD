library(tidyverse)
library(ChainLadder)
library(ggplot2)

source("~/Desktop/ReservingInR_NAS_CPD/Reserving_App/HealthReserving/CoreTriangleCode.R")

#Cumulative triangles
tri_plan1_cum <- incr2cum(tri_plan1, T)
tri_plan2_cum <- incr2cum(tri_plan2, T)

#ata factors
ata_f1 <- ata(tri_plan1_cum, colname.sep = ":")
ata_f2 <- ata(tri_plan2_cum, colname.sep = ":")

#vwtd for plan 1 and 2
f1 <- attr(ata(tri_plan1_cum), "vwtd")
f2 <- attr(ata(tri_plan2_cum), "vwtd")

# 12mon vwtd factor
f1_12 <- round(sapply(lapply(c(1:(ncol(tri_plan1_cum)-1)), lmCL, tri_plan1_cum, 12), coef),3)
f2_12 <- round(sapply(lapply(c(1:(ncol(tri_plan2_cum)-1)), lmCL, tri_plan2_cum, 12), coef),3)

# 24mon vwtd factor
f1_24 <- round(sapply(lapply(c(1:(ncol(tri_plan1_cum)-1)), lmCL, tri_plan1_cum, 24), coef),3)
f2_24 <- round(sapply(lapply(c(1:(ncol(tri_plan2_cum)-1)), lmCL, tri_plan2_cum, 24), coef),3)

#plot 1/f
plot(100*(rev(1/cumprod(rev(f2)))), t="b",
     main="Expected claims development pattern",
     xlab="Dev. period", ylab="Development % of ultimate loss")

# #set ielr for each acc_cohort
# acc_cohorts <- dimnames(tri_plan1_cum)[[1]]
# ielr_s <- rep(0.8, length(acc_cohorts))
# #choose appropriate method for each cohort
# selected_mthd <- c(rep("CL", length(acc_cohorts)))#-2), rep("BF",2))

#res_df_1 <- reserve_df(tri_plan1_cum, 0, 1000, ielr_s, selected_mthd)
#res_df_2 <- reserve_df(tri_plan2_cum, 0, 1000, ielr_s, selected_mthd)

# #check - this will produce same resul as standard chainladder
# mack <- MackChainLadder(tri_plan1_cum)
# plot(mack)
# plot(mack , lattice = T)
# 
# #Mack S.E seem quite high, let bootstrap
# res <- BootChainLadder(tri_plan1_cum, R = 10000, process.distr = "od.pois")
# plot(res)
# 
# #full triangle
# n = ncol(tri_plan1_cum)
# full_tri_plan1 <- tri_plan1_cum
# for(k in 1:n){
#   full_tri_plan1[(n-k+1):n, k+1] <- full_tri_plan1[(n-k+1):n,k]*f1[k]
# }
# 
# res_df