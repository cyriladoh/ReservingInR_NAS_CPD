library(tidyverse)
library(ChainLadder)
library(ggplot2)

source("~/Desktop/ReservingInR_NAS_CPD/Reserving_App/ReservingScripts/CoreTriangleCode.R")

#Cumulative triangles
tri_plan1_cum <- incr2cum(tri_plan1, T)

#ata factors
ata_f1 <- ata(tri_plan1_cum, colname.sep = ":")

#vwtd for plan 1 and 2
f1 <- attr(ata(tri_plan1_cum), "vwtd")

# 12mon vwtd factor
f1_12 <- round(sapply(lapply(c(1:(ncol(tri_plan1_cum)-1)), lmCL, tri_plan1_cum, 12), coef),3)

# 24mon vwtd factor
f1_24 <- round(sapply(lapply(c(1:(ncol(tri_plan1_cum)-1)), lmCL, tri_plan1_cum, 24), coef),3)

#plot 1/f
plot(100*(rev(1/cumprod(rev(f1)))), t="b",
     main="Expected claims development pattern",
     xlab="Dev. period", ylab="Development % of ultimate loss")

#set ielr for each acc_cohort
acc_cohorts <- dimnames(tri_plan1_cum)[[1]]
ielr_s <- rep(0.89, length(acc_cohorts))
#choose appropriate method for each cohort
selected_mthd <- c(rep("CL", length(acc_cohorts)))#, rep("BF",2))

res_df_1 <- reserve_df(tri_plan1_cum, 0, Historical_EP[,2], ielr_s, f1, selected_mthd)

sum(res_df_1$selected_ibnr)

#check - this will produce same result as standard chainladder
mack <- MackChainLadder(tri_plan1_cum)
mack
plot(mack)
plot(mack , lattice = T)

# Bootstrap
res <- BootChainLadder(tri_plan1_cum, R = 500, process.distr = "od.pois")
plot(res)

#
glm1 <- glmReserve(tri_plan1_cum)


 #full triangle
n = ncol(tri_plan1_cum)
full_tri_plan1 <- tri_plan1_cum
for(k in 1:n){  
  full_tri_plan1[(n-k+1):n, k+1] <- full_tri_plan1[(n-k+1):n,k]*f1[k]
}
# res_df