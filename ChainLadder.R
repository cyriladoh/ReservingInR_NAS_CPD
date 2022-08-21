#load the chainladder package
library(ChainLadder)

#We will use the RAA data that comes with the chainladder package to illustrate how the package works.

RAA  #Cumulative RAA triangle
cum2incr(RAA) #Convert from cumulative to incremental. Use incr2cum to convert from incremental to cumulative.

plot(RAA/1000,  main = "Claims development by origin year")
plot(RAA/1000,  lattice = T, main = "Claims development by origin year")

ata(RAA) #Age to age factors

# Calculate volume weighted average factors for RAA triangle
n <- ncol(RAA)
f <- sapply(1:(n-1),
            function(i){
              sum(RAA[c(1:(n-i)),i+1])/sum(RAA[c(1:(n-i)),i])
            }
)
f

plot(100*(rev(1/cumprod(rev(f)))), t="b",
     main="Expected claims development pattern",
     xlab="Dev. period", ylab="Development % of ultimate loss")

fullRAA <- cbind(RAA, Ult = rep(0, 10))
for(k in 1:n){
  fullRAA[(n-k+1):n, k+1] <- fullRAA[(n-k+1):n,k]*f[k]
}
fullRAA[,11] <- fullRAA[,10]
round(fullRAA)

sum(fullRAA[,11]-getLatestCumulative(RAA))

currentEval <- getLatestCumulative(RAA)
EstdUlt <- currentEval * rev(LDF) #
# Start with the body of the exhibit
Exhibit <- data.frame(currentEval, LDF = round(rev(LDF), 3), EstdUlt)
# Tack on a Total row
Exhibit <- rbind(Exhibit,
                 data.frame(currentEval=sum(currentEval), LDF=NA, EstdUlt=sum(EstdUlt),
                            row.names = "Total"))
Exhibit
