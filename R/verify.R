verify <-
function(errors.chk, strata) {
attach(strata)
plot(DOM1,soluz,pch=4,col="blue",xlab="Region",ylab="Allocation")
title("Distribution of allocations in strata by domains", 
      sub = "Identify strata by clicking (End=Stop)",
      cex.main = 1.25,   font.main= 4, col.main= "red",
      cex.sub = 1.00, font.sub = 3, col.sub = "red")
v <- identify(DOM1,soluz,labels=strato)
edit(strata[v,])
strata$DOM2 <- strata$DOM1
strata$DOM1 <- c(rep(1,nrow(strata)))
n <- bethel(strata, errors.chk, minnumstrat=2, printa=TRUE)
attr(n,"outcv")
attr(n,"confr")
sum(n)
return(n)
}

