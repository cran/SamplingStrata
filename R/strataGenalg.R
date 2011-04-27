########################################################################
# Function: strataGenalg
# Version: 9 August 2010
# Author: Giulio Barcaroli
# Optimise sampling units multivariate allocation with Genetic Algorithm
# together with strata determination
# Implementing full cartesian product solutions
#########################################################################
strataGenalg <- function (errors,
                          strata,
                          cens=NULL,
                          strcens=FALSE,
                          dominio,
                          initialStrata,
                          minnumstr,
                          iter,
                          pops,
                          mut_chance,
                          elitism_rate,
                          addStrataFactor,
                          highvalue,
                          suggestions)
{                          
#options(echo=FALSE)
#--------------------------------------------------------------------------
# Loading genetic algorithm package and functions
fileres <- paste("results",dominio,".txt",sep="")
sink(file=fileres)
#--------------------------------------------------------------------------
nvar=ncol(errors)-1
ndom=nrow(errors)
nstrcamp <- nrow(strata)
if (strcens == TRUE) nstrcens <- nrow(cens)
#--------------------------------------------------------------------------
# Dimension of solutions vector
totcard <- nrow(strata)
# Setting maximum number of strata
#initialStrata <- ceiling(nrow(strata) * numstrataPar)
cat("\n---------------------------------------------")
cat("\nOptimal stratification with Genetic Algorithm")
cat("\n---------------------------------------------")
cat("\n *** Parameters ***")
cat("\n---------------------------")
cat("\nDomain: ",dominio)
cat("\nMaximum number of strata: ",initialStrata)
cat("\nMinimum number of units per stratum: ",minnumstr)
cat("\nTake-all strata (TRUE/FALSE): ",strcens)
if (strcens == TRUE) cat("\nnumber of take-all strata : ",nstrcens)
cat("\nnumber of sampling strata : ",nstrcamp)
cat("\nNumber of target variables: ",nvar)
cat("\nNumber of domains: ",ndom)
cat("\nNumber of GA iterations: ",iter)
cat("\nDimension of GA population: ",pops)
cat("\nMutation chance in GA generation: ",mut_chance)
cat("\nElitism rate in GA generation: ",elitism_rate)
cat("\nChance to add strata to maximum: ",addStrataFactor)
sink()
#--------------------------------------------------------------------------
varloop <- c(1:nvar)
#--------------------------------------------------------------------------
# Preparation of take-all strata
if (strcens == TRUE) {
  vett <- c(rep(1,nrow(cens)))
  censiti <- 1
  cens <- aggrStrata (cens, nvar, vett, censiti, dominio)
  dimcens <- nrow(cens)
}
#--------------------------------------------------------------------------
######################
# Evaluation function
######################
evaluate <- function(indices) {
  soluz <- NULL
  v <- NULL
  dimens <- NULL
  censiti <- 0
  strcor <- aggrStrata (strata, nvar, floor(indices), censiti, dominio)
  dimsamp <- nrow(strcor)
  if (strcens == TRUE) strcor <- rbind(strcor,cens)
  dimens <- nrow(strcor)
#  cat("\nCurrent solution:",indices)
#  cat("\nNumber of input strata:",dimens)
#  cat("\n    ...sampling strata:",dimsamp)
#  cat("\n    ...take-all strata:",dimcens)
#--------------------------------------------------------------------------
# Chiamata funzione allocazione multivariata
#  if (dimens < initialStrata)
  soluz <- bethel (
         	strcor,
        	errors,
        	minnumstr,      
        	printa=FALSE
        	)
  sink()
  sink(file=fileres,append=TRUE)
  ntot <- round(sum(soluz))
#  if (dimens > (initialStrata-1)) ntot <- highvalue
#  cat("\nSolution: ",indices)
  cat("\nNumber of strata:",nrow(strcor)," Sample size:",ntot)
#  cat("\n",floor(indices))
  return(ntot)
#print(paste("Dimensione: ",round(ntot),"   Numero strata: ",dimens))
}
##########################
# Monitoring of processing
##########################
monitor <- function(obj) {
    # plot the population
    minEval = min(obj$evaluations);
    plot(obj,type="hist");
#plot(dimens,round(rbga.results$best[iter]),type="b",main = "",col="blue")
#title(main = list("Best sample sizes vs number of strata", cex=1.5,
#                  col="red", font=2))
}
##############################
# Genetic algorithm execution
##############################
stringMin <- rep(1,totcard)
stringMax <- rep(initialStrata,totcard)
rbga.results <- rbga(
		stringMin,
		stringMax,
		suggestions = suggestions,
		monitorFunc=monitor,
		iters=iter,
		popSize=pops,
		mutationChance=mut_chance,
    elitism_rate,
    addStrataFactor,		
		evalFunc=evaluate,
		verbose=TRUE,
		showSetting=TRUE,
    )
######################
# Results
######################
plot(rbga.results)
title(paste("Domain",dominio," - Sample size",rbga.results$best[iter]),col.main= "red")
summary(rbga.results,echo=TRUE)
#print(paste("Sample size: ",round(rbga.results$best[iter])))
#cat(" *** Sample size: ",round(rbga.results$best[iter]))
#--------------------------------------------------------------------------
# Writing strata corresponding to optimal solution
#--------------------------------------------------------------------------
v <- floor(rbga.results$population[rbga.results$evaluations==min(rbga.results$evaluations),])
if (class(v) == "matrix") v <- as.vector(v[1,])
stmt <- paste("write.table(v,'solution",dominio,".txt',row.names=FALSE,col.names=FALSE,sep='\t',quote=FALSE)",sep="")
eval(parse(text=stmt))
censiti <- 0
strcor <- aggrStrata (strata, nvar, v, censiti, dominio)
if (strcens == TRUE) strcor <- rbind(strcor,cens)
dimens <- nrow(strcor)
soluz <- bethel (
         	strcor,
        	errors,
        	minnumstr,
        	printa=F
        	)
sink()
sink(file=fileres,append=TRUE)
cat("\n *** Sample size: ",sum(soluz))
cat(paste("\n *** Number of strata: ",dimens))
risulta <- cbind(strcor,soluz)
fileout <- paste("outstrata",dominio,".txt",sep="")
write.table(risulta,file=fileout,sep="\t",row.names=FALSE,col.names=TRUE,quote=FALSE)
cat("\n...written output to",fileout)
#proc.time()
sink()
# End function
}
