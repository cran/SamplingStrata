#--------------------------------------------------------------------------
# Cycling function for genetic algorithm
# GA-based optimal stratafication and optimal allocation
# Author: Giulio Barcaroli
#--------------------------------------------------------------------------
optimizeStrata <- function ( 
    errors , 
    strata , 
    cens = NULL,
    strcens = FALSE,
	alldomains = TRUE,
	dom = NULL,
    initialStrata = 3000,
    addStrataFactor = 0.01,    
    minnumstr = 2,
    iter = 20,
    pops = 20,
    mut_chance = 0.05,
    elitism_rate = 0.2,
    highvalue = 100000000,
    suggestions = NULL,
	realAllocation = FALSE,
    writeFile = "YES")
{ 
# Check
#checkInput(errors,strata)             
#--------------------------------------------------------------------------
# Parameters
#--------------------------------------------------------------------------
colnames(errors) <- toupper(colnames(errors))
colnames(strata) <- toupper(colnames(strata))
erro <- split(errors,list(errors$DOMAINVALUE))
stcamp <- split(strata,list(strata$DOM1))
if (strcens == TRUE) colnames(strcens) <- toupper(colnames(strcens))
if (strcens == TRUE) stcens <- split(cens,list(cens$DOM1))
ndom <- length(levels(as.factor(strata$DOM1)))
# begin alldomains = TRUE
if (alldomains == TRUE) {
for (i in 1:ndom) {
  erro[[i]] <- erro[[i]][,-ncol(errors)]
  if (strcens == TRUE) cens <- stcens[[i]]
#  statement <- paste("solut <- read.table('solutionNew",i,".txt',header=TRUE)",sep="")
#  eval(parse(text=statement))
#  sugg <- matrix(solut$stratonew,nrow=20,ncol=nrow(solut),byrow=TRUE)
  strataGenalg (errors=erro[[i]],
                strata=stcamp[[i]],
                cens=cens,
                strcens,
                dominio=i,
                initialStrata,
                minnumstr,
                iter,
                pops,
                mut_chance,
                elitism_rate,
                addStrataFactor,
                highvalue,
                suggestions,
				realAllocation
                )
  }

outstrata <- read.delim("outstrata1.txt")
colnames(outstrata) <- toupper(colnames(outstrata))
out <- NULL
if (ndom > 1) {
  for (i in 2:ndom) {
    statement <- paste('out<-read.delim("outstrata',i,'.txt")',sep="")
    eval(parse(text=statement))
    outstrata <- rbind(outstrata,out)
    }
  }
}
# end alldomains = TRUE
# begin alldomains = FALSE
if (alldomains == FALSE) {
  if (dom < 1 | dom > ndom) stop ("\nInvalid value of the indicated domain\n")
  i <- dom
  erro[[i]] <- erro[[i]][,-ncol(errors)]
  if (strcens == TRUE) cens <- stcens[[i]]
#  statement <- paste("solut <- read.table('solutionNew",i,".txt',header=TRUE)",sep="")
#  eval(parse(text=statement))
#  sugg <- matrix(solut$STRATONEW,nrow=20,ncol=nrow(solut),byrow=TRUE)
  strataGenalg (errors=erro[[i]],
                strata=stcamp[[i]],
                cens=cens,
                strcens,
                dominio=i,
                initialStrata,
                minnumstr,
                iter,
                pops,
                mut_chance,
                elitism_rate,
                addStrataFactor,
                highvalue,
                suggestions,
				realAllocation
                )
  stmt <- paste("outstrata <- read.delim('outstrata",i,".txt",sep="")
  eval(parse(text=stmt))
  }

# end alldomains = FALSE
dimens <- sum(outstrata$SOLUZ)
if (writeFile == "YES") write.table(outstrata,file="outstrata.txt",sep="\t",row.names=FALSE,col.names=TRUE,quote=FALSE)
cat("\n *** Sample size : ",dimens)
cat("\n *** Number of strata : ",nrow(outstrata))
cat("\n---------------------------")
cat("\n...written output to outstrata.txt")
sink()
return(outstrata)
}
#End function
                    