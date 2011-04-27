#--------------------------------------------------------------------------
# Function for selecting a sample
# on the basis of the result of optimal stratification and allocation
# Authors: Giulio Barcaroli, Diego Zardetto
#--------------------------------------------------------------------------
selectSample <- function(
      frame ,
      outstrata ,
      writeFile = "YES"
      )
{
  strata.sample <- function(frame,strata,nh,rep){
  stratodist <- table(frame[,strata])
  stratocum <- c(0, cumsum(stratodist))
  s.stratocum <- c(0, cumsum(nh)) 
  permuta <- rep(NA,sum(nh))
  sapply(1:length(nh), function(i) {
        permuta[(s.stratocum[i] + 1):s.stratocum[i + 1]] <<- (stratocum[i] + sample(stratodist[i],nh[i],rep=rep))
        })
  weights <- rep(if (rep==FALSE) stratodist/nh else 1/(1-(1-1/stratodist)^nh),nh) 
  attr(permuta,"weights") <- weights
  permuta
  }
	numdom <- length(levels(as.factor(frame$domainvalue)))
	samptot <- NULL
	chktot <- NULL
	# begin domains cycle
	for (d in (1:numdom)) {
		domframe <- frame[frame$domainvalue == d,]
		domstrata <- outstrata[outstrata$DOM1 == d,]
		strataord <- domstrata[order(domstrata$strato),]
		lista <- domframe
		lista$strato <- lista$label
		listaord <- lista[order(lista$strato),]
		s <- strata.sample(listaord,c("strato"),strataord$soluz,rep=FALSE)
		samp <- data.frame(listaord[s,],weights=attr(s,"weights"))
		samptot <- rbind(samptot,samp)
        chk <- data.frame(domainvalue=d,strato=strataord$strato,Nh_frame=as.vector(table(listaord$strato)),Nh_strata=strataord$N,planned_units=strataord$soluz,selected_units=as.vector(table(samp$strato)),sum_of_wgts=tapply(samp$weights,samp$strato,sum))
		chktot <- rbind(chktot,chk)
	}  # end domain cycle
  if (writeFile == "YES") write.table(samptot,"sample.xls",sep="\t",row.names=FALSE,col.names=TRUE,quote=FALSE)
  if (writeFile == "YES") write.table(chktot,"sampling check.xls",sep="\t",row.names=FALSE,col.names=TRUE,quote=FALSE)
  return(samptot)
}  

