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
  strata.sample <- function(frame,strata,nh,repl){
  stratodist <- table(frame[,strata])
  stratocum <- c(0, cumsum(stratodist))
  s.stratocum <- c(0, cumsum(nh)) 
  permuta <- rep(NA,sum(nh))
  sapply(1:length(nh), function(i) {
        permuta[(s.stratocum[i] + 1):s.stratocum[i + 1]] <<- (stratocum[i] + sample(stratodist[i],nh[i],replace=repl))
        })
  WEIGHTS <- rep(if (repl==FALSE) stratodist/nh else 1/(1-(1-1/stratodist)^nh),nh) 
  attr(permuta,"WEIGHTS") <- WEIGHTS
  permuta
  }
	colnames(frame) <- toupper(colnames(frame))
	colnames(outstrata) <- toupper(colnames(outstrata))
	outstrata$SOLUZ <- ceiling(outstrata$SOLUZ) # rounding of allocation numbers
	numdom <- length(levels(as.factor(frame$DOMAINVALUE)))
	samptot <- NULL
	chktot <- NULL
	# begin domains cycle
	for (d in (1:numdom)) {
		domframe <- frame[frame$DOMAINVALUE == d,]
		domstrata <- outstrata[outstrata$DOM1 == d,]
		strataord <- domstrata[order(domstrata$STRATO),]
		lista <- domframe
		lista$STRATO <- lista$LABEL
		listaord <- lista[order(lista$STRATO),]
		s <- strata.sample(listaord,c("STRATO"),strataord$SOLUZ,repl=FALSE)
		samp <- data.frame(listaord[s,],WEIGHTS=attr(s,"WEIGHTS"))
		samptot <- rbind(samptot,samp)
        chk <- data.frame(DOMAINVALUE=d,STRATO=strataord$STRATO,Nh_frame=as.vector(table(listaord$STRATO)),Nh_strata=strataord$N,planned_units=strataord$SOLUZ,selected_units=as.vector(table(samp$STRATO)),sum_of_wgts=tapply(samp$WEIGHTS,samp$STRATO,sum))
		chktot <- rbind(chktot,chk)
	}  # end domain cycle
  colnames(samptot) <- toupper(colnames(samptot))
  colnames(chktot) <- toupper(colnames(chktot))
  if (writeFile == "YES") write.table(samptot,"sample.csv",sep=",",row.names=FALSE,col.names=TRUE,quote=FALSE)
  if (writeFile == "YES") write.table(chktot,"sampling check.csv",sep=",",row.names=FALSE,col.names=TRUE,quote=FALSE)
  return(samptot)
}  

