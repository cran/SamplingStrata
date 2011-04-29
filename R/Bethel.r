###############################################################
# Function BETHEL definition                                  #
# Multivariate optimal allocation for different domains       #
# of interest in stratified sample design                     #
# Extension of Bethel methodology with Chromy Algorithm       #
# see Bethel(1989)"Sample Allocation in Multivarate Surveys"  #
#                  - Survey Methodology                       #
# Version 2.2: 30.5.                                          #
# Author: Daniela Pagliuca                                    # 
###############################################################
bethel <- function (
          stratif,
		  errors, 		  
          minnumstrat=2,
          epsilon=10^(-11),
          maxiter=200,
          printa=FALSE
          ) 
# Begin body          
          {

#--------------------------------------------------------------#------------------------------------------------------------
# Initialization of parameters. Attribution of initial values
#------------------------------------------------------------

  
  iter1<-0  
  maxiter1<-25

  val=NULL
  m <- NULL
  s <- NULL
  cv=NULL                          

#------------------------------
# Initialization of parameters 
#------------------------------

  nstrat=nrow(stratif)                
  nvar=ncol(errors)-1
  ndom=nrow(errors)

  varloop <- c(1:nvar)
  strloop <- c(1:nstrat)
  domloop <- c(1:ndom)

#---------------------------------------------------------
# Initial Data Structures - selection from input variables 
#---------------------------------------------------------

# means
  med   <-as.matrix(stratif[,names(stratif) %in% 
                    sapply(1:nvar, function(i) paste("M",i,sep=""))])

# variances estimates 
  esse  <-as.matrix(stratif[,names(stratif) %in% 
                    sapply(1:nvar, function(i) paste("S",i,sep=""))])

# domains 
  nom_dom <-sapply(1:ndom, function(i) paste("DOM",i,sep="")) 
  dom     <-as.vector(stratif[,names(stratif) %in% nom_dom])

# populations
  N <- as.vector(stratif$N)

# vectors cens and cost
  cens  <- as.vector(stratif$cens) 
  cost  <- as.vector(stratif$cost)
    nocens=1-cens  

# numbers of different domains of interest (numbers of modalities/categories 
# for each type of domain)
  if (ndom ==1) (nvalues<-nlevels((as.factor(stratif$DOM1))))  
  if (ndom>1) {nvalues<-sapply(nom_dom, function(vari) 
                  {val<-c(val, nlevels(as.factor(dom[,vari])))})}

#-------------------------------------------------
#  Building means (m) and deviations matrices (s) 
#  for different domains of interest
#-------------------------------------------------

#-------------------------------------------------
# disjunctive matrix
#-------------------------------------------------
crea_disj=function(data,vars){
  out=NULL
  sapply(vars, function(vari){ out<<-
  cbind(out,diag(nlevels(as.factor(data[,vari])))[as.factor(data[,vari]),] ) })                            
  out }

disj<-crea_disj(stratif,nom_dom)

nc  <- ncol(disj)
#  (m) and (s) 
for(i in 1:nc) 
   { m <- cbind(m, disj[,i]*med )
     s <- cbind(s, disj[,i]*esse ) }

#-------------------------------------------------------------
# computation of the coefficients of variation CVs 
# for different domains of interest
#-------------------------------------------------------------
for (k in domloop) 
 {cvx <- as.matrix( errors[k,names(errors) %in% 
          sapply(1:nvar, function(i)   paste("CV",i,sep=""))])
     ndomvalues <- c(1:nvalues[k])      
     for (k1 in ndomvalues) { cv <- cbind(cv,cvx)  }    
    }

#------------------------------------------------------------
# New definition of initial values 
#------------------------------------------------------------

  nvar_orig<-nvar

  nvar <- ncol(cv)         # new numbers of variables 
  varloop <- c(1:nvar)   

  NTOT <- c(rep(0,nvar))
  CVfin <- c(rep(0,nvar))
  varfin <- c(rep(0,nvar))
  totm <- c(rep(0,nvar))

#------------------------------------------------------------
# Calculation of aij - matrix of standardized precision units    
#------------------------------------------------------------
crea_a=function(){
  numA <- (N**2)*(s**2)*nocens

  denA1<-colSums(t(t(N*m)*c(cv)))^2 
  denA2<-colSums(N*(s**2)*nocens)

  denA<-denA1+denA2+epsilon 

  a<-t(t(numA)/denA)                     
return(a) 
}

#-----------------------------------------------------------
# Computation of alfa's values - Chromy Algorithm Iteration 
#-----------------------------------------------------------

chromy=function(alfatot,diff,iter, alfa, alfanext, x)
{
  while ( diff > epsilon && iter<maxiter ) 
  {
  iter <- iter + 1

  den1 =sqrt(rowSums(t( t(a)*c(alfa)) ))              
  den2=sum(sqrt(rowSums(t(t(a*cost)*c(alfa)))))

  x<-sqrt(cost)/(den1*den2+ epsilon)
       
  alfatot <- sum( c(alfa)*(t(a)%*%x)**2 )   
  alfanext <- c(alfa)*(t(a)%*%x)**2/alfatot           

  diff <- max(abs(alfanext-alfa))
  alfa <- alfanext  
  }

# Allocation vector
n <- ceiling(1 / x)

return(n)
}

a<-crea_a()
n<-chromy(0,999,0,c(rep(1/nvar,nvar)), c(rep(0,nvar)), array(0.1,dim=c(nstrat,1)))

# check n>N  
contx<-sum(n>N) 
cens[n>N]<-1
nocens=1-cens

#check n<minnumstr
for (i in strloop) 
  { if (n[i] < minnumstrat) { n[i] <- min(minnumstrat, N[i])} }


####ITERATIONS###
while (contx>0 && iter1<maxiter1)
{
  iter1=iter1+1

  a<-crea_a()
  n<-chromy(0,999,0,c(rep(1/nvar,nvar)), c(rep(0,nvar)), array(0.1,dim=c(nstrat,1)))

  # check n>N  
  contx<-sum(n>N) 
  cens[n>N]<-1
  nocens=1-cens

for (i in strloop) 
  { if (n[i] < minnumstrat) { n[i] <- min(minnumstrat, N[i])} }
}

#-------------------------
#Definitive best allocation
#-------------------------
n=(nocens*n)+(cens*N)
#-------------------------

#-------------------------------------------------------------
# Populations in strata for different domains of interest NTOTj    
#-------------------------------------------------------------
  for (j in varloop)  { NTOT[j] <- sum((m[,j]>0)*N)  }  
#------------------------------------------------------------
# Computation of the CVs  
#------------------------------------------------------------
varfin=rowSums(t((s*N)**2*(1-round(n)/N)/round(n) ) / NTOT**2)
totm  =rowSums(t(m*N))

CVfin <- round(sqrt(varfin/(totm/NTOT)**2),digits=4)

if (printa == TRUE) {
#---------------------
# Printing allocation
#---------------------

for (i in strloop) {
print (paste("Stratum: ",i, "  Population: ",N[i], "  Sample Units: ",round(n[i])))
}
print(paste("Total sample size : ",round(sum(n))))

#---------------------
# Printing CV's
#---------------------
domcard <- c(rep(0,ndom))
for (k in (1:ndom))  {
  statement <- paste(" domcard[",k,"] <- length(levels(as.factor(stratif$DOM",k,")))",sep="")
  eval(parse(text=statement))
  }
j <- 0
for (k in (1:ndom)) {
  valloop <- c(1:(domcard[k]))
  for (k1 in valloop) {
    for (k2 in 1:(ncol(errors)-1)) {
      j <- j + 1
      if (j == 1) print ("Domain/Var.  Planned CV   Actual CV  ")
       print(paste(k,k1,"/",k2,"       ",cv[j],"       ",CVfin[j]))
}}}
}
return(n)
# End body
}