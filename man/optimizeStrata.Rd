\name{optimizeStrata}
\Rdversion{1.1}
\alias{optimizeStrata}
\title{
Best stratification of a sampling frame for multipurpose surveys
}
\description{
This function runs a set of other functions to optimise the stratification of a sampling frame 
}
\usage{
	optimizeStrata(
	errors , 
	strata , 
	cens = NULL, 
	strcens = FALSE, 
	initialStrata = 3000, 
	addStrataFactor = 0.01, 
	minnumstr = 2, 
	iter = 20, 
	pops = 20, 
	mut_chance = 0.05, 
	elitism_rate = 0.2,
	highvalue = 1e+08, 
	suggestions = NULL,
	writeFile = "YES"
	)
}
\arguments{
  \item{errors}{
  This is the (mandatory) dataframe containing the precision levels expressed in terms of Coefficients of Variation
  that estimates on target variables Y's of the survey must comply 
}
  \item{strata}{
  This is the (mandatory) dataframe containing the information related to "atomic" strata, i.e. the strata obtained by 
  the Cartesian product of all auxiliary variables X's. Information concerns the identifiability of strata
  (values of X's) and variability of Y's (for each Y, mean and standard error in strata) 
}
  \item{cens}{
  This the (optional) dataframe containing the takeall strata, those strata whose units must be selected in 
  whatever sample. It has same structure than "strata" dataframe
}
  \item{strcens}{
  Flag (TRUE/FALSE) to indicate if takeall strata do exist or not. Default is FALSE
}
  \item{initialStrata}{
  This is the initial limit on the number of strata for each solution. Default is 3000
}
  \item{addStrataFactor}{
  This parameter indicates the probability that at each mutation the number of strata may increase with 
  respect to the current value. Default is 0.01 (1%).
}
  \item{minnumstr}{
  Indicates the minimum number of units that must be allocated in each stratum. Default is 2
}
  \item{iter}{
  Indicated the maximum number of iterations (= generations) of the genetic algorithm. Default is 20
}
  \item{pops}{
  The dimension of each generations in terms of individuals. Default is 50
}
  \item{mut_chance}{
  Mutation chance: for each new individual, the probability to change each single chromosome, i.e. one bit
  of the solution vector. High values of this parameter allow a deeper exploration of the solution space,
  but a slower convergence, while low values permit a faster convergence, but the final solution can be
  distant from the optimal one. Default is 0.05 
}
  \item{elitism_rate}{
  This  parameter indicates the rate of better solutions that must be preserved from one generation
	to another. Default is 0.2 (20%).
}
  \item{highvalue}{
  Parameter for genetic algorithm. Not to be changed
}
  \item{suggestions}{
  Optional parameter for genetic algorithm that indicates one possible solution 
  (maybe from previous runs) that will be introduced in the initial population. 
  Default is NULL.
}
  \item{writeFile}{
  Indicates if at the end of the processing the resulting strata will be outputted in a delimited file.
  Default is "YES".
}
}

\value{
A dataframe containing strata
}
\references{
Ballin M., Barcaroli G. (2008) - 'Optimal stratification of 
sampling frames in a multivariate 
and multidomain sample design', Contributi Istat n.10/2008,  
http://www.istat.it/dati/pubbsci/contributi/contributi2008.html
}
\author{
Giulio Barcaroli
}

\seealso{
  \code{\link{buildStrataDF}} to build the "strata" dataframe containing
                        information on target variables Y's starting from information in a frame or in a 
						previous round of the sample,
  \code{\link{verify}} to analyse and control the stratification
                        resulting by running optimizeStrata,
  \code{\link{updateStrata}} to add new strata labels in the initial strata file,
  \code{\link{updateFrame}} to modify strata labels in the initial frame file,
  \code{\link{selectSample}} to select a stratified sample from the frame
                        with srswor method,
}  
   


\examples{
#
# This is a toy example, and can be run
#
library(SamplingStrata)
data(errors)
data(strata)
# optimisation of sampling strata
outstrata <- optimizeStrata ( 
    errors = errors, 
    strata = strata, 
    cens = NULL,
    strcens = FALSE,
    initialStrata = 3000,
    addStrataFactor = 0.01,    
    minnumstr = 2,
    iter = 30,
    pops = 20,
    mut_chance = 0.05,
    elitism_rate = 0.2,
    highvalue = 100000000,
    suggestions = NULL,
	writeFile = "YES")
head(outstrata)
#
# The following example is realistic, but is time consuming
#
\dontrun{
library(SamplingStrata)
data(swisserrors)
data(swissstrata)
outstrata <- optimizeStrata ( 
    errors = swisserrors, 
    strata = swissstrata, 
    cens = NULL,
    strcens = FALSE,
    initialStrata = 3000,
    addStrataFactor = 0.01,    
    minnumstr = 2,
    iter = 60,
    pops = 20,
    mut_chance = 0.05,
    elitism_rate = 0.2,
    highvalue = 100000000,
    suggestions = NULL,
	writeFile = "YES")
head(outstrata)
}
}
\keyword{ survey }

