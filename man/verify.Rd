\name{verify}
\Rdversion{1.1}
\alias{verify}
\title{
Analysis and control of stratification resulting by running optimizeStrata
}
\description{
Once stratification has been obtained ('outstrata') it is worth while to analyse the  distribution 
of population units in resulting strata. This can be done by using function 'verify' to
1. visually inspect strata
2. select (by left clicking) outlier strata
3. edit selected strata (by rigth clicking and choosing 'stop' in the graph)
Finally, it is possible to verify that sample allocation is compliant not only 
with the subdomains that have been used to optimise stratification, but also 
with all desired levels of possible domains. To do that, prepare a 'errors.chk.txt' that has the 
same structure than that of 'errors' dataframe, with two differences:
a) the last variable ('domainvalue') must not be present;
b) add one row for the upper domain type (DOM1), while the second row is for DOM2
(that coincides with with the domains used for best stratification) and CV values of the
latter are the corresponding maxima in dataframe 'errors'
 
}
\usage{
verify(errors.chk, strata)
}
\arguments{
  \item{errors.chk}{
  This is the (mandatory) dataframe containing the precision levels expressed in terms of Coefficients of Variation
  that estimates on target variables Y's of the survey must comply. It can coincide with dataframe 'errors'
  but does not contain variable 'domainvalue' and may contain two different domain levels (DOM1 and DOM2)
}
  \item{strata}{
  This is the (mandatory) dataframe containing the information related to resulting strata output of optimizeStrata.
}
}

\value{
A vector containing the sample allocation
}

\author{
Giulio Barcaroli
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
# optimisation of sampling strata
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
# analysis of results
data(swisserrors.chk)
verify(swisserrors.chk,outstrata)
}
}
\keyword{ survey }

