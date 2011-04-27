\name{updateStrata}
\Rdversion{1.1}
\alias{updateStrata}
\title{
Attribution of new labels to atomic strata on the basis of the optimised strata aggregation 
}
\description{
Once optimal stratification has been obtained ('outstrata') it is important to attribute 
new strata labels to each atomic stratum.
By executing this function, a new dataframe "newstrata" will be obtained with the same structure of the old, 
("strata") but with the addition of a new stratum label.
By indicating "YES" to "writeFile" parameter, the dataframe "newstrata" will be outputted in a delimited file ("newstrata.txt").
Also a second delimited file ("strata_aggregation.txt") will be outputted, containing relations bewteen atomic and aggregated strata. 
}
\usage{
updateStrata(strata, writeFile = "YES")
}
\arguments{
  \item{strata}{
  This is the (mandatory) dataframe containing the information related to the atomic strata to which the 
  optimisation has been applied to. 
}
  \item{writeFile}{
  Indicates if at the end of the processing the resulting strata will be outputted in a delimited file.
  Default is "YES".
}
}

\value{
A dataframe containing the strata
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
  \code{\link{optimizeStrata}} to optimise strata in the selection frame,
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
# updating sampling strata with new strata labels
newstrata <- updateStrata(strata)
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
# updating sampling strata with new strata labels
newstrata <- updateStrata(swissstrata)
}
}
\keyword{ survey }

