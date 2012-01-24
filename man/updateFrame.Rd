\name{updateFrame}
\Rdversion{1.1}
\alias{updateFrame}
\title{
Modification of the initial frame on the basis of optimised strata aggregation 
}
\description{
Once optimal stratification has been obtained, and new labels have been attributed to initial
atomic strata ("newstrata"), it is important to report the new classification of units in the 
sampling frame by attributing new strata labels to each unit.
By executing this function, a new frame will be obtained with the same structure of the old, 
but the addition of a new stratum label.
It is important that the old frame contains a variable named 'domainvalue' that reports the 
values of the domain that has been used with the 'optimizeStrata' function. If no domains have been
defined, this variable will contains all 1's, but it must exist
 
}
\usage{
updateFrame(frame, newstrata)
}
\arguments{
  \item{frame}{
  This is the (mandatory) dataframe containing the sampling frame. 
}
  \item{newstrata}{
  This is the (mandatory) dataframe containing the information related to the optimisation applied to 
  initial stratification (new labels applied to atomic strata). It is produced by executing 
  the "updateStrata" function. 
}
}

\value{
A dataframe containing the frame
}

\author{
Giulio Barcaroli
}

\examples{
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
# updating sampling frame with new strata labels
data(swissframe)
framenew <- updateFrame(frame=swissframe,newstrata=newstrata)
}
}
\keyword{ survey }

