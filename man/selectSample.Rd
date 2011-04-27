\name{selectSample}
\Rdversion{1.1}
\alias{selectSample}
\title{
Selection of a stratified sample from the frame with srswor method 
}
\description{
Once optimal stratification has been obtained ('outstrata'),and a new frame has been 
built by assigning to the units of the old one the new stratum labels (by means of 
"updateFrame" function, it is possible to select a stratified sample from the frame 
with the srswor method. 
The result of the execution of "selectSample" function is a dataframe containing selected
units, with the probabilities of inclusion.
It is possible to output this dataframe in a .xls file.
One more .xls file is produced ("sampling check"), containing coeherence checks between 
(a) population in frame strata
(b) population in optimised strata
(c) planned units to be selected in optimised strata
(d) actually selected units
(e) sum of weights in each stratum

}
\usage{
selectSample(frame, outstrata, writeFile = "YES")
}
\arguments{
  \item{frame}{
  This is the (mandatory) dataframe containing the sampling frame, as it has been modified 
  by the execution of the "updateFrame" function. 
  Name of stratum variable must be 'strato'.
}
  \item{outstrata}{
  This is the (mandatory) dataframe containing the information related to resulting 
  stratification obtained by the execution of "optimizeStrata" function.
  Name of stratum variable must be 'strato'.
}
  \item{writeFile}{
  Indicates if at the end of the processing the resulting strata will be outputted in a delimited file.
  Default is "YES".
}
}

\value{
A dataframe containing the sample
}

\author{
Giulio Barcaroli, Diego Zardetto
}


\seealso{
  \code{\link{buildStrataDF}} to build the "strata" dataframe containing
                        information on target variables Y's starting from information in a frame or in a 
						previous round of the sample,
  \code{\link{verify}} to analyse and control the stratification
                        resulting by running optimizeStrata,
  \code{\link{updateStrata}} to add new strata labels in the initial strata file,
  \code{\link{updateFrame}} to modify strata labels in the initial frame file,
  \code{\link{optimizeStrata}} to optimise strata in the selection frame,
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
# selection of sample
sample <- selectSample(frame = framenew, outstrata = outstrata, writeFile="YES")
head(sample)
}
}
\keyword{ survey }

