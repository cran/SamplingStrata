\name{checkInput}
\Rdversion{1.1}
\alias{checkInput}
\title{
Check of the inputs to the package: dataframes "errors", "strata" and "sampling frame"
}
\description{
This functions checks the internal structure of the different input dataframes ("errors", "strata" and "sampling frame"), 
and also the correctness of the relationships among them.
 
}
\usage{
checkInput(errors=NULL, strata=NULL, sampframe=NULL)
}
\arguments{
  \item{errors}{
  This is the dataframe containing the precision levels expressed in terms of Coefficients of Variation
  that estimates on target variables Y's of the survey must comply. 
}
  \item{strata}{
  This is the dataframe containing the information related to strata.
}
  \item{sampframe}{
  This is the dataframe containing the information related to all the units belonging to the population of interest.
}
}

\author{
Giulio Barcaroli
}


\seealso{
  \code{\link{buildStrataDF}} to build the "strata" dataframe containing
                        information on target variables Y's starting from information in a frame or in a 
						previous round of the sample,
  \code{\link{optimizeStrata}} to optimise strata in the selection frame,
  \code{\link{updateStrata}} to add new strata labels in the initial strata file,
  \code{\link{updateFrame}} to modify strata labels in the initial frame file,
  \code{\link{selectSample}} to select a stratified sample from the frame with srswor method,
}  
\examples{

library(SamplingStrata)
data(swisserrors)
data(swissstrata)
data(swissframe)
checkInput(swisserrors,swissstrata,swissframe)

}
\keyword{ survey }

