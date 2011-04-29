\name{buildStrataDF}
\Rdversion{1.1}
\alias{buildStrataDF}
\title{
Building the "strata" dataframe containing information on target variables Y's
distributions in the different strata, starting from sample data or from a frame
}
\description{
This function allows to build the information regarding strata in the population required by
the optimal allocation Bethel algorithm.
In order to estimate means and variables for target variables Y's, we need data coming from:
1. a previous round of the survey whose sample we want to plan;
2. sample data from a survey with variables that are proxy to the ones we are interested to;
3. a frame containing values of Y's variables (or proxy variables) for all the population.
In all cases, each unit in the dataset must contain auxiliary information (X's variables)
and also target variables Y's (or proxy variables) values: under these conditions it is possible 
to build the dataframe "strata", containing information on the distribution of Y's in the different strata 
(namely, means and standard errors), together with information on strata (total population, 
if it is to be censused or not, the cost per single interview).
If the information is contained in a sample dataset, a variable named "WEIGHT" is expected to be 
present. In case of a frame, no such variable is given, and the fucntion will define a WEIGHT variable
for each unit, whose value is always "1".
Missing values for each Y variable will not be taken into account in the computation of means and standard 
deviations (in any case, NA's can be present in the dataset).
The dataframe "strata" is written to an external file (tab delimited, extension "txt"), and will be
used as an input by optimizeStrata.
}
\usage{
buildStrataDF(dataset)
}
\arguments{
  \item{dataset}{
  This is the name of the dataframe containing the sampling data, or frame data. 
  It is strictly required that auxiliary information is organised in variables named 
  as X1, X2, ... , Xm (there should be at least one of them) and the target variables 
  are denoted by Y1, Y2, ... , Yn.
  In addition, in case of sample data, a variable named 'WEIGHT' must be present in the dataframe, 
  containing   the weigths associated to each sampling unit
	}
}
\value{
A dataframe containing strata
}
\author{
Giulio Barcaroli
}

\seealso{
  \code{\link{optimizeStrata}} to optimise strata in the selection frame,
  \code{\link{verify}} to analyse and control the stratification
                        resulting by running optimizeStrata,
  \code{\link{updateStrata}} to add new strata labels in the initial strata file,
  \code{\link{updateFrame}} to modify strata labels in the initial frame file,
  \code{\link{selectSample}} to select a stratified sample from the frame
                        with srswor method,
}  
\examples{

library(SamplingStrata)
data(swissframe)
strata <- buildStrataDF(swissframe)
head(strata)

}
\keyword{ survey }

