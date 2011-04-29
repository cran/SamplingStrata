\name{errors.chk}
\Rdversion{1.1}
\alias{errors.chk}
\docType{data}
\title{
Precision constraints to be used after strata optimisation for checking results
}
\description{
Dataframe containing precision levels (expressed in terms of acceptable CV's).
To be used only by function 'verify' once the strata optimisation has been
executed (function 'optimizeStrata')
}
\usage{data(errors)}
\format{
  The constraint data frame (errors) contains a row per each domain value with the following variables: 
  \describe{
    \item{DOM}{
    Type of domain code (factor)
    } 
    \item{CV1}{
    Planned coefficient of variation for first variable Y1 (numeric)
    } 
    \item{CVj}{
    Planned coefficient of variation for j-th variable Yj (numeric)
    }
    \item{CVn}{
    Planned coefficient of variation for last variable Yn (numeric)
    } 
  }
}
\details{
Note: the names of the variables must be the ones indicated above
}

\examples{
## data(errors.chk)
## errors.chk
}
\keyword{datasets}
