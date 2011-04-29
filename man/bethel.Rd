\name{bethel}
\Rdversion{1.1}
\alias{bethel}
\title{
Multivariate optimal allocation
}
\description{
Multivariate optimal allocation for different domains of interest in stratified sample design 
}
\usage{
bethel  (
          stratif,
          errors, 		  
          minnumstrat=2,
          epsilon=10^(-11),
          maxiter=200,
          printa=FALSE
          ) 
		  }
\arguments{
  \item{errors}{
 Data frame of coefficients of variation for each domain
}
  \item{stratif}{
 Data frame of survey strata
}
  \item{minnumstrat}{
 Minimum number of units per strata (default=2) 
}
  \item{maxiter}{
Maximum number of iterations (default=25) of the general procedure. 
This kind of iteration may be required by the fact that when in a stratum the number 
of allocated units is greater or equal to its population, that stratum is set as "census stratum", 
and the whole procedure is re-initialised 
}
  \item{printa}{
If TRUE add two attributes to resulting vector.
The first (confr) is a comparison between results obtained with 3 different allocation methods: Bethel, proportional and equal.
The second is a table reporting planned and actual CV and sensitivity 
}
  \item{epsilon}{
Epsilon (default=1e-11)): this value is used to compare the difference in results from one iteration to the other; 
if it it is lower than "epsilon", then the procedure stops 
}
}

\value{
A vector containing the computed optimal allocation
}
\author{
Daniela Pagliuca
}

\examples{

library(SamplingStrata)
data(strata)
data(errors)
n <- bethel(strata, errors, printa=TRUE)
sum(n)

}
\keyword{ survey }

