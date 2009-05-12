\name{LLdecomp-package}
\Rdversion{1.1}
\alias{LLdecomp-package}
\alias{LLdecomp}
\docType{package}
\title{The main function "decompfit" decomposes discrete variable into cliques and separators.}

\description{The main function "decompfit" takes as input a matrix consisting of discrete
  variables. Decomposes these using Random Forests and the Message
  Passing algorithm into cliques and separators. These can then be used
  to fit a log-linear model. The whole procedure is described in \url{http://arxiv.org/abs/0904.1510}.
}
\details{
\tabular{ll}{
Package: \tab LLdecomp\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-05-12\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
Please note that this is an *early test release*.

The best entry point for the package are the examples in the help
file of the function \code{\link{decompfit}}.
Index:
Index:
\preformatted{
  logilasso                 Fits a loglinear model or/and performs
                            cross-validation
  levelcv                   Performs cross-validation for the specified
                            number of interactions
  traceplot                 Plots the solution path from lambdamax to lambdamin for
                            all components of the solution vector beta
  graphmod                  Plots a graphical model
  plot.logilasso            Plot method for a logilasso object
  predict.logilasso         Predict method for a logilasso object
}
}
\author{
  Corinne Dahinden
  
  Maintainer: Corinne Dahinden <dahinden@stat.math.ethz.ch>

}
\keyword{package}



\references{\url{http://arxiv.org/abs/0904.1510}
}

\examples{
## Data generation where the first and second variables are dependent.
data1 <- matrix(NA,nrow=200,ncol=5)
data1[,-1] <- sample(c(0,1),200*4,replace=TRUE)
prob <- data1[,2]*0.6+0.2
data1[,1] <- rbinom(200,1,prob)

dfit <- decompfit(data1,3)
}
