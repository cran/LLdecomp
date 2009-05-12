\name{decompfit}
\Rdversion{1.1}
\alias{decompfit}
\title{Decomposes discrete variable into cliques and separators}
\description{Takes as input a matrix consisting of discrete
  variables. Decomposes these using Random Forests and the Message
  Passing algorithm into cliques and separators. These can then be used
  to fit a log-linear model. The whole procedure is described in \url{http://arxiv.org/abs/0904.1510}.}

\usage{
decompfit(data, smax = 3, ...)
}
\arguments{
  \item{data}{
    Discrete nxp matrix, where columns consist of n measurements of the
    p different variables.
}
\item{smax}{
  Maximal size of a clique. This size has to be adapted, depending on
  how large log-linear models can be fitted subsequent to the decomposition.
}
\item{\dots}{
  Parameters passed to the randomForest function.
}
}
\details{Random Forests is used to measure the association between two
  variables. In terms of a graph, one starts with the whole graph and
  recursively deletes edges between nodes with least association till
  the graph becomes decomposable with the desired maximal clique
  size. Subsequent of having these cliques and separators, a log-linear
  model can be fitted to all of them and these can be combined to form a
  global log-linear model.
}
\value{A list with 2 elements
\item{cliques}{List of cliques.}
\item{separators }{List of separators.}
}
\references{Corinne Dahinden, Peter Buehlmann available at \url{http://arxiv.org/abs/0904.1510}
}
\author{Corinne Dahinden, \email{dahinden@stat.math.ethz.ch} }

\examples{
## Data generation where the first and second variables are dependent.
data1 <- matrix(NA,nrow=200,ncol=5)
data1[,-1] <- sample(c(0,1),200*4,replace=TRUE)
prob <- data1[,2]*0.6+0.2
data1[,1] <- rbinom(200,1,prob)

dfit <- decompfit(data1,3)

}
\keyword{models}
