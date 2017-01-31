\name{Ops.english}
\alias{Ops.english}
\title{
Arithmetic operations with objects of class english
}
\description{
Allows arithmetic with objects of class \code{english}.
}
\usage{
\method{Ops}{english}(e1, e2)
}
\arguments{
  \item{e1, e2}{
    Numeric vectors, of which at least one must be of class \code{english}.
}
}
\details{
A standard group generic function that strips off the \code{english}
class from its arguments, invokes the next arithmetic operation method
and reinstates the class \code{english} on the result.
}
\value{
A numeric vector of class \code{english}.
}
\references{
  See original note by John Fox in the Programmers Niche section of
  \url{https://cran.r-project.org/doc/Rnews/Rnews_2005-1.pdf}.
}
\author{
  John Fox and Bill Venables
}

\seealso{
  \code{\link{english}}, \code{\link{as.english}}.
}
\examples{
english(1:10)^2 + 1
}
\keyword{ arith }

