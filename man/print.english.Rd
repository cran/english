\name{print.english}
\alias{print.english}

\title{
  A print method for objects of cleas english
}
\description{
  Provides a \code{print} method for objects of class \code{english}.
}
\usage{
\method{print}{english}(x, ...)
}

\arguments{
  \item{x}{
An object of class \code{english}
}
  \item{\dots}{
    Additional arguments to be passed to the \code{print} generic function.
}
}
\details{
  Coerces the object to character and prints it.
}
\value{
The original object, invisibly.
}
\references{
  See original note by John Fox in the Programmers Niche section of
  \url{https://cran.r-project.org/doc/Rnews/Rnews_2005-1.pdf}.
}
\author{
  John Fox and Bill Venables
}

\seealso{
  \code{\link{as.character.english}}.
}
\examples{
print(english(1:10))
}
\keyword{ arith }

