\name{as.numeric.english}
\alias{as.numeric.english}
\title{
  Convert an english object back to class numeric
}
\description{
  This a convenience function that simply removes the S3 class attribute
  from and object of class \code{english}.
}
\usage{
\method{as.numeric}{english}(x, ...)
}
\arguments{
  \item{x}{
    An object of class \code{english}.
}
  \item{\dots}{
    Currently unused.
}
}

\value{
An object of class \code{numeric}.
}
\author{
  John Fox and Bill Venables.
}
\references{
  See original note by John Fox in the Programmers Niche section of
  \url{https://cran.r-project.org/doc/Rnews/Rnews_2005-1.pdf}.
}
\seealso{
\code{\link{english}}, \code{\link{as.english}}.
}
\examples{
(x <- english(1:10))
as.numeric(x)
}

\keyword{ arith }

