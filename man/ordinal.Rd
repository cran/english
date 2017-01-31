\name{ordinal}
\alias{ordinal}
\alias{ordinal.numeric}
\alias{ordinal.character}
\alias{ordinal.english}
\title{
  Generic functions and methods generating english versions of
  ordinal whole numbers as character strings
}
\description{
  The function \code{ordinal} is generic and works in much
  the same way as the function \code{as.character.english} but
  produces an ordinal, rather than a cardinal number version
}
\usage{
ordinal(x, ...)
\method{ordinal}{numeric}(x, ...)
\method{ordinal}{english}(x, ...)
\method{ordinal}{character}(x, ...)
}
\arguments{
  \item{x}{
    An object to produce english ordinal output.
}
  \item{\dots}{
    Additional arguments passed on to \code{english} for the initial
    conversion.
  }
}
\details{
  The object is first converted to a character string an \code{english}
  object, if necessary, then to character and finally adjusted so that
  it provides the ordinal version of the english representation.
}
\value{
  A character string vector of ordinal numbers in English.
}
\author{
  Anthony Damico
}
\seealso{
  \code{\link{as.character.english}}
}
\references{
  See original note by John Fox in the Programmers Niche section of
  \url{https://cran.r-project.org/doc/Rnews/Rnews_2005-1.pdf}.
}
\examples{
## for UK style English:
ordinal(c(1, 9, 10, 11, 12, 19, 20, 21, 99, 100, 101, 109, 111,
          119, 1000, 1100, 1199, 9999, 10000, 10001), UK = TRUE)
## for USA style English:
ordinal(c(1, 9, 10, 11, 12, 19, 20, 21, 99, 100, 101, 109, 111,
          119, 1000, 1100, 1199, 9999, 10000, 10001), UK = FALSE)
## For mothers of small children:
cat(paste("This is the", ordinal(1:5), "time I've told you!"), sep = "\n")
}
\keyword{ arith }
