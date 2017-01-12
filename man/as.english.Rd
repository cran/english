\name{as.english}
\alias{as.english}
\alias{english}
\alias{english.numeric}
\alias{english.default}
\alias{english.english}
\title{
  Generic functions and methods for S3 class english
}
\description{
  The functions \code{as.english} and \code{english} are fully
  equivalent generic constructor functions for the S3 class
  \code{english}, which allows numeric objects to be represented in
  the form of their customary English expression.
}
\usage{
as.english(x, ...)
english(x, ...)
\method{english}{default}(x, ...)
\method{english}{numeric}(x, UK, ...)
\method{english}{english}(x, UK, ...)
}
\arguments{
  \item{x}{
    Any numeric object.  If the components are not integers, they are
    rounded.  In the case of \code{english.default} this may be an
    object of any class, but will always result in an error.
  }
  \item{UK}{
    A logical flag, should English-style numbers be given (\code{TRUE})
    or American style (\code{FALSE})? If missing, a default value is
    chosen as \code{TRUE} unless the locale in use is guessed to be an
    English locale of the USA.
  }
  \item{\dots}{
    Currently unused.
}
}
\details{
  The function \code{english} is a generic constructor function for
  objects of class \code{english}.  The methods either mark the object
  as having the appropriate class, or, in the case of
  \code{english.default}, result in an error message.  The function
  \code{as.english} is provided as a corresponding function to
  \code{as.roman} in the \code{utils} package.  The method
  \code{english.english} simply allows the constructor function to act
  as a coercion and to have no effect on objects already of the class.
}
\value{
  An object of class \code{english}, if possible, or an error message if
  not.
}
\references{
  See original note by John Fox at
  \url{http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html}.
}
\author{
  John Fox and Bill Venables
}
\seealso{
  \code{\link{as.character.english}}, \code{\link{print.english}}.
}
\examples{
english(1010, UK = FALSE)
english(1010, UK = TRUE)
english(101)       ## UK not given: result will depend on the locale
as.english(10001001) + (-5):5
}
\keyword{ arith }

