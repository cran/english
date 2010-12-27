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
\method{english}{numeric}(x, ...)
\method{english}{english}(x, ...)
\method{english}{default}(x, ...)
}
\arguments{
  \item{x}{
    Any numeric object.  If the components are not integers, they are
    rounded.  In the case of \code{english.default} this may be an
    object of any class, but will always result in an error.
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
  Bill Venables
}
\seealso{
  \code{\link{as.character.english}}, \code{\link{print.english}}.
}
\examples{
as.english(10001001) + (-5):5
}
\keyword{ arith }

