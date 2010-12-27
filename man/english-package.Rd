\name{english-package}
\alias{english-package}
\docType{package}
\title{
English
}
\description{
  A simple facility to provide an english language representation of
  integer vectors.
}
\details{
  \tabular{ll}{
    Package: \tab english\cr
    Type: \tab Package\cr
    Version: \tab 1.0-1\cr
    Date: \tab 2010-12-27\cr
    License: \tab GPL-2\cr
    LazyLoad: \tab yes\cr
}
In answer to a question on R-help John Fox provided an elegant R
function to translate integers into English numbers.  The present
package extends this code to an S3 class, with constructor functions and
methods to make this original idea more conveniently available.

The function \code{as.english} is intended to provide a parallel
facility to the function \code{as.roman} in the \code{utils} package.

The main purpose of the package is to present an interesting programming
example rather than to solve a likely real problem, though there could
well be some applications in unusual contexts.
}
\author{
  John Fox and Bill Venables.

  Maintainer: Bill Venables, <Bill.Venables@gmail.com>
}
\references{
  See original note by John Fox at
  \url{http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html}.
}
\keyword{ package }
\seealso{
  \code{\link[utils]{as.roman}}.
}
\examples{
english(1:10)^2 + 1:10
(x <- english(sample(1:100, 10)))
sort(x)
toupper(english(1:10))
}
