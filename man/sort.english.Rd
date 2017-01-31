\name{sort.english}
\alias{sort.english}
\title{
A sorting method for objects of class english
}
\description{
  A method for the generic function \code{sort} which allows numeric
  objects of class \code{english} to be sorted.
}
\usage{
\method{sort}{english}(x, decreasing = FALSE, ...)
}
\arguments{
  \item{x}{
    An object of class \code{english}.
}
  \item{decreasing}{
    logical: should the object be sorted in decreasing order?
}
  \item{\dots}{
    Not currently used.
}
}
\details{
The method temporarily removes the \code{english} class, sorts the
object components and reinstates the \code{english} class to the result.
}
\value{
An object of class \code{english} with its components in numerically
sorted order.
}
\references{
  See original note by John Fox in the Programmers Niche section of
  \url{https://cran.r-project.org/doc/Rnews/Rnews_2005-1.pdf}.
}
\author{
  John Fox and Bill Venables
}
\seealso{
  \code{\link{english}}.
}
\examples{
set.seed(1010)
x <- english(sample(1:100, 5))
noquote(matrix(as.character(x)))
noquote(matrix(as.character(sort(x))))
}
\keyword{ arith }

