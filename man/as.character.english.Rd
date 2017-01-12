\name{as.character.english}
\alias{as.character.english}

\title{
Integers into English
}
\description{
Translates integer vectors into character strings representing their
expression in the English language.
}
\usage{
\method{as.character}{english}(x, ...)
}
\arguments{
  \item{x}{
    A numeric vector of class \code{english}.
  }
  \item{\dots}{
    Currently unused.
}
}
\details{
  Uses a recursive algorithm to translate numbers into their expression in
  English.
}
\value{
  A character string vector
}
\references{
  See original note by John Fox at
  \url{http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html}.
}
\author{
  John Fox, with minor extensions by Bill Venables.
}

\seealso{
  \code{\link{english}}, \code{\link{as.english}},
  \code{\link{print.english}}.
}
\examples{
oneOone <- english(101, UK = FALSE)
as.character(oneOone)
as.character(as.english(oneOone, UK = TRUE))
as.character(oneOone + 1:10)
}
\keyword{ arith }

