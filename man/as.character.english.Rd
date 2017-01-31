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
  See original note by John Fox in the Programmers Niche section of
  \url{https://cran.r-project.org/doc/Rnews/Rnews_2005-1.pdf}.
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

