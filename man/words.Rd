\name{words}
\alias{words}
\alias{Words}

\title{
Convert integers to words as character strings
}
\description{
Convenience functions mainly for use in R markdown inline code inserts.
\code{words} converts an \code{numeric} or \code{english} object to
character strings; \code{Words} does the same job, but additionally
changes the first letter of the output strings to upper case, as needed
if the R markdown code insert begins a sentence.
}
\usage{
words(x) ## lower case
Words(x) ## capitalized initial letter
}

\arguments{
  \item{x}{
     Either a numerical vector of integers or an \code{english} object.
}
}
\details{
Convenience functions only.  Intended primarily for use in R markdown code inserts.
}
\value{
A character string vector representing the input numerical object in English words.
}

\author{
Bill Venables.
}
\note{
In inline code inserts in R markdown, functions like \code{cat} do not
produce output.  Some \code{print} methods, including \code{print.english}
also do not produce output.  As such inline code inserts require a
character string output only, these convenience functions are designed to
facilitate that process.
}

\seealso{
\code{\link{as.character.english}}, \code{\link{indefinite}}
}
\examples{
  paste("The Duke of York had", words(10006), "men")
  paste("How many?", Words(10006))
}
\keyword{ arith }% use one of  RShowDoc("KEYWORDS")
