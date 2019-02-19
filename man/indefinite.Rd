\name{indefinite}
\alias{indefinite}
\title{
Add Indefinite Article
}
\description{
Coerce to character and add the indefinite article, namely 'an' or 'a', depending
on the english form of the number involved.
}
\usage{
indefinite(n, words = TRUE)
}
\arguments{
  \item{n}{
  An integer vector to receive the article.
}
  \item{words}{
  A logical value: should the number be converted to English (TRUE) or left in
  numerical form (FALSE)?
}
}
\details{
  Used mainly in documents to allow the appropriate article to be chosen
  algorithmically.
}
\value{
  A character string vector with the appropriate article attached to each component.
}

\author{
  Based on a suggestion of Anne Pier Salverda.
}

\seealso{
  See \code{\link{Words}} and \code{\link{words}} for including numbers
  in documents.
}
\examples{
indefinite(0:11)
indefinite(0:11, FALSE)
}
\keyword{ arith }
