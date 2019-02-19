\name{as.english}
\alias{as.english}
\alias{english}
\alias{english.numeric}
\alias{english.default}
\alias{english.english}
\alias{[.english}
\alias{rep.english}
\alias{format.english}
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
as.english(x, \dots)
english(x, \dots)
\method{english}{default}(x, \dots)
\method{english}{numeric}(x, UK, \dots)
\method{english}{english}(x, UK, \dots)
\method{[}{english}(x, i)
\method{rep}{english}(x, \dots)
\method{format}{english}(x, \dots)
}
\arguments{
  \item{x}{
    Any numeric object.  If the components are not integers, they are
    rounded.  In the case of \code{english.default} this may be an
    object of any class, but will always result in an error.
  }
  \item{UK}{
    A logical flag, should English-style numbers be given (\code{TRUE})
    or American-style (\code{FALSE})?
    If missing, may be set as an option, \code{options(englisl.UK = TRUE)}
    with, if unset, a default value
    chosen as \code{TRUE} unless the locale in use is guessed to be an
    English locale of the USA.
  }
  \item{\dots}{
    For \code{rep}, and \code{format} arguments passed on to other
    methods.  Ignored in all other cases.
}
  \item{i}{
    Any allowable form of index vector.
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
  See original note by John Fox in the Programmers Niche section of
  \url{https://cran.r-project.org/doc/Rnews/Rnews_2005-1.pdf}.
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
## The default UK setting will depend on the locale:
cat("\n", ifelse(grepl("^(en_us|english_united)",
    tolower(Sys.getlocale("LC_CTYPE"))), "USA", "UK"),
    "English is your default\n")
english(101)       ## UK not given: deduced from options

myUK <- getOption("english.UK",
                  !grepl("^(en_us|english_united)",
                         tolower(Sys.getlocale("LC_CTYPE"))))
options(english.UK = FALSE) ## American spoken here, for now.
as.english(10001001) + (-5):5
options(english.UK = myUK)  ## restore original setting.
}
\keyword{ arith }

