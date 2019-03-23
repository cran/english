## #' English
## #'
## #' @details
## #' In answer to a question on R-help John Fox provided an elegant R
## #' function to translate integers into English numbers.  The present
## #' package extends this code to an S3 class, with constructor functions and
## #' methods to make this original idea more conveniently available.
## #'
## #' The function \code{as.english} is intended to provide a parallel
## #' facility to the function \code{as.roman} in the \code{utils} package.
## #'
## #' The main purpose of the package is to present an interesting programming
## #' example rather than to solve a likely real problem, though there could
## #' well be some applications in unusual contexts.
## #'
## #' Note added in Version 1.1-4.  The two small helper functions \code{words}
## #' and \code{Words} are included to facilitate inline code inserts in
## #' R markdown files.  See the help files for examples.  The \code{ordinal}
## #' function produces character strings and may be used directly in inline
## #' code inserts.  Use \verb{`r words(10000)`} rather than \verb{`r english(10000)`}
## #' in R markdown files.
## #'
## #' Note added in Version 1.2-0.  The function \code{indefinite} added for
## #' algorithmically including an apprporiate indefinite article in a
## #' document insert.  Based on a suggestion of Anne Pier Salverda.
## #' @keywords internal
#' "_PACKAGE"

#' Numbers in Englis Words
#'
#' Converts numerical vectors into object that display
#' as English words
#'
#' @param x A numerical vector, usually integer.
#' @param UK Logical, Use the UK (English) style (TRUE) or the USA (American) style (FALSE).
#'           The default can be set as \code{options(english.UK = TRUE)}; if unset, a suitable
#'           style is guessed from the user's current locale.
#' @param i Index vector of any kind
#' @param decreasing Logical: should the sorting be in decreasing order?
#' @param ... Additional arguments passed on, currently mostly ignored
#'
#' @return A numerical object that can be printed as English words, or coerced to
#'         character as English words
#' @export
#'
#' @examples
#' english(10000) + (-5):5
#' set.seed(123)
#' (jumble <- english(sample(1:20)))
#' sort(jumble)
#' (x <- english(sample(1:100, 10)))
#' sort(x)
#' toupper(english(1:10))
#' ## For mothers of small children:
#' cat(paste("This is the", ordinal(1:5), "time I've told you!"), sep = "\n")
as.english <- function (x, ...) {
  UseMethod("english")
}

#' @rdname as.english
#' @export
english <- as.english

#' @rdname as.english
#' @export
english.default <- function (x, ...)
    stop("no method defined for objects of class ",
         paste(dQuote(class(x)), collapse = ", "))

#' @rdname as.english
#' @export
english.numeric <- english.english <- function (x, UK, ...) {
  if(missing(UK)) {
    UK <- getOption("english.UK",
                    !grepl("^(en_us|english_united)",
                           tolower(Sys.getlocale("LC_CTYPE"))))
    UK <- isTRUE(UK) || identical(UK, "UK")
  } else {
    UK <- as.logical(UK)[1]
    if(is.na(UK))
        stop("Bad specification of the UK flag. Must be logical scalar")
  }
  structure(x, class = "english", useUK = UK)
}

#' @rdname as.english
#' @export
as.numeric.english <- function(x, ...) {
  x <- unclass(x)
  attr(x, "useUK") <- NULL
  x
}

#' @rdname as.english
#' @export
print.english <- function (x, ...) {
  print(noquote(as.character.english(x)))
  invisible(x)
}

#' @rdname as.english
#' @export
rep.english <- function (x, ...)
  structure(rep(unclass(x), ...), class = class(x))

#' @rdname as.english
#' @export
`[.english` <- function(x, i) {
  cl <- oldClass(x)
  y <- NextMethod("[")
  oldClass(y) <- cl
  y
}

#' @rdname as.english
#' @export
format.english <- function(x, ...) {
  format(as.character.english(x), ...)
}

#' @rdname as.english
#' @export
as.character.english <- function (x, ...) {
  UK <- attr(x, "useUK")
  and <- function (dvec) {
    if(UK && (d <- makeNumber(dvec)) > 0 && d < 100) "and" else ""
  }
  helper <- function (x) {
    digits <- split_digits(x)
    nDigits <- length(digits)
    if (nDigits == 1)
      as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19)
        as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    helper(as.numeric(digits[1]))))
    else if (nDigits == 3)
      trim(paste(ones[digits[3]], "hundred",
                 and(digits[2:1]),
                 helper(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) {
        warning(paste(x, "is too large!"))
        return(as.character(as.vector(x)))
      }
      trim(paste(helper(makeNumber(digits[nDigits:(3 * nSuffix + 1)])),
                 suffixes[nSuffix],
                 and(digits[(3 * nSuffix):1]),
                 helper(makeNumber(digits[(3 * nSuffix):1]))))
    }
  }
  r <- character(length(x))
  bad <- is.na(x) | is.nan(x) | is.infinite(x)
  if (any(!bad & x%%1 != 0)) {
    warning("non-integer values rounded for display")
    x <- round(x)
  }
  if (any(n <- !bad & x < 0))
    r[n] <- paste("minus", sapply(-x[n], helper))
  if (any(z <- !bad & x == 0))
    r[z] <- "zero"
  if (any(p <- !bad & x > 0))
    r[p] <- sapply(x[p], helper)
  r[is.na(x)] <- ""
  r[is.nan(x)] <- "not a number"
  if (any(k <- x < 0 & is.infinite(x)))
    r[k] <- "minus infinity"
  if (any(k <- x > 0 & is.infinite(x)))
    r[k] <- "infinity"
  names(r) <- names(x)
  r
}

#' @rdname as.english
#' @export
sort.english <- function (x, decreasing = FALSE, ...) {
  structure(NextMethod("sort"), class = "english", useUK = attr(x, "useUK"))
}

#' English Arithmetic
#'
#' Allows arithmetic operations on \code{"english"} class objects.  To make sense
#' the operation should return an integer value.
#'
#' @param e1 Numeric vector of object of class \code{"english"}.
#' @param e2 Numeric vector of object of class \code{"english"}.
#'
#' @return Numeric vector of class \code{"english"}
#' @export
#'
#' @examples
#' english(1:10)^2 + 1:10
#' english(100) + (-5):5
Ops.english <- function (e1, e2) {
  e1 <- unclass(e1)
  if (!missing(e2))
    e2 <- unclass(e2)
  structure(NextMethod(.Generic), class = "english", useUK = attr(e1, "useUK"))
}

ones <- c(`0` = "", `1` = "one", `2` = "two", `3` = "three", `4` = "four",
          `5` = "five", `6` = "six", `7` = "seven", `8` = "eight", `9` = "nine")

suffixes <- c("thousand", "million", "billion", "trillion", "quadrillion",
              "quintillion", "sextillion", "septillion")

teens <- c(`0` = "ten", `1` = "eleven", `2` = "twelve", `3` = "thirteen",
           `4` = "fourteen", `5` = "fifteen", `6` = "sixteen", `7` = "seventeen",
           `8` = "eighteen", `9` = "nineteen")

tens <- c(`2` = "twenty", `3` = "thirty", `4` = "forty", `5` = "fifty",
          `6` = "sixty", `7` = "seventy", `8` = "eighty", `9` = "ninety")

makeNumber <- function (n)
  as.numeric(paste(n, collapse = ""))

trim <- function (text)
  sub("^ *", "", sub(" *$", "", gsub("  +", " ", text)))

split_digits <- function(x) {
  d <- character()
  while(x > 0) {
    d <- c(d, x %% 10)
    x <- x %/% 10
  }
  if(length(d) == 0)
    d <- "0"
  d
}
