as.english <- english <- function (x, ...) {
  UseMethod("english")
}


english.default <- function (x, ...)
    stop("no method defined for objects of class ",
         paste(dQuote(class(x)), collapse = ", "))


english.numeric <- english.english <- function (x, UK, ...) {
  if(missing(UK)) {
    UK <- !grepl("^(en_us|english_united)",
                  tolower(Sys.getlocale("LC_CTYPE")))
  } else {
    UK <- as.logical(UK)[1]
    if(is.na(UK))
        stop("Bad specification of the UK flag. Must be logical scalar")
  }
  structure(x, class = "english", useUK = UK)
}

Ops.english <- function (e1, e2) {
  e1 <- unclass(e1)
  if (!missing(e2))
      e2 <- unclass(e2)
  structure(NextMethod(.Generic), class = "english", useUK = attr(e1, "useUK"))
}

as.numeric.english <- function(x, ...) {
  x <- unclass(x)
  attr(x, "useUK") <- NULL
  x
}

print.english <- function (x, ...) {
  print(noquote(as.character.english(x)))
  invisible(x)
}

sort.english <- function (x, decreasing = FALSE, ...) {
  structure(NextMethod("sort"), class = "english", useUK = attr(x, "useUK"))
}

as.character.english <- local({
  ones <- structure(c("", "one", "two", "three", "four", "five", "six",
                      "seven", "eight", "nine"),
                  .Names = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
  suffixes <- c("thousand", "million", "billion", "trillion", "quadrillion",
                "quintillion", "sextillion", "septillion")
  teens <- structure(c("ten", "eleven", "twelve", "thirteen", "fourteen",
                       "fifteen", "sixteen", "seventeen", "eighteen",
                       "nineteen"),
                     .Names = c("0", "1", "2", "3", "4", "5", "6",
                                "7", "8", "9"))
  tens <- structure(c("twenty", "thirty", "forty", "fifty", "sixty",
                      "seventy", "eighty", "ninety"),
                    .Names = c("2", "3", "4", "5", "6", "7", "8", "9"))

  makeNumber <- function (n)
    as.numeric(paste(n, collapse = ""))
  trim <- function (text)
    sub("^ *", "", sub(" *$", "", gsub("  +", " ", text)))
  
  function (x, ...) {
    UK <- attr(x, "useUK")
    and <- function (dvec) {
      if(UK && (d <- makeNumber(dvec)) > 0 && d < 100) "and" else ""
    }
    helper <- function (x) {
      digits <- rev(strsplit(as.character(x), "")[[1]])
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
    
    opts <- options(scipen = 100)
    on.exit(options(opts))
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
})
