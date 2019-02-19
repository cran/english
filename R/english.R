as.english <- english <- function (x, ...) {
  UseMethod("english")
}

english.default <- function (x, ...)
    stop("no method defined for objects of class ",
         paste(dQuote(class(x)), collapse = ", "))


english.numeric <- english.english <- function (x, UK, ...) {
  if(missing(UK)) {
    UK <- getOption("english.UK",
                    !grepl("^(en_us|english_united)",
                           tolower(Sys.getlocale("LC_CTYPE"))))
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
  # opts <- options(scipen = 100)
  # on.exit(options(opts))
  print(noquote(as.character.english(x)))
  invisible(x)
}

sort.english <- function (x, decreasing = FALSE, ...) {
  structure(NextMethod("sort"), class = "english", useUK = attr(x, "useUK"))
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

as.character.english <- function (x, ...) {
  # opts <- options(scipen = 100)
  # on.exit(options(opts))
  UK <- attr(x, "useUK")
  and <- function (dvec) {
    if(UK && (d <- makeNumber(dvec)) > 0 && d < 100) "and" else ""
  }
  helper <- function (x) {
    # digits <- rev(strsplit(as.character(x), "")[[1]])
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

rep.english <- function (x, ...)
    structure(rep(unclass(x), ...), class = class(x))

`[.english` <- function(x, i) {
    cl <- oldClass(x)
    y <- NextMethod("[")
    oldClass(y) <- cl
    y
}

format.english <- function(x, ...) {
  format(as.character.english(x), ...)
}
