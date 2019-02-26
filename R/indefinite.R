#' Add Indefinite Article
#'
#' Adds an initial indefinite article "a" or "an" to a numerical
#' object expressed either as digits or as words in the result.
#' The capitalized form, \code{Initial}, capitalizes the initial
#' letter: "An" or "A".
#'
#' @param n either a numeric vector or a ordinal character string.
#' @param words logical: should the numbers be expressed as words (TRUE) or digits (FALSE)?
#' @param ... Extra arguments, currently ignored
#'
#' @return A character string vector with an article prepended, either capitalized or not.
#' @export
#'
#' @examples
#' indefinite(1:12)
#' paste0(Indefinite(1:12, FALSE), "-stage process")
#' cat(paste(Indefinite(ordinal(1:10)), " point is ...\n", sep = ""))
indefinite <- function(n, ...) {
  UseMethod("indefinite")
}

#' @rdname indefinite
#' @export
indefinite.numeric <- function(n, words = TRUE, ...) {
  en <- as.character(as.english(n))
  paste(ifelse(substring(en, 0, 1) == "e", "an", "a"),
        if(words) en else n)
}

#' @rdname indefinite
#' @export
indefinite.english <- function(n, words = TRUE, ...) {
  en <- as.character(n)
  paste(ifelse(substring(en, 0, 1) == "e", "an", "a"),
        if(words) en else n)
}

#' @rdname indefinite
#' @export
indefinite.ordinal <- function(n, ...) {
  paste(ifelse(substring(n, 0, 1) == "e", "an", "a"), n)
}

#' @rdname indefinite
#' @export
indefinite.character <- indefinite.ordinal

#' @rdname indefinite
#' @export
Indefinite <- function(n, ...) {
  en <- indefinite(n, ...)
  substring(en, 0, 1) <- toupper(substring(en, 0, 1))
  en
}
