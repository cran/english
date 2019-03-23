#' Ordinal Numbers
#'
#' Generates character strings of the ordinal version of numbers in English words.
#'
#' @param x A numeric vector, usually integer, or an object of class "ordinal"
#' @param ... Ignored. Included only for compatibility.
#'
#' @return A character string vector of ordinal versions of the number, with S3 class "ordinal"
#' @export
#'
#' @examples
#' ordinal(1:12)
ordinal <- function(x, ...) {
  UseMethod("ordinal")
}

#' @rdname ordinal
#' @export
ordinal.english <- function(x, ...) {
  Cards <- c("one",  "two",    "three", "eight",  "nine")
  Ords <- c("first", "second", "third", "eighth", "ninth")
  initial <- sub("^(.*) (.*)$", "\\1 ", x)
  final   <- sub("^(.*) (.*)$", "\\2",  x)

  initial[initial == final] <- ""

  final <- sub("ty$", "tie", final) ## 20, 30, ...
  final <- sub("ve$", "f"  , final) ## 5 and 12

  final <- ifelse(final %in% Cards, Ords[match(final, Cards)],
                  paste0(final, "th"))

  res <- paste0(initial, final)
  class(res) <- c("ordinal", "character")
  res
}

#' @rdname ordinal
#' @export
ordinal.numeric <- function(x, ...)
  ordinal.english(english(x, ...))

#' @rdname ordinal
#' @export
ordinal.character <- ordinal.english

#' @rdname ordinal
#' @export
print.ordinal <- function(x, ...) {
  print(noquote(unclass(x)))
  invisible(x)
}
