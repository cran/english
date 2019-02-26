#' Express Numbers in Words
#'
#' Convert numerical objects to Enghish character strings.
#' A convenience function for use mainly with \code{RMarkdown}
#' in-text inserts.  The capitalized version, \code{Words},
#' makes the initial letter of the result upper case.
#'
#' @param x A numeric vector, usually integer
#'
#' @return A character string vector with the numbers expressed
#'         in English words.
#' @export
#'
#' @examples
#' cat("The Duke of York had ", words(10006), " men.\n", sep = "")
#' cat("How many did he have? ", Words(10006), ".\n", sep = "")
words <- function(x) {
  as.character(as.english(x))
}

#' @rdname words
#' @export
Words <- function(x) {
  w <- as.character(as.english(x))
  substring(w, 0, 1) <- toupper(substring(w, 0, 1))
  w
}
