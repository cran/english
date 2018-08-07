words <- function(x) {
  as.character(as.english(x))
}

Words <- function(x) {
  w <- as.character(as.english(x))
  substring(w, 0, 1) <- toupper(substring(w, 0, 1))
  w
}
