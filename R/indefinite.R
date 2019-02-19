indefinite <- function(n, words = TRUE) {
  en <- as.character(as.english(n))
  paste(ifelse(substring(en, 0, 1) == "e", "an", "a"),
        if(words) en else n)
}