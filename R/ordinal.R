ordinal <- function(x, ...)
  UseMethod("ordinal")

ordinal.english <- local({
  Cards <- c("one", "two", "three", "four", "five", "six",
             "seven", "eight", "nine", "twelve", "twenty",
             "thirty", "forty", "fifty", "sixty", "seventy",
             "eighty", "ninety")
  
  Ords <- c("first", "second", "third", "fourth", "fifth", "sixth",
            "seventh", "eighth", "ninth", "twelfth", "twentieth",
            "thirtieth", "fortieth", "fiftieth", "sixtieth","seventieth",
            "eightieth", "ninetieth")
  
  function(x, ...) {
    initial <- sub("^(.*) (.*)$", "\\1 ", x)
    final   <- sub("^(.*) (.*)$", "\\2",  x)
    
    initial[initial == final] <- ""
    
    final <- ifelse(final %in% Cards, Ords[match(final, Cards)],
                    paste0(final, "th"))
    
    noquote(paste0(initial, final))
  }            
})

ordinal.numeric <- function(x, ...)
  ordinal.english(english(x, ...))

ordinal.character <- ordinal.english

