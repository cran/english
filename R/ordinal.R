ordinal <- function(x, ...)
  UseMethod("ordinal")

ordinal.english <- local({
  Cards <- c("one",  "two",    "three", "eight",  "nine")  
  Ords <- c("first", "second", "third", "eighth", "ninth")
  
  function(x, ...) {
    initial <- sub("^(.*) (.*)$", "\\1 ", x)
    final   <- sub("^(.*) (.*)$", "\\2",  x)
    
    initial[initial == final] <- ""

    final <- sub("ty$", "tie", final) ## 20, 30, ...
    final <- sub("ve$", "f"  , final) ## 5 and 12
    
    final <- ifelse(final %in% Cards, Ords[match(final, Cards)],
                    paste0(final, "th"))
    
    noquote(paste0(initial, final))
  }            
})

ordinal.numeric <- function(x, ...)
  ordinal.english(english(x, ...))

ordinal.character <- ordinal.english

