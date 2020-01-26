## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,
                      comment = "")
library(english)

## ---- results = "hold"--------------------------------------------------------
soldiers <- 10006
oldOpt <- options(english.UK = TRUE)
cat("The Duke of York had approximately", words(soldiers), "men.\n")
cat("How many did you say?  ", Words(soldiers), ", approximately.\n", sep = "")

## ---- results = "hold"--------------------------------------------------------
options(english.UK = FALSE)
cat("The Duke of York had approximately", words(soldiers), "men.\n")
cat("How many did you say?  ", Words(soldiers), ", approximately.\n", sep = "")
options(oldOpt)

## -----------------------------------------------------------------------------
days <- 1:6
cat(paste("\nOn the", ordinal(days), "day of Christmas..."))

## -----------------------------------------------------------------------------
steps <- 7:11
cat(paste0("\nThis is ", indefinite(steps), "-step process..."))
cat(paste0("\nThis is ", indefinite(steps, words = FALSE), "-step process..."))
cat(paste0("\n", Indefinite(ordinal(steps)), " step of the process is..."))

## ---- results = "hold"--------------------------------------------------------
numbers <- c(1:10, 1999, 2019)
punct <- c(rep(",", 10), " and",".")
cat(c("In Roman notation:", 
      paste0("\n\t", Words(numbers), " is written as \"",
             utils::as.roman(numbers),"\"", punct)))
cat("\nDoing arithmetic in Roman notation can be difficult.")

## -----------------------------------------------------------------------------
english(100) + (-5):5

