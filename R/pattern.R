#_______________________________________________________________________________
#----                           pattern class                               ----
#_______________________________________________________________________________

#'
#' Pattern class.
#'
#' @export
setClass(
  "pattern",
  representation(),
  contains = "character",
  validity = function(object) {
    if (object %>% as.character() %>% length() != 1) {
      return("Invalid pattern. Pattern must be a single character string.")
    }
    return(TRUE)
  }
)

#'
#' Create a pattern.
#'
#' @param x regular expression
#' @return a pattern
#' @export
Pattern <- function(x) {
  return(new("pattern", x))
}

#_______________________________________________________________________________
#----                         variable_pattern class                        ----
#_______________________________________________________________________________

#'
#' Variable pattern class.
#'
#' @export
setClass(
  "variable_pattern",
  representation(),
  contains = "pattern"
)

#'
#' Create a variable pattern.
#'
#' @param x variable name
#' @return a variable pattern
#' @export
VariablePattern <- function(x) {
  return(new("variable_pattern", x))
}

#_______________________________________________________________________________
#----                             replace_all                                ----
#_______________________________________________________________________________

#' @rdname replace_all
setMethod(
  "replace_all",
  signature = c("character", "variable_pattern", "character"),
  definition = function(object, pattern, replacement, ...) {
    return(gsub(
      paste0("(?<![a-zA-Z0-9_])(", pattern, ")(?![a-zA-Z0-9_])"),
      replacement = replacement,
      x = object,
      perl = TRUE
    ))
  }
)

#' @rdname replace_all
setMethod(
  "replace_all",
  signature = c("character", "pattern", "character"),
  definition = function(object, pattern, replacement, ...) {
    return(gsub(pattern, replacement = replacement, x = object))
  }
)
