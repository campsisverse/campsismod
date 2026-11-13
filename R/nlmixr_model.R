setClass(
  "nlmixr_model",
  representation(
    ini = "character",
    model = "character"
  )
)

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

#' @rdname export
setMethod("export", signature = c("campsis_model", "nlmixr_type"), definition = function(object, dest, ...) {
  return(
    new(
      "nlmixr_model",
      ini = nlmixr_ini(object),
      model = nlmixr_code(object)
    )
  )
})

#_______________________________________________________________________________
#----                             to_string                                 ----
#_______________________________________________________________________________

indent_lines <- function(x, n) {
  return(ifelse(x == "", x, paste0(strrep(" ", n), x)))
}

#' @rdname to_string
setMethod("to_string", signature = c("nlmixr_model"), definition = function(object, ...) {
  code <- c(
    "function() {",
    "  ini({",
    indent_lines(object@ini, 4),
    "  })",
    "  model({",
    indent_lines(object@model, 4),
    "  })",
    "}"
  )
  return(paste0(code, collapse = "\n"))
})
