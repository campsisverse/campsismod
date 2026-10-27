#_______________________________________________________________________________
#----                  covariate_definitions class                          ----
#_______________________________________________________________________________

#'
#' Covariate definitions class.
#'
#' @export
setClass(
  "covariate_definitions",
  contains = "pmx_list",
  prototype = prototype(type = "covariate_definition")
)

#'
#' Create a list of covariate definitions.
#'
#' @return an empty list of covariate definitions
#' @export
CovariateDefinitions <- function() {
  return(new("covariate_definitions"))
}

#_______________________________________________________________________________
#----                                 sort                                  ----
#_______________________________________________________________________________

#' @rdname sort
setMethod("sort", signature = c("covariate_definitions"), definition = function(x, decreasing = FALSE, ...) {
  if (length(x) > 0) {
    names <- x %>% get_names()
    x@list <- x@list[order(names, decreasing = decreasing)]
  }
  return(x)
})

#_______________________________________________________________________________
#----                          export_to_json                               ----
#_______________________________________________________________________________

#' @rdname export_to_json
setMethod("export_to_json", signature = c("covariate_definitions"), definition = function(object, ...) {
  json <- object@list %>%
    purrr::map(function(x) {
      return(export_to_json(x)@data)
    })
  return(JSONElement(json))
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature = c("covariate_definitions"), definition = function(object) {
  cat("Covariates:\n")
  if (length(object) == 0) {
    cat("None\n")
  } else {
    for (element in object@list) {
      show(element)
    }
  }
})
