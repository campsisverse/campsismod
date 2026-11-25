#_______________________________________________________________________________
#----                         compartment class                             ----
#_______________________________________________________________________________

#'
#' Compartment class.
#'
#' @slot name compartment name, as used in the ODE (e.g. 'CENTRAL', or
#'   'A_CENTRAL' in legacy models), NA for unnamed compartments (A_<index>)
#' @slot index compartment index
#' @export
setClass(
  "compartment",
  representation(
    name = "character",
    index = "integer"
  ),
  contains = "pmx_element",
  prototype = prototype(name = as.character(NA)),
  validity = function(object) {
    return(expect_one_for_all(object, c("name", "index")))
  }
)

#'
#' Create a compartment.
#'
#' @param index compartment index
#' @param name compartment name, as used in the ODE (e.g. 'CENTRAL', or
#'   'A_CENTRAL' in legacy models), NA for unnamed compartments (A_<index>)
#' @return an empty list of compartments
#' @export
Compartment <- function(index, name = NA) {
  return(new("compartment", index = as.integer(index), name = as.character(name)))
}

#_______________________________________________________________________________
#----                              get_name                                  ----
#_______________________________________________________________________________

#' @rdname get_name
setMethod("get_name", signature = c("compartment"), definition = function(x) {
  return(paste0("A", "_", x@index))
})

#_______________________________________________________________________________
#----                               show                                    ----
#_______________________________________________________________________________

setMethod("show", signature = c("compartment"), definition = function(object) {
  cat(paste0(object %>% to_string(), " (CMT=", object@index, ")"))
})

#_______________________________________________________________________________
#----                             to_string                                 ----
#_______________________________________________________________________________

#' @rdname to_string
setMethod("to_string", signature = c("compartment"), definition = function(object, ...) {
  if (is.na(object@name)) {
    return(paste0("A", "_", object@index))
  } else {
    return(object@name)
  }
})
