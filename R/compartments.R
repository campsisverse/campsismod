#_______________________________________________________________________________
#----                        compartments class                             ----
#_______________________________________________________________________________

#'
#' Compartments class.
#'
#' @slot properties compartment properties of the compartments defined in this class
#' @export
setClass(
  "compartments",
  representation(
    properties = "compartment_properties"
  ),
  contains = "pmx_list",
  prototype = prototype(type = "compartment", properties = new("compartment_properties"))
)

#'
#' Create a list of compartments
#'
#' @return an empty list of compartments
#' @export
Compartments <- function() {
  return(new("compartments"))
}

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________

#' @rdname add
setMethod("add", signature = c("compartments", "compartment_property"), definition = function(object, x) {
  object@properties <- object@properties %>% add(x)
  return(object)
})

#' @rdname add
setMethod("add", signature = c("compartments", "compartments"), definition = function(object, x) {
  return(object %>% appendCompartments(x))
})

#' Append compartments.
#'
#' @param compartments1 base set of compartments
#' @param compartments2 extra set of compartments to be appended
#' @return the resulting set of compartments
#' @keywords internal
#'
appendCompartments <- function(compartments1, compartments2) {
  cmtNames1 <- compartments1@list %>% purrr::map_chr(~ .x %>% to_string())
  cmtNames2 <- compartments2@list %>% purrr::map_chr(~ .x %>% to_string())
  cmtMax <- compartments1 %>% length()

  checkCollisionOnCmts <- cmtNames1 %in% cmtNames2
  if (any(checkCollisionOnCmts)) {
    stop(paste0(
      "Model can't be appended because of duplicate compartment name(s): ",
      paste0(cmtNames1[checkCollisionOnCmts], collapse = ", ")
    ))
  }
  for (compartment in compartments2@list) {
    compartment@index <- compartment@index + cmtMax
    compartments1 <- compartments1 %>% add(compartment)
  }
  for (property in compartments2@properties@list) {
    property@compartment <- property@compartment + cmtMax
    compartments1 <- compartments1 %>% add(property)
  }
  return(compartments1 %>% sort())
}

#_______________________________________________________________________________
#----                               delete                                  ----
#_______________________________________________________________________________

#' @rdname delete
setMethod("delete", signature = c("compartments", "compartment_property"), definition = function(object, x) {
  object@properties <- object@properties %>% delete(x)
  return(object)
})

#_______________________________________________________________________________
#----                                 find                                  ----
#_______________________________________________________________________________

#' @rdname find
setMethod("find", signature = c("compartments", "compartment_property"), definition = function(object, x) {
  return(object@properties %>% find(x))
})

#_______________________________________________________________________________
#----                         get_compartment_index                         ----
#_______________________________________________________________________________

#' @rdname get_compartment_index
setMethod("get_compartment_index", signature = c("compartments", "character"), definition = function(object, name) {
  cmtNames <- object@list %>% purrr::map_chr(~ .x %>% to_string())
  position <- match(name, cmtNames)
  # Backwards compatibility: legacy compartments (e.g. 'A_CENTRAL') may be referred to without prefix
  if (is.na(position)) {
    position <- match(paste0("A_", name), cmtNames)
  }
  if (is.na(position)) {
    stop(paste0("Compartment ", name, " not found."))
  }
  return(object@list[[position]]@index)
})

#_______________________________________________________________________________
#----                               replace                                 ----
#_______________________________________________________________________________

#' @rdname replace
setMethod("replace", signature = c("compartments", "compartment_property"), definition = function(object, x) {
  object@properties <- object@properties %>% replace(x)
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature = c("compartments"), definition = function(object) {
  cat("Compartments:\n")
  for (element in object@list) {
    show(element)
    cat("\n")
  }
})

#_______________________________________________________________________________
#----                                  sort                                 ----
#_______________________________________________________________________________

#' @rdname sort
setMethod("sort", signature = c("compartments"), definition = function(x, decreasing = FALSE, ...) {
  # Sort compartment properties
  x@properties <- x@properties %>% sort()
  return(x)
})
