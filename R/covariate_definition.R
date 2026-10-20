#_______________________________________________________________________________
#----                  covariate_definition class                           ----
#_______________________________________________________________________________

#'
#' Covariate definition class.
#'
#' @slot name covariate name
#' @slot default_value default value
#' @slot label covariate label, any string
#' @slot description detailed description
#' @export
setClass(
  "covariate_definition",
  representation(
    name = "character",
    default_value = "numeric",
    label = "character",
    description = "character"
  ),
  contains = "pmx_element",
  prototype = prototype(
    name = as.character(NA),
    default_value = as.numeric(NA),
    label = as.character(NA),
    description = as.character(NA)
  ),
  validity = function(object) {
    return(expect_one_for_all(object, c("name", "default_value", "label", "description")))
  }
)

#_______________________________________________________________________________
#----               numeric_covariate_definition class                      ----
#_______________________________________________________________________________

#'
#' Numeric covariate definition class.
#'
#' @slot unit covariate unit
#' @export
setClass(
  "numeric_covariate_definition",
  representation(
    unit = "character"
  ),
  contains = "covariate_definition",
  prototype = prototype(
    unit = as.character(NA)
  ),
  validity = function(object) {
    return(expect_one(object, "unit"))
  }
)

#'
#' Create a numeric covariate definition.
#'
#' @param name covariate name
#' @param default_value default value
#' @param label covariate label, optional
#' @param unit covariate unit, optional
#' @param description detailed description, optional
#' @return a numeric covariate definition
#' @export
NumericCovariateDefinition <- function(
  name = NA,
  default_value = NA,
  label = NA,
  unit = NA,
  description = NA
) {
  return(new(
    "numeric_covariate_definition",
    name = as.character(name),
    default_value = as.numeric(default_value),
    label = as.character(label),
    unit = as.character(unit),
    description = as.character(description)
  ))
}

#_______________________________________________________________________________
#----             categorical_covariate_definition class                    ----
#_______________________________________________________________________________

#'
#' Categorical covariate definition class.
#'
#' @slot categories mapping of category labels to numeric values
#' @export
setClass(
  "categorical_covariate_definition",
  representation(
    categories = "numeric"
  ),
  contains = "covariate_definition",
  prototype = prototype(
    categories = numeric(0)
  ),
  validity = function(object) {
    errors <- character()
    if (length(object@categories) > 0 && (is.null(names(object@categories)) || any(names(object@categories) == ""))) {
      errors <- c(errors, "All categories must be named")
    }
    return(errors)
  }
)

#'
#' Create a categorical covariate definition.
#'
#' @param name covariate name
#' @param default_value default value
#' @param label covariate label, optional
#' @param categories mapping of category labels to numeric values, optional
#' @param description detailed description, optional
#' @return a categorical covariate definition
#' @export
CategoricalCovariateDefinition <- function(
  name = NA,
  default_value = NA,
  label = NA,
  categories = numeric(0),
  description = NA
) {
  return(new(
    "categorical_covariate_definition",
    name = as.character(name),
    default_value = as.numeric(default_value),
    label = as.character(label),
    categories = categories,
    description = as.character(description)
  ))
}

#'
#' Create a discrete covariate definition.
#' Alias for CategoricalCovariateDefinition.
#'
#' @param name covariate name
#' @param default_value default value
#' @param label covariate label, optional
#' @param categories mapping of category labels to numeric values, optional
#' @param description detailed description, optional
#' @return a categorical covariate definition
#' @export
DiscreteCovariateDefinition <- CategoricalCovariateDefinition

#_______________________________________________________________________________
#----                              get_name                                  ----
#_______________________________________________________________________________

#' @rdname get_name
setMethod("get_name", signature = c("covariate_definition"), definition = function(x) {
  return(x@name)
})

#_______________________________________________________________________________
#----                          export_to_json                               ----
#_______________________________________________________________________________

#' @rdname export_to_json
setMethod("export_to_json", signature = c("numeric_covariate_definition"), definition = function(object, ...) {
  json <- map_s4_slots_to_json_properties(object, optional = c("label", "description", "unit"))
  json$type <- "numeric_covariate_definition"
  return(JSONElement(json))
})

#' @rdname export_to_json
setMethod("export_to_json", signature = c("categorical_covariate_definition"), definition = function(object, ...) {
  json <- map_s4_slots_to_json_properties(object, optional = c("label", "description"))
  json$type <- "categorical_covariate_definition"
  # Convert categories vector to named list for JSON
  categories <- as.list(object@categories)
  json$categories <- categories
  return(JSONElement(json))
})

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

#' @rdname load_from_json
setMethod("load_from_json", signature = c("numeric_covariate_definition", "json_element"), definition = function(object, json) {
  object <- map_json_properties_to_s4_slots(object, json)
  return(object)
})

#' @rdname load_from_json
setMethod("load_from_json", signature = c("categorical_covariate_definition", "json_element"), definition = function(object, json) {
  # Manually handle categories as map_json_properties_to_s4_slots unlists everything
  data <- json@data
  categories <- unlist(data$categories)
  data$categories <- NULL
  object <- map_json_properties_to_s4_slots(object, JSONElement(data))
  object@categories <- categories
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature = c("covariate_definition"), definition = function(object) {
  type <- ifelse(is(object, "numeric_covariate_definition"), "numeric_covariate_definition", "categorical_covariate_definition")
  label <- if (!is.na(object@label)) paste0(" [", object@label, "]") else ""
  cat(object@name, label, " (", type, "), default=", object@default_value, sep = "")
  if (is(object, "numeric_covariate_definition") && !is.na(object@unit)) {
    cat(", unit=", object@unit, sep = "")
  }
  if (is(object, "categorical_covariate_definition") && length(object@categories) > 0) {
    cat(", categories: ", paste0(names(object@categories), "=", object@categories, collapse = ", "), sep = "")
  }
  cat("\n")
})
