#' Has exact method allows to check the existence of a S4 method in Campsis
#' based on its signature.
#'
#' @param generic generic function name
#' @param signature function signature
#' @param where where to search functions
#' @return logical value
#' @importFrom methods getGeneric findMethods isGeneric
#' @export
#'
has_exact_method <- function(generic, signature, where = topenv(parent.frame())) {
  if (!methods::isGeneric(generic, where = where)) {
    return(FALSE)
  }
  gen <- methods::getGeneric(generic, where = where)
  ml <- methods::findMethods(gen)
  target <- paste(signature, collapse = "#")
  return(target %in% names(ml))
}

#' Map JSON properties to S4 slots.
#'
#' @param object S4 object
#' @param json JSON element, json_element class
#' @param discard_type discard JSON property 'type'
#' @return a S4 object
#' @export
#'
map_json_properties_to_s4_slots <- function(object, json, discard_type = TRUE) {
  json <- json@data
  properties <- names(json)
  if (discard_type) {
    properties <- properties[properties != "type"]
  }

  for (property in properties) {
    value <- json[[property]]
    isList <- is.list(value)

    if (isList && !is.null(value$type)) {
      # Recursion
      if (has_exact_method(generic = "load_from_json", signature = c(value$type, "json_element"))) {
        value <- load_from_json(object = new(value$type), json = JSONElement(value))
      } else {
        value <- map_json_properties_to_s4_slots(
          object = new(value$type),
          json = JSONElement(value),
          discard_type = TRUE
        )
      }
    } else {
      if (isList) {
        value <- unlist(value)
      }
      if (is.null(value)) {
        value <- character(0)
      }
    }
    slot(object, property) <- value
  }
  return(object)
}

#' Map S4 slots to JSON properties.
#'
#' @param object S4 object
#' @param add_type add type as a property, TRUE by default
#' @param optional properties that are optional in JSON, character vector
#' @param ignore slots to be ignored
#' @return a JSON object ready to be serialised
#' @export
#'
map_s4_slots_to_json_properties <- function(object, add_type = TRUE, optional = NULL, ignore = NULL) {
  if (!isS4(object)) {
    stop("Input must be an S4 object.")
  }

  # Initialize list for JSON properties
  json <- list()

  # Optionally add the type field
  if (add_type) {
    json$type <- class(object)[[1]]
  }

  # Iterate over slots
  slotNames <- slotNames(object)
  for (property in slotNames[!slotNames %in% ignore]) {
    value <- slot(object, property)

    if (isS4(value)) {
      # Recursive call for nested S4
      json[[property]] <- map_s4_slots_to_json_properties(value, add_type = TRUE)
    } else if (is.list(value)) {
      # Handle lists: check if elements are S4
      json[[property]] <- lapply(value, function(v) {
        if (isS4(v)) {
          map_s4_slots_to_json_properties(v, add_type = TRUE)
        } else {
          v
        }
      })
    } else if (length(value) == 0) {
      # Map empty slot to NULL
      if (!property %in% optional) {
        json[[property]] <- NULL
      }
    } else {
      # Atomic vectors, scalars, etc.
      if (!(property %in% optional && is.na(value))) {
        json[[property]] <- value
      }
    }
  }

  return(json)
}

#' JSON to Campsis dataset.
#'
#' @param object empty dataset
#' @param json json element
#' @return Campsis dataset
#' @importFrom jsonlite parse_json
#' @importFrom purrr keep imap map flatten_chr
#' @keywords internal
#'
json_to_campsis_model <- function(object, json) {
  json <- json@data
  model <- object

  # Parse model code
  text <- unlist(json$code)
  if (!is.null(text)) {
    model@model <- read.model(text = text)
  }

  # Parse parameters
  jsonParameters <- json$parameters
  jsonThetas <- jsonParameters %>%
    purrr::keep(~ .x$type == "theta")
  jsonOmegasOnDiag <- jsonParameters %>%
    purrr::keep(~ .x$type == "omega" && is.null(.x$name2))
  jsonOmegasOffDiag <- jsonParameters %>%
    purrr::keep(~ .x$type == "omega" && !is.null(.x$name2))
  jsonSigmasOnDiag <- jsonParameters %>%
    purrr::keep(~ .x$type == "sigma" && is.null(.x$name2))
  jsonSigmasOffDiag <- jsonParameters %>%
    purrr::keep(~ .x$type == "sigma" && !is.null(.x$name2))

  thetas <- jsonThetas %>%
    purrr::imap(~ json_to_parameter(x = .x, index = .y, index2 = .y))
  omegas <- jsonOmegasOnDiag %>%
    purrr::imap(~ json_to_parameter(x = .x, index = .y, index2 = .y))
  sigmas <- jsonSigmasOnDiag %>%
    purrr::imap(~ json_to_parameter(x = .x, index = .y, index2 = .y))

  omegaNames <- omegas %>%
    purrr::map(~ .x@name)
  sigmaNames <- sigmas %>%
    purrr::map(~ .x@name)

  omegasOffDiag <- jsonOmegasOffDiag %>%
    purrr::map(~ json_to_off_diag_parameter(json = .x, diag_names = omegaNames))
  sigmasOffDiag <- jsonSigmasOffDiag %>%
    purrr::map(~ json_to_off_diag_parameter(json = .x, diag_names = sigmaNames))

  model@parameters@list <- c(thetas, omegas, omegasOffDiag, sigmas, sigmasOffDiag)

  # Parse covariate definitions
  jsonCovariates <- json$covariates
  if (!is.null(jsonCovariates)) {
    model@covariates@list <- jsonCovariates %>%
      purrr::map(~ load_from_json(new(.x$type), JSONElement(.x)))
  }

  # Update compartments
  model <- model %>%
    update_compartments()

  # Sort model parameters
  model <- model %>%
    campsismod::sort()

  # Parse variance-covariance matrix
  varcov <- json$varcov
  if (length(varcov) > 0) {
    # Find all possible parameter names and initialize the matrix
    rowNames <- varcov %>%
      purrr::map(
        ~ c(
          find_varcov_parameter(ref = .x$ref1, model = model) %>% get_name(),
          find_varcov_parameter(ref = .x$ref2, model = model) %>% get_name()
        )
      ) %>%
      purrr::flatten_chr() %>%
      unique()

    matrix <- matrix(0L, nrow = length(rowNames), ncol = length(rowNames))
    dimnames(matrix) <- list(rowNames, rowNames)

    # Fill in with values
    for (entry in varcov) {
      ref1Name <- find_varcov_parameter(ref = entry$ref1, model = model) %>% get_name()
      ref2Name <- find_varcov_parameter(ref = entry$ref2, model = model) %>% get_name()
      matrix[ref1Name, ref2Name] <- entry$cov
      matrix[ref2Name, ref1Name] <- entry$cov
    }

    model@parameters@varcov <- matrix
  }
  return(model)
}

find_varcov_double_array_parameter <- function(ref, model, type) {
  if (type == "omega") {
    paramRef <- Omega()
  } else if (type == "sigma") {
    paramRef <- Sigma()
  } else {
    stop("type must be 'omega' or 'sigma'")
  }
  if (is.null(ref$name2)) {
    paramRef@name <- ref$name
    retValue <- model %>% find(paramRef)
  } else {
    # First attempt
    paramRef@name <- paste0(ref$name, "_", ref$name2)
    retValue <- model %>% find(paramRef)
    # Second attempt
    if (is.null(retValue)) {
      paramRef@name <- paste0(ref$name2, "_", ref$name)
      retValue <- model %>% find(paramRef)
    }
  }
  return(retValue)
}

find_varcov_parameter <- function(ref, model) {
  if (ref$type == "theta_ref") {
    retValue <- model %>% find(Theta(name = ref$name))
  } else if (ref$type == "omega_ref") {
    retValue <- find_varcov_double_array_parameter(ref = ref, model = model, type = "omega")
  } else if (ref$type == "sigma_ref") {
    retValue <- find_varcov_double_array_parameter(ref = ref, model = model, type = "sigma")
  }
  if (is.null(retValue)) {
    if (is.null(ref$name2)) {
      stop(sprintf("Parameter reference not found (type: %s, name: %s)", ref$type, ref$name))
    } else {
      stop(sprintf("Parameter reference not found (type: %s, name: %s, name2: %s)", ref$type, ref$name, ref$name2))
    }
  }
  return(retValue)
}

#' Convert JSON correlation parameter (OMEGA or SIGMA) into a Campsis parameter.
#'
#' @param json JSON data
#' @param diag_names parameter names on the diagonal, character vector
#' @return the corresponding Campsis parameter
#' @keywords internal
json_to_off_diag_parameter <- function(json, diag_names) {
  name <- json$name
  name2 <- json$name2
  index <- which(diag_names == name)
  index2 <- which(diag_names == name2)
  return(json_to_parameter(x = json, index = index, index2 = index2))
}

#' Process JSON double array parameter.
#'
#' @param x JSON data, OMEGA or SIGMA parameter
#' @return updated JSON data with updated 'name' field and removed 'name2' field
#' @keywords internal
process_json_double_array_parameter <- function(x) {
  if (!is.null(x$name2)) {
    x$name <- paste0(x$name, "_", x$name2)
    x$name2 <- NULL
  }
  return(x)
}

#' JSON to Campsis parameter.
#'
#' @param x JSON data
#' @param index parameter index to add
#' @param index2 second parameter index to add for OMEGAs and SIGMAs
#' @return Campsis parameter
#' @export
#'
json_to_parameter <- function(x, index = NULL, index2 = NULL) {
  if (x$type == "theta") {
    if (is.null(index)) {
      theta <- Theta()
    } else {
      theta <- Theta(index = index)
    }
    x$type <- NULL
    return(load_from_json(object = theta, JSONElement(x)))
  } else if (x$type == "omega") {
    x <- process_json_double_array_parameter(x)
    if (is.null(index)) {
      omega <- Omega()
    } else {
      omega <- Omega(index = index, index2 = index2)
    }
    x$type <- x$var_type
    x$var_type <- NULL
    return(load_from_json(object = omega, JSONElement(x)))
  } else if (x$type == "sigma") {
    x <- process_json_double_array_parameter(x)
    if (is.null(index)) {
      sigma <- Sigma()
    } else {
      sigma <- Sigma(index = index, index2 = index2)
    }
    x$type <- x$var_type
    x$var_type <- NULL
    return(load_from_json(object = sigma, JSONElement(x)))
  } else {
    stop("Unknown parameter type")
  }
}

#' Open JSON file.
#'
#' @param json JSON in its string form or path to JSON file
#' @param schema JSON schema
#'
#' @return parsed JSON object
#' @importFrom jsonlite parse_json
#' @importFrom jsonvalidate json_schema
#' @keywords internal
#'
open_json <- function(json, schema = NULL) {
  if (is.list(json)) {
    return(JSONElement(json)) # Don't go further if data is already parsed
  }
  assertthat::assert_that(length(json) == 1, msg = "Argument json must be a path or the JSON string")

  if (grepl(pattern = "\\s*[\\[\\{]", x = json)) {
    rawJson <- json
  } else {
    rawJson <- suppressWarnings(paste0(readLines(json), collapse = "\n"))
  }

  # Validate content against schema
  if (get_campsismod_option(name = "VALIDATE_JSON", default = TRUE)) {
    obj <- jsonvalidate::json_schema$new(schema)
    obj$validate(rawJson, error = TRUE)
  }

  json_ <- jsonlite::parse_json(rawJson, simplifyVector = FALSE)

  return(JSONElement(json_))
}
