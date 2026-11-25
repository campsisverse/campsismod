#_______________________________________________________________________________
#----                 JSON interface, version 2.0 (structured)              ----
#_______________________________________________________________________________

# Compartment property keys in the JSON (v2.0) and their S4 class
get_json_v2_property_classes <- function() {
  return(c(
    f = "compartment_bioavailability",
    lag = "compartment_lag_time",
    duration = "compartment_infusion_duration",
    rate = "compartment_infusion_rate",
    init = "compartment_initial_condition"
  ))
}

#_______________________________________________________________________________
#----                          JSON -> Campsis model                        ----
#_______________________________________________________________________________

json_v2_optional_comment <- function(node) {
  if (is.null(node$comment)) {
    return(as.character(NA))
  }
  return(node$comment)
}

#' Convert a JSON (v2.0) AST node into a model statement.
#'
#' @param node JSON node (list)
#' @return a model statement
#' @keywords internal
json_v2_node_to_statement <- function(node) {
  comment <- json_v2_optional_comment(node)
  type <- node$type
  if (!is.null(type)) {
    return(switch(
      type,
      comment = Comment(node$text),
      blank = LineBreak(),
      "if" = IfStatement(
        condition = node$condition,
        equation = Equation(node$then$lhs, node$then$rhs),
        comment = comment
      ),
      stop(sprintf("Unknown statement type '%s'", type))
    ))
  }
  if (!is.null(node$compartment)) {
    return(Ode(node$compartment, node$rate, comment = comment))
  }
  if (!is.null(node$lhs)) {
    return(Equation(node$lhs, node$rhs, comment = comment))
  }
  stop("Unable to interpret JSON statement: ", jsonlite::toJSON(node, auto_unbox = TRUE))
}

json_v2_nodes_to_statements <- function(nodes) {
  statements <- ModelStatements()
  statements@list <- nodes %>%
    purrr::map(json_v2_node_to_statement)
  return(statements)
}

json_v2_endpoint_to_statement <- function(node) {
  return(Endpoint(
    variable = node$variable,
    error_model = node$error_model,
    sigma = node$sigma,
    theta_add = if (is.null(node$theta_add)) NA else node$theta_add,
    theta_prop = if (is.null(node$theta_prop)) NA else node$theta_prop,
    output = node$output,
    comment = json_v2_optional_comment(node)
  ))
}

#' Check that the parameters referenced by the endpoints exist in the model.
#' A warning is raised when an endpoint sigma does not have unit variance.
#'
#' @param model Campsis model
#' @return nothing
#' @keywords internal
check_endpoint_references <- function(model) {
  errorRecord <- model@model %>% get_by_name("ERROR")
  if (length(errorRecord) == 0) {
    return(invisible(NULL))
  }
  endpoints <- errorRecord@statements@list %>% purrr::keep(~ is(.x, "endpoint"))
  for (endpoint in endpoints) {
    thetas <- c(endpoint@theta_add, endpoint@theta_prop) %>% purrr::discard(is.na)
    for (theta in thetas) {
      if (is.null(model %>% find(Theta(name = theta)))) {
        stop(sprintf("Endpoint '%s': THETA '%s' not found in model parameters", endpoint@lhs, theta))
      }
    }
    sigma <- model %>% find(Sigma(name = endpoint@sigma))
    if (is.null(sigma)) {
      stop(sprintf("Endpoint '%s': SIGMA '%s' not found in model parameters", endpoint@lhs, endpoint@sigma))
    }
    variance <- (sigma %>% standardise())@value
    if (is.na(variance) || abs(variance - 1) > sqrt(.Machine$double.eps)) {
      warning(sprintf(
        "Endpoint '%s': SIGMA '%s' should have a unit variance (found %s). Residual error will be scaled accordingly.",
        endpoint@lhs,
        endpoint@sigma,
        format(variance)
      ))
    }
  }
  return(invisible(NULL))
}

#' JSON (version 2.0) to Campsis model.
#'
#' @param object empty Campsis model
#' @param json json element
#' @return Campsis model
#' @keywords internal
json_v2_to_campsis_model <- function(object, json) {
  json <- json@data
  model <- object

  if (!is.null(json$endpoints) && !is.null(json$error)) {
    stop("Fields 'endpoints' and 'error' are mutually exclusive")
  }

  # Code records
  records <- CodeRecords()
  if (!is.null(json$main)) {
    record <- new("main_record", statements = json_v2_nodes_to_statements(json$main))
    records@list <- c(records@list, record)
  }
  if (!is.null(json$ode)) {
    record <- new("ode_record", statements = json_v2_nodes_to_statements(json$ode))
    records@list <- c(records@list, record)
  }
  if (!is.null(json$endpoints)) {
    statements <- ModelStatements()
    statements@list <- json$endpoints %>% purrr::map(json_v2_endpoint_to_statement)
    records@list <- c(records@list, new("error_record", statements = statements))
  } else if (!is.null(json$error)) {
    record <- new("error_record", statements = parse_statements(as.character(unlist(json$error))))
    records@list <- c(records@list, record)
  }
  model@model <- records

  # Compartments and their properties
  compartments <- records %>% get_compartments()
  cmtNames <- compartments@list %>% purrr::map_chr(~ .x %>% to_string())
  propertyClasses <- get_json_v2_property_classes()
  for (key in names(propertyClasses)) {
    for (cmtName in names(json[[key]])) {
      index <- match(cmtName, cmtNames)
      if (is.na(index)) {
        stop(sprintf("Compartment undefined: '%s' in '%s'", cmtName, key))
      }
      value <- json[[key]][[cmtName]]
      if (is.list(value)) {
        rhs <- value$rhs
        comment <- json_v2_optional_comment(value)
      } else {
        rhs <- value
        comment <- as.character(NA)
      }
      property <- new(
        propertyClasses[[key]],
        compartment = as.integer(index),
        rhs = rhs,
        comment = comment
      )
      compartments <- compartments %>% add(property)
    }
  }
  model@compartments <- compartments

  # Parameters, covariates and variance-covariance matrix
  model <- json_to_parameters_and_covariates(model = model, json = json)
  model <- model %>%
    campsismod::sort()
  model <- json_to_varcov(model = model, json = json)

  check_endpoint_references(model)

  return(model)
}

#_______________________________________________________________________________
#----                          Campsis model -> JSON                        ----
#_______________________________________________________________________________

add_json_v2_comment <- function(node, statement) {
  if (!is.na(statement@comment)) {
    node$comment <- statement@comment
  }
  return(node)
}

#' Convert a model statement into a JSON (v2.0) AST node.
#'
#' @param statement model statement
#' @return JSON node (list)
#' @keywords internal
statement_to_json_v2_node <- function(statement) {
  if (is(statement, "ode")) {
    node <- list(compartment = statement@lhs, rate = statement@rhs)
    return(add_json_v2_comment(node, statement))
  } else if (is(statement, "equation")) {
    node <- list(lhs = statement@lhs, rhs = statement@rhs)
    return(add_json_v2_comment(node, statement))
  } else if (is(statement, "if_statement")) {
    node <- list(
      type = "if",
      condition = statement@condition,
      then = list(lhs = statement@equation@lhs, rhs = statement@equation@rhs)
    )
    return(add_json_v2_comment(node, statement))
  } else if (is(statement, "comment")) {
    return(list(type = "comment", text = statement@comment))
  } else if (is(statement, "line_break")) {
    return(list(type = "blank"))
  } else if (is(statement, "unknown_statement")) {
    stop(sprintf(
      "Unknown statement '%s' can't be exported to JSON version 2.0. Please use version 1.0.",
      statement@line
    ))
  }
  stop(sprintf("Statement of class '%s' can't be exported to JSON version 2.0", class(statement)[[1]]))
}

endpoint_to_json_v2_node <- function(endpoint) {
  node <- list(variable = endpoint@variable)
  if (endpoint@lhs != paste0(endpoint@variable, "_ERR")) {
    node$output <- endpoint@lhs
  }
  node$error_model <- endpoint@error_model
  if (!is.na(endpoint@theta_add)) {
    node$theta_add <- endpoint@theta_add
  }
  if (!is.na(endpoint@theta_prop)) {
    node$theta_prop <- endpoint@theta_prop
  }
  node$sigma <- endpoint@sigma
  return(add_json_v2_comment(node, endpoint))
}

#' Export a Campsis model to the structured JSON format (version 2.0).
#' Compartment names are exported as is.
#'
#' @param object Campsis model
#' @return a JSON element
#' @keywords internal
campsis_model_to_json_v2 <- function(object) {
  json <- list(version = "2.0")

  # Main and ODE blocks
  for (recordName in c("MAIN", "ODE")) {
    record <- object@model %>% get_by_name(recordName)
    if (!isS4(record)) {
      next
    }
    json[[tolower(recordName)]] <- record@statements@list %>%
      purrr::map(statement_to_json_v2_node)
  }

  # Compartment properties
  propertyClasses <- get_json_v2_property_classes()
  for (key in names(propertyClasses)) {
    properties <- object@compartments@properties@list %>%
      purrr::keep(~ is(.x, propertyClasses[[key]]))
    if (length(properties) == 0) {
      next
    }
    values <- properties %>%
      purrr::map(function(property) {
        if (is.na(property@comment)) {
          return(property@rhs)
        }
        return(list(rhs = property@rhs, comment = property@comment))
      })
    names(values) <- properties %>%
      purrr::map_chr(~ object@compartments %>% find(Compartment(index = .x@compartment)) %>% to_string())
    json[[key]] <- values
  }

  # Endpoints or raw error block
  errorRecord <- object@model %>% get_by_name("ERROR")
  if (length(errorRecord) > 0) {
    statements <- errorRecord@statements@list
    isEndpoint <- statements %>% purrr::map_lgl(~ is(.x, "endpoint"))
    isLineBreak <- statements %>% purrr::map_lgl(~ is(.x, "line_break"))
    if (any(isEndpoint) && all(isEndpoint | isLineBreak)) {
      json$endpoints <- statements[isEndpoint] %>% purrr::map(endpoint_to_json_v2_node)
    } else {
      if (any(isEndpoint)) {
        warning("ERROR record mixes endpoints with other statements: endpoints are exported as raw code.")
      }
      # Kept as a list so that a single line is still serialised as an array
      json$error <- statements %>% purrr::map(~ .x %>% to_string(dest = "campsis"))
    }
  }

  json <- c(json, parameters_and_covariates_to_json(object))
  return(JSONElement(json))
}
