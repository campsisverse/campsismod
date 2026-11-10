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

#' Rename variables in expressions.
#'
#' Whole-word replacement: a variable is only replaced when it is not
#' preceded or followed by a letter, a digit or an underscore.
#'
#' @param x character vector (expressions)
#' @param from variable names to replace
#' @param to replacement names, same length as \code{from}
#' @return the updated character vector
#' @keywords internal
rename_variables <- function(x, from, to) {
  if (length(x) == 0 || all(is.na(x))) {
    return(x)
  }
  for (i in seq_along(from)) {
    x <- gsub(
      pattern = sprintf("(?<![A-Za-z0-9_])%s(?![A-Za-z0-9_])", from[i]),
      replacement = to[i],
      x = x,
      perl = TRUE
    )
  }
  return(x)
}

#' Rename variables in a model statement (left-hand side, right-hand side,
#' conditions). Comments are left untouched. Slots are modified directly,
#' validity is not checked.
#'
#' @param statement model statement
#' @param from variable names to replace
#' @param to replacement names
#' @return the updated model statement
#' @keywords internal
rename_statement_variables <- function(statement, from, to) {
  if (length(from) == 0) {
    return(statement)
  }
  if (is(statement, "endpoint")) {
    statement@variable <- rename_variables(statement@variable, from, to)
    statement@lhs <- rename_variables(statement@lhs, from, to)
    statement@rhs <- endpoint_rhs(
      variable = statement@variable,
      error_model = statement@error_model,
      theta_add = statement@theta_add,
      theta_prop = statement@theta_prop,
      sigma = statement@sigma
    )
  } else if (is(statement, "equation")) {
    statement@lhs <- rename_variables(statement@lhs, from, to)
    statement@rhs <- rename_variables(statement@rhs, from, to)
  } else if (is(statement, "if_statement")) {
    statement@condition <- rename_variables(statement@condition, from, to)
    statement@equation <- rename_statement_variables(statement@equation, from, to)
  } else if (is(statement, "unknown_statement")) {
    statement@line <- rename_variables(statement@line, from, to)
  }
  return(statement)
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
    return(Ode(paste0("A_", node$compartment), node$rate, comment = comment))
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

  # Compartment names (in order of appearance in the ODE block)
  cmtNames <- json$ode %>%
    purrr::keep(~ !is.null(.x$compartment)) %>%
    purrr::map_chr(~ .x$compartment) %>%
    unique()
  legacyNames <- cmtNames[startsWith(cmtNames, "A_")]
  if (length(legacyNames) > 0) {
    stop(sprintf(
      "Compartment names must not use the legacy 'A_' prefix in version 2.0: %s",
      paste0(legacyNames, collapse = ", ")
    ))
  }
  prefixedNames <- paste0("A_", cmtNames)
  toModel <- function(statement) {
    rename_statement_variables(statement, from = cmtNames, to = prefixedNames)
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
  records@list <- records@list %>%
    purrr::map(function(record) {
      record@statements@list <- record@statements@list %>% purrr::map(toModel)
      return(record)
    })
  model@model <- records

  # Compartments and their properties
  compartments <- records %>% get_compartments()
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
        rhs = rename_variables(rhs, from = cmtNames, to = prefixedNames),
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
#' Compartment names are expected to be already stripped of their prefix.
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

get_statement_expressions <- function(statement) {
  if (is(statement, "endpoint")) {
    return(c(statement@variable, statement@lhs, statement@rhs))
  } else if (is(statement, "equation")) {
    return(c(statement@lhs, statement@rhs))
  } else if (is(statement, "if_statement")) {
    return(c(statement@condition, get_statement_expressions(statement@equation)))
  } else if (is(statement, "unknown_statement")) {
    return(statement@line)
  }
  return(character(0))
}

#' Get all variable-like tokens used in the model code and in the
#' compartment properties.
#'
#' @param model Campsis model
#' @return character vector of unique tokens
#' @keywords internal
get_model_tokens <- function(model) {
  expressions <- model@model@list %>%
    purrr::map(~ .x@statements@list %>% purrr::map(get_statement_expressions)) %>%
    unlist()
  expressions <- c(expressions, model@compartments@properties@list %>% purrr::map_chr(~ .x@rhs))
  tokens <- regmatches(expressions, gregexpr("[A-Za-z_][A-Za-z0-9_]*", expressions)) %>%
    unlist()
  return(unique(tokens))
}

#' Export a Campsis model to the structured JSON format (version 2.0).
#'
#' @param object Campsis model
#' @return a JSON element
#' @keywords internal
campsis_model_to_json_v2 <- function(object) {
  # Compartment names without prefix
  compartments <- object@compartments@list
  cmtNames <- compartments %>% purrr::map_chr(~ .x@name)
  if (any(is.na(cmtNames))) {
    unnamed <- compartments[is.na(cmtNames)] %>% purrr::map_chr(~ .x %>% to_string())
    stop(sprintf(
      "Compartment(s) %s have no name. JSON version 2.0 requires named compartments.",
      paste0(unnamed, collapse = ", ")
    ))
  }
  isIdentifier <- grepl(pattern = paste0("^", variable_pattern_str(), "$"), x = cmtNames)
  invalid <- cmtNames[startsWith(cmtNames, "A_") | !isIdentifier]
  if (length(invalid) > 0) {
    stop(sprintf(
      "Compartment name(s) %s can't be exported to JSON version 2.0",
      paste0(invalid, collapse = ", ")
    ))
  }
  collisions <- cmtNames[cmtNames %in% get_model_tokens(object)]
  if (length(collisions) > 0) {
    stop(sprintf(
      "Variable(s) %s collide with compartment name(s) once the 'A_' prefix is dropped",
      paste0(collisions, collapse = ", ")
    ))
  }
  prefixedNames <- paste0("A_", cmtNames)
  toJson <- function(statement) {
    rename_statement_variables(statement, from = prefixedNames, to = cmtNames)
  }

  json <- list(version = "2.0")

  # Main and ODE blocks
  for (recordName in c("MAIN", "ODE")) {
    record <- object@model %>% get_by_name(recordName)
    if (!isS4(record)) {
      next
    }
    json[[tolower(recordName)]] <- record@statements@list %>%
      purrr::map(~ statement_to_json_v2_node(toJson(.x)))
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
        rhs <- rename_variables(property@rhs, from = prefixedNames, to = cmtNames)
        if (is.na(property@comment)) {
          return(rhs)
        }
        return(list(rhs = rhs, comment = property@comment))
      })
    names(values) <- properties %>% purrr::map_chr(~ cmtNames[.x@compartment])
    json[[key]] <- values
  }

  # Endpoints or raw error block
  errorRecord <- object@model %>% get_by_name("ERROR")
  if (length(errorRecord) > 0) {
    statements <- errorRecord@statements@list %>% purrr::map(toJson)
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
