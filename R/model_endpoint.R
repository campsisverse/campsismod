#_______________________________________________________________________________
#----                           endpoint class                              ----
#_______________________________________________________________________________

get_endpoint_error_models <- function() {
  return(c("additive", "proportional", "combined1", "combined2"))
}

#' Generate the right-hand side of an endpoint equation.
#'
#' Theta parameters hold standard deviations and the sigma parameter is
#' expected to have unit variance (nlmixr2-style parametrisation).
#'
#' @param variable model prediction
#' @param error_model one of 'additive', 'proportional', 'combined1' or
#'   'combined2'
#' @param theta_add name of the additive theta (without prefix)
#' @param theta_prop name of the proportional theta (without prefix)
#' @param sigma name of the sigma (without prefix)
#' @return the right-hand side, single character string
#' @keywords internal
endpoint_rhs <- function(variable, error_model, theta_add, theta_prop, sigma) {
  add <- paste0("THETA_", theta_add)
  prop <- paste0("THETA_", theta_prop)
  eps <- paste0("EPS_", sigma)
  # Products instead of '^' so that the code is also valid C++ (mrgsolve)
  switch(
    error_model,
    additive = sprintf("%s + %s*%s", variable, add, eps),
    proportional = sprintf("%s*(1 + %s*%s)", variable, prop, eps),
    combined1 = sprintf("%s + (%s + %s*%s)*%s", variable, add, prop, variable, eps),
    combined2 = sprintf(
      "%s + sqrt(%s*%s + (%s*%s)*(%s*%s))*%s",
      variable,
      add,
      add,
      prop,
      variable,
      prop,
      variable,
      eps
    ),
    stop(sprintf("Unknown error model '%s'", error_model))
  )
}

#'
#' Endpoint class.
#'
#' A structured residual error model applied to a model variable. An endpoint
#' is an equation whose left-hand side is the output variable and whose
#' right-hand side is generated from the error model.
#'
#' @slot variable model prediction the error is applied to
#' @slot error_model 'additive', 'proportional', 'combined1' or 'combined2'
#' @slot theta_add name of the THETA holding the additive standard deviation
#' @slot theta_prop name of the THETA holding the proportional standard
#'   deviation
#' @slot sigma name of the unit-variance SIGMA
#' @export
setClass(
  "endpoint",
  representation(
    variable = "character",
    error_model = "character",
    theta_add = "character",
    theta_prop = "character",
    sigma = "character"
  ),
  contains = "equation",
  prototype = prototype(theta_add = as.character(NA), theta_prop = as.character(NA)),
  validity = function(object) {
    errors <- expect_one_for_all(object, c("variable", "error_model", "theta_add", "theta_prop", "sigma"))
    if (length(errors) > 0) {
      return(errors)
    }
    errorModel <- object@error_model
    if (!errorModel %in% get_endpoint_error_models()) {
      return(sprintf(
        "error_model must be one of: %s",
        paste0(get_endpoint_error_models(), collapse = ", ")
      ))
    }
    needsAdd <- errorModel %in% c("additive", "combined1", "combined2")
    needsProp <- errorModel %in% c("proportional", "combined1", "combined2")
    if (needsAdd == is.na(object@theta_add)) {
      errors <- c(
        errors,
        sprintf("theta_add must be %s for error model '%s'", ifelse(needsAdd, "provided", "NA"), errorModel)
      )
    }
    if (needsProp == is.na(object@theta_prop)) {
      errors <- c(
        errors,
        sprintf("theta_prop must be %s for error model '%s'", ifelse(needsProp, "provided", "NA"), errorModel)
      )
    }
    if (is.na(object@sigma)) {
      errors <- c(errors, "sigma must be provided")
    }
    if (length(errors) == 0) {
      expected <- endpoint_rhs(
        variable = object@variable,
        error_model = errorModel,
        theta_add = object@theta_add,
        theta_prop = object@theta_prop,
        sigma = object@sigma
      )
      if (!identical(object@rhs, expected)) {
        errors <- c(errors, "Endpoint right-hand side is not consistent with its error model")
      }
    }
    if (length(errors) == 0) {
      return(TRUE)
    }
    return(errors)
  }
)

#'
#' Create an endpoint (structured residual error model).
#'
#' The parametrisation follows nlmixr2: \code{theta_add} and \code{theta_prop}
#' are THETA parameters holding standard deviations, and \code{sigma} is a
#' SIGMA parameter with unit variance. With \eqn{f} the model prediction and
#' \eqn{\epsilon \sim N(0, 1)}:
#' \itemize{
#'   \item additive: \eqn{y = f + a \epsilon}
#'   \item proportional: \eqn{y = f (1 + b \epsilon)}
#'   \item combined1: \eqn{y = f + (a + b f) \epsilon}
#'   \item combined2: \eqn{y = f + \sqrt{a^2 + b^2 f^2} \epsilon}
#' }
#'
#' @param variable model prediction the error is applied to, e.g. 'CONC'
#' @param error_model 'additive', 'proportional', 'combined1' or 'combined2'
#' @param sigma name of the unit-variance SIGMA (prefix EPS is added in code)
#' @param theta_add name of the THETA holding the additive standard deviation
#'   (prefix THETA is added in code)
#' @param theta_prop name of the THETA holding the proportional standard
#'   deviation (prefix THETA is added in code)
#' @param output output variable, default is \code{<variable>_ERR}
#' @param comment any comment, NA by default
#' @return an endpoint
#' @export
Endpoint <- function(
  variable,
  error_model,
  sigma,
  theta_add = NA,
  theta_prop = NA,
  output = NULL,
  comment = as.character(NA)
) {
  if (is.null(output)) {
    output <- paste0(variable, "_ERR")
  }
  if (!is.character(error_model) || length(error_model) != 1 || !error_model %in% get_endpoint_error_models()) {
    stop(sprintf("error_model must be one of: %s", paste0(get_endpoint_error_models(), collapse = ", ")))
  }
  return(new(
    "endpoint",
    lhs = as.character(output),
    rhs = endpoint_rhs(
      variable = variable,
      error_model = error_model,
      theta_add = theta_add,
      theta_prop = theta_prop,
      sigma = sigma
    ),
    variable = as.character(variable),
    error_model = error_model,
    theta_add = as.character(theta_add),
    theta_prop = as.character(theta_prop),
    sigma = as.character(sigma),
    comment = as.character(comment)
  ))
}

update_endpoint_rhs <- function(object) {
  object@rhs <- endpoint_rhs(
    variable = object@variable,
    error_model = object@error_model,
    theta_add = object@theta_add,
    theta_prop = object@theta_prop,
    sigma = object@sigma
  )
  methods::validObject(object)
  return(object)
}

#_______________________________________________________________________________
#----                             replace_all                                ----
#_______________________________________________________________________________

replace_prefixed_name <- function(name, prefix, pattern, replacement, ...) {
  if (is.na(name)) {
    return(name)
  }
  newName <- paste0(prefix, name) %>% replace_all(pattern = pattern, replacement = replacement, ...)
  if (!startsWith(newName, prefix)) {
    stop(sprintf("Replacement in endpoint would remove prefix '%s' from '%s%s'", prefix, prefix, name))
  }
  return(substring(newName, nchar(prefix) + 1))
}

#' @rdname replace_all
setMethod(
  "replace_all",
  signature = c("endpoint", "pattern", "character"),
  definition = function(object, pattern, replacement, ...) {
    object@variable <- object@variable %>% replace_all(pattern = pattern, replacement = replacement, ...)
    object@lhs <- object@lhs %>% replace_all(pattern = pattern, replacement = replacement, ...)
    object@theta_add <- replace_prefixed_name(object@theta_add, "THETA_", pattern, replacement, ...)
    object@theta_prop <- replace_prefixed_name(object@theta_prop, "THETA_", pattern, replacement, ...)
    object@sigma <- replace_prefixed_name(object@sigma, "EPS_", pattern, replacement, ...)
    return(update_endpoint_rhs(object))
  }
)

#_______________________________________________________________________________
#----                          compile_endpoints                            ----
#_______________________________________________________________________________

#' Compile endpoints into plain equations.
#'
#' Structured endpoints are translated into their equivalent \code{[ERROR]}
#' code (plain equations) before the code is emitted for a simulation engine.
#'
#' @param model Campsis model
#' @return the Campsis model where all endpoints are replaced by equations
#' @keywords internal
compile_endpoints <- function(model) {
  model@model@list <- model@model@list %>%
    purrr::map(function(record) {
      record@statements@list <- record@statements@list %>%
        purrr::map(function(statement) {
          if (is(statement, "endpoint")) {
            return(Equation(statement@lhs, statement@rhs, comment = statement@comment))
          }
          return(statement)
        })
      return(record)
    })
  return(model)
}
