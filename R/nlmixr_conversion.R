#_______________________________________________________________________________
#----                             ini block                                 ----
#_______________________________________________________________________________

format_nlmixr_value <- function(value, fix = FALSE) {
  if (length(value) > 1) {
    value <- paste0("c(", paste0(as.character(value), collapse = ", "), ")")
  } else {
    value <- as.character(value)
  }
  if (fix) {
    value <- paste0("fix(", value, ")")
  }
  return(value)
}

append_nlmixr_label <- function(line, label) {
  if (is.na(label)) {
    return(line)
  }
  return(paste0(line, "; label(", encodeString(label, quote = "\""), ")"))
}

#' Get the THETA lines of the nlmixr2 ini block.
#'
#' @param model Campsis model
#' @param used names of the variables used in the model code
#' @return character vector
#' @keywords internal
nlmixr_ini_theta <- function(model, used) {
  values <- rxode_params(model)
  thetaNames <- names(values)
  missing <- thetaNames[is.na(values)]
  if (length(missing) > 0) {
    stop(sprintf("THETA(s) %s must have a value to be exported to nlmixr2", paste0(missing, collapse = ", ")))
  }
  lines <- character(0)
  for (i in which(thetaNames %in% used)) {
    theta <- model@parameters %>% get_by_index(Theta(index = i))
    line <- paste0(thetaNames[i], " <- ", format_nlmixr_value(values[[i]], fix = theta@fix))
    lines <- c(lines, append_nlmixr_label(line, theta@label))
  }
  return(lines)
}

#' Split a variance-covariance matrix into diagonal blocks.
#'
#' A block spans from its first index up to the last index having a non-zero
#' covariance with any element of the block.
#'
#' @param matrix symmetric matrix
#' @return list of integer vectors (indexes of each block)
#' @keywords internal
get_matrix_blocks <- function(matrix) {
  blocks <- list()
  size <- nrow(matrix)
  start <- 1L
  while (start <= size) {
    end <- start
    i <- start
    while (i <= end) {
      end <- max(end, which(matrix[i, ] != 0))
      i <- i + 1L
    }
    blocks <- c(blocks, list(seq.int(start, end)))
    start <- end + 1L
  }
  return(blocks)
}

#' Get the OMEGA lines of the nlmixr2 ini block.
#'
#' Diagonal OMEGAs give \code{ETA_X ~ var}, OMEGA blocks give
#' \code{ETA_X + ETA_Y ~ c(var_x, covar_xy, var_y)} (lower triangle). A block
#' is fixed when all its diagonal OMEGAs are fixed. Blocks where no ETA is used
#' are dropped.
#'
#' @param model Campsis model
#' @param used names of the variables used in the model code
#' @return character vector
#' @keywords internal
nlmixr_ini_omega <- function(model, used) {
  matrix <- rxode_matrix(model, type = "omega")
  if (nrow(matrix) == 0) {
    return(character(0))
  }
  diagOmegas <- (model@parameters %>% select("omega"))@list %>% purrr::keep(is_diag)
  fixed <- rep(FALSE, nrow(matrix))
  fixed[diagOmegas %>% purrr::map_int(~ .x@index)] <- diagOmegas %>% purrr::map_lgl(~ .x@fix)

  lines <- character(0)
  for (block in get_matrix_blocks(matrix)) {
    subMatrix <- matrix[block, block, drop = FALSE]
    if (!any(rownames(subMatrix) %in% used)) {
      next
    }
    lhs <- paste0(rownames(subMatrix), collapse = " + ")
    values <- subMatrix[upper.tri(subMatrix, diag = TRUE)]
    lines <- c(lines, paste0(lhs, " ~ ", format_nlmixr_value(values, fix = all(fixed[block]))))
  }
  return(lines)
}

#' Get the nlmixr2 ini block.
#'
#' THETAs are exported as fixed effects (\code{THETA_X <- value}) and OMEGAs as
#' between-subject variabilities (\code{ETA_X ~ variance}). SIGMAs are not
#' exported: residual variability is described by the endpoints in the model
#' block, their THETAs holding the standard deviations. Parameters that are not
#' used in the model code are dropped with a warning, since nlmixr2 rejects
#' them.
#'
#' @param model Campsis model
#' @return character vector, one line per parameter
#' @export
nlmixr_ini <- function(model) {
  used <- get_model_tokens(model)
  lines <- c(nlmixr_ini_theta(model, used = used), nlmixr_ini_omega(model, used = used))
  declared <- c(names(rxode_params(model)), rownames(rxode_matrix(model, type = "omega")))
  exported <- unlist(regmatches(lines, gregexpr("[A-Za-z_][A-Za-z0-9_]*", lines)))
  dropped <- setdiff(declared, exported)
  if (length(dropped) > 0) {
    warning(sprintf(
      "Parameter(s) %s are not used in the model code and are not exported to nlmixr2.",
      paste0(dropped, collapse = ", ")
    ))
  }
  return(lines)
}

#_______________________________________________________________________________
#----                            model block                                ----
#_______________________________________________________________________________

#' Get the nlmixr2 residual error line of an endpoint.
#'
#' @param endpoint endpoint
#' @return single character string, e.g. \code{CONC ~ add(THETA_ADD)}
#' @keywords internal
nlmixr_endpoint <- function(endpoint) {
  add <- sprintf("add(THETA_%s)", endpoint@theta_add)
  prop <- sprintf("prop(THETA_%s)", endpoint@theta_prop)
  # combined2() is explicit so that the model does not depend on the addProp control setting
  rhs <- switch(
    endpoint@error_model,
    additive = add,
    proportional = prop,
    combined1 = paste(add, prop, "combined1()", sep = " + "),
    combined2 = paste(add, prop, "combined2()", sep = " + "),
    stop(sprintf("Unknown error model '%s'", endpoint@error_model))
  )
  return(paste0(endpoint@variable, " ~ ", rhs))
}

get_endpoints <- function(model) {
  return(
    model@model@list %>%
      purrr::map(~ .x@statements@list %>% purrr::keep(~ is(.x, "endpoint"))) %>%
      purrr::flatten()
  )
}

#' Check the SIGMAs before export to nlmixr2.
#'
#' @param model Campsis model
#' @return the EPS names that are not attached to any endpoint
#' @keywords internal
check_nlmixr_sigmas <- function(model) {
  sigmas <- (model@parameters %>% select("sigma"))@list %>% purrr::keep(is_diag)
  epsNames <- sigmas %>% purrr::map_chr(~ .x %>% get_name_in_model())
  endpointEps <- get_endpoints(model) %>% purrr::map_chr(~ paste0("EPS_", .x@sigma))

  for (sigma in sigmas[epsNames %in% endpointEps]) {
    variance <- (sigma %>% standardise())@value
    if (is.na(variance) || abs(variance - 1) > sqrt(.Machine$double.eps)) {
      warning(sprintf(
        "SIGMA '%s' has a variance of %s: nlmixr2 assumes a unit variance, this variance is ignored.",
        sigma %>% get_name_in_model(),
        format(variance)
      ))
    }
  }

  unused <- setdiff(epsNames, endpointEps)
  if (length(unused) > 0) {
    warning(sprintf(
      paste0(
        "SIGMA(s) %s are not attached to any endpoint and can't be exported to nlmixr2: ",
        "they are replaced by 0 in the model code. Please use Endpoint() to define residual error models."
      ),
      paste0(unused, collapse = ", ")
    ))
  }
  return(unused)
}

#' Get the nlmixr2 model block.
#'
#' The code is generated with the rxode2 exporter. Compartments are named
#' without the 'A_' prefix and endpoints are written using the nlmixr2
#' residual error syntax (\code{add()}, \code{prop()}, \code{combined1()},
#' \code{combined2()}).
#'
#' @param model Campsis model
#' @return character vector, one line per statement
#' @export
nlmixr_code <- function(model) {
  cmtNames <- get_unprefixed_compartment_names(model, target = "nlmixr2", allow_unnamed = TRUE)
  prefixedNames <- model@compartments@list %>% purrr::map_chr(~ .x %>% to_string())
  unusedEps <- check_nlmixr_sigmas(model)

  model@model@list <- model@model@list %>%
    purrr::map(function(record) {
      record@statements@list <- record@statements@list %>%
        purrr::map(function(statement) {
          if (is(statement, "endpoint")) {
            return(UnknownStatement(nlmixr_endpoint(statement), comment = statement@comment))
          }
          return(statement)
        })
      return(record)
    })

  renamed <- cmtNames != prefixedNames
  code <- rxode_code(model) %>%
    rename_variables(
      from = c(prefixedNames[renamed], unusedEps),
      to = c(cmtNames[renamed], rep("0", length(unusedEps)))
    )
  return(code)
}
