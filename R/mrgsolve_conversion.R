#' Get the parameters block for mrgsolve.
#'
#' @param model Campsis model
#' @param extra_params extra parameter names to be added. By default, they will be assigned a zero value.
#' @return character vector, 1 parameter per line. First one is header [PARAM].
#' @export
mrgsolve_param <- function(model, extra_params = character(0)) {
  params <- rxode_params(model)
  retValue <- "[PARAM] @annotated"
  for (index in seq_along(params)) {
    param <- params[index]
    retValue <- retValue %>% append(paste0(names(param), " : ", as.numeric(param), " : ", names(param)))
  }
  for (param in extra_params) {
    retValue <- retValue %>% append(paste0(param, " : ", as.numeric(0), " : ", param))
  }
  return(retValue)
}

#' Get the compartment block for mrgsolve.
#'
#' @param model Campsis model
#' @return character vector, each value is a line
#' @export
mrgsolve_compartment <- function(model) {
  compartments <- model@compartments
  retValue <- "[CMT] @annotated"
  for (compartment in compartments@list) {
    # Annotation kept without the legacy 'A_' prefix
    annotation <- sub(pattern = "^A_", replacement = "", x = compartment@name)
    retValue <- retValue %>% append(paste0(compartment %>% to_string(), " : ", annotation))
  }
  return(retValue)
}

#' Get the OMEGA/SIGMA matrix for mrgsolve.
#'
#' @param model Campsis model
#' @param type either omega or sigma
#' @return named matrix or character(0) if matrix is empty
#' @export
mrgsolve_matrix <- function(model, type = "omega") {
  matrix <- rxode_matrix(model, type = type)
  if (nrow(matrix) == 0) {
    return(character(0))
  }
  if (type == "omega") {
    retValue <- "[OMEGA] @annotated @block"
  } else {
    retValue <- "[SIGMA] @annotated @block"
  }
  names <- row.names(matrix)
  for (rowIndex in seq_len(nrow(matrix))) {
    retValue <- retValue %>%
      append(paste0(
        names[rowIndex],
        " : ",
        paste0(matrix[rowIndex, seq_len(rowIndex)], collapse = " "),
        " : ",
        names[rowIndex]
      ))
  }
  return(retValue)
}

#' Get the MAIN block for mrgsolve.
#'
#' @param model Campsis model
#' @return MAIN block
#' @export
mrgsolve_main <- function(model) {
  records <- model@model
  properties <- model@compartments@properties
  retValue <- "[MAIN]"
  record <- records %>% get_by_name("MAIN")
  retValue <- mrgsolve_block(record, init = "[MAIN]")
  if (properties %>% length() > 0) {
    for (property in properties@list) {
      compartmentIndex <- property@compartment
      compartment <- model@compartments %>% find(Compartment(index = compartmentIndex))
      equation <- paste0(property %>% to_string(model = model, dest = "mrgsolve"), ";")
      retValue <- retValue %>% append(equation)
    }
  }
  return(retValue)
}

#' Convert Campsis comment style to C/C++ code.
#' Only the first # is translated to //.
#'
#' @param x any record line
#' @return same line with comments translated to C/C++
#' @keywords internal
convert_any_comment <- function(x) {
  return(sub(pattern = "#", replacement = "//", x = x))
}


#' Convert code record for mrgsolve.
#'
#' @param record code record
#' @param init name of mrgsolve block
#' @param capture 'capture' instead of 'double'
#' @return translated record for mrgsolve
#' @export
mrgsolve_block <- function(record, init = NULL, capture = FALSE) {
  retValue <- init
  if (is.null(record)) {
    return(retValue)
  }
  for (statement in record@statements@list) {
    retValue <-
      retValue %>%
      append(
        statement %>%
          to_string(
            dest = "mrgsolve",
            init = !capture,
            capture = capture
          )
      )
  }
  return(retValue)
}

#' Get the ODE block for mrgsolve.
#'
#' @param model Campsis model
#' @return ODE block
#' @export
mrgsolve_ode <- function(model) {
  records <- model@model
  odeRecord <- records %>% get_by_name("ODE")

  # Automatically replace simulation time 't' (default in Campsis) by SOLVERTIME
  if (!is.null(odeRecord)) {
    odeRecord <- odeRecord %>% campsismod::replace_all(pattern = VariablePattern("t"), replacement = "SOLVERTIME")
  }
  retValue <- mrgsolve_block(odeRecord, init = "[ODE]")
  return(retValue)
}

#' Get the TABLE block for mrgsolve.
#'
#' @param model Campsis model
#' @return TABLE block if at least one line in error record, character(0) otherwise
#' @export
mrgsolve_table <- function(model) {
  records <- model@model
  errorRecord <- records %>% get_by_name("ERROR")
  if (is.null(errorRecord)) {
    return(character(0))
  }
  retValue <- mrgsolve_block(errorRecord, init = "[TABLE]", capture = TRUE)
  return(retValue)
}

#' Get the CAPTURE block for mrgsolve.
#'
#' @param outvars outvars in method simulate
#' @param model Campsis model
#' @return CAPTURE block or character(0) if no variable in outvars
#' @export
mrgsolve_capture <- function(outvars, model) {
  # Get rid of variables that are already in error block (and thus already captured in TABLE)
  outvars <- convert_outvars_to_capture(outvars, model = model)

  if (is.null(outvars) || outvars %>% length() == 0) {
    return(character(0))
  } else {
    return(paste("[CAPTURE]", outvars))
  }
}

#' Convert outvars argument to capture. Variables that are already in error block
#' will be discarded.
#'
#' @param outvars character vector
#' @param model Campsis model
#' @importFrom purrr keep map_chr
#' @return all variables to capture
#' @keywords internal
convert_outvars_to_capture <- function(outvars, model) {
  # List all variables that are already exported into mrgsolve TABLE block by pmxmod
  error <- model@model %>% get_by_name("ERROR")
  list <- NULL
  if (!is.null(error)) {
    list <- error@statements@list %>% purrr::keep(~ is(.x, "equation")) %>% purrr::map_chr(~ .x@lhs)
    outvars <- outvars[!(outvars %in% list)]
  }
  return(outvars)
}
