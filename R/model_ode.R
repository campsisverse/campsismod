#_______________________________________________________________________________
#----                             ode class                                 ----
#_______________________________________________________________________________

#'
#' ODE class. Any statement in the form d/dt(CMT) = B.
#'
#' @export
setClass(
  "ode",
  representation(),
  contains = "equation"
)

#'
#' Create a new ordinary differential equation (ODE).
#'
#' @param lhs left-hand side variable corresponding to the compartment name
#'   (e.g. 'CENTRAL', or 'A_CENTRAL' in legacy models)
#' @param rhs right-hand side expression corresponding to derivative value
#' @param comment comment if any, single character string
#' @return an ODE
#' @export
Ode <- function(lhs, rhs = "", comment = as.character(NA)) {
  return(new("ode", lhs = lhs, rhs = rhs, comment = comment))
}

#_______________________________________________________________________________
#----                            get_name                                    ----
#_______________________________________________________________________________

#' @rdname get_name
setMethod("get_name", signature = c("ode"), definition = function(x) {
  return(paste0("ODE (", x@lhs, ")"))
})

#_______________________________________________________________________________
#----                             to_string                                 ----
#_______________________________________________________________________________

#' @rdname to_string
setMethod("to_string", signature = c("ode"), definition = function(object, ...) {
  dest <- process_extra_arg(args = list(...), name = "dest", default = "campsis")
  model <- process_extra_arg(args = list(...), name = "model", default = CampsisModel())

  if (dest == "campsis" || is_rxode(dest)) {
    retValue <- paste0("d/dt(", object@lhs, ")", "=", object@rhs)
  } else if (dest == "mrgsolve") {
    retValue <- paste0("dxdt_", object@lhs, "=", object@rhs, ";")
  } else if (dest == "NONMEM") {
    retValue <- paste0("DADT(", model %>% get_compartment_index(object@lhs), ")", "=", object@rhs)
  } else {
    UnsupportedDestException()
  }
  return(retValue %>% append_comment(object, dest))
})
