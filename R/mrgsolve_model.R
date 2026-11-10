setClass(
  "mrgsolve_model",
  representation(
    param = "character",
    cmt = "character",
    main = "character",
    ode = "character",
    omega = "character",
    sigma = "character",
    table = "character",
    capture = "character"
  )
)

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

#' @param outvars additional variables to capture
#' @param extra_params extra parameter names to be added. By default, they will be assigned a zero value.
#' @rdname export
setMethod(
  "export",
  signature = c("campsis_model", "mrgsolve_type"),
  definition = function(object, dest, outvars = NULL, extra_params = character(0)) {
    object <- compile_endpoints(object)
    return(
      new(
        "mrgsolve_model",
        param = mrgsolve_param(object, extra_params = extra_params),
        cmt = mrgsolve_compartment(object),
        main = mrgsolve_main(object),
        ode = mrgsolve_ode(object),
        omega = mrgsolve_matrix(object, type = "omega"),
        sigma = mrgsolve_matrix(object, type = "sigma"),
        table = mrgsolve_table(object),
        capture = mrgsolve_capture(outvars, model = object)
      )
    )
  }
)

#_______________________________________________________________________________
#----                             to_string                                 ----
#_______________________________________________________________________________

#' @rdname to_string
setMethod("to_string", signature = c("mrgsolve_model"), definition = function(object, ...) {
  cpp <- NULL
  if (!is.null(object@param)) {
    cpp <- cpp %>% append(object@param)
    cpp <- cpp %>% append("")
  }
  cpp <- cpp %>% append(object@cmt)
  cpp <- cpp %>% append("")
  if (!is.null(object@omega)) {
    cpp <- cpp %>% append(object@omega)
    cpp <- cpp %>% append("")
  }
  if (!is.null(object@sigma)) {
    cpp <- cpp %>% append(object@sigma)
    cpp <- cpp %>% append("")
  }
  cpp <- cpp %>% append(object@main)
  cpp <- cpp %>% append("")
  cpp <- cpp %>% append(object@ode)
  cpp <- cpp %>% append("")
  cpp <- cpp %>% append(object@table)
  if (!is.null(object@capture)) {
    cpp <- cpp %>% append("")
    cpp <- cpp %>% append(object@capture)
  }
  cpp <- paste0(cpp, collapse = "\n")
  return(cpp)
})
