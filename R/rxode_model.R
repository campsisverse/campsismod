setClass(
  "rxode_model",
  representation(
    code = "character",
    theta = "numeric",
    omega = "matrix",
    sigma = "matrix"
  )
)

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

#' @rdname export
setMethod("export", signature = c("campsis_model", "rxode_type"), definition = function(object, dest, ...) {
  object <- compile_endpoints(object)
  return(
    new(
      "rxode_model",
      code = rxode_code(object),
      theta = rxode_params(object),
      omega = rxode_matrix(object, type = "omega"),
      sigma = rxode_matrix(object, type = "sigma")
    )
  )
})
