#_______________________________________________________________________________
#----                         campsis_model class                           ----
#_______________________________________________________________________________

#' Campsis model class.
#'
#' @slot model a list of code records
#' @slot parameters model parameters
#' @slot compartments model compartments
#' @slot covariates model covariate definitions
#' @export
setClass(
  "campsis_model",
  representation(
    model = "code_records",
    parameters = "parameters",
    compartments = "compartments",
    covariates = "covariate_definitions"
  ),
  prototype = prototype(
    model = new("code_records"),
    parameters = new("parameters"),
    compartments = new("compartments"),
    covariates = new("covariate_definitions")
  )
)

#' Create a new Campsis model.
#'
#' @param json path to JSON Campsis model file or JSON content in string form
#' @return a Campsis model, empty
#' @export
CampsisModel <- function(json = NULL) {
  if (is.null(json)) {
    model <- new("campsis_model")
  } else {
    schema <- system.file("extdata", "campsismod.schema.json", package = "campsismod")
    model <- load_from_json(CampsisModel(), open_json(json = json, schema = schema))
  }
  return(model)
}

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________

#' @rdname add
setMethod("add", signature = c("campsis_model", "compartment_property"), definition = function(object, x) {
  compartment <- object@compartments %>% find(Compartment(index = x@compartment))
  if (is.null(compartment)) {
    stop(paste0("Unable to find compartment ", x@compartment, " in Campsis model"))
  }

  # Add characteristic (delegate to add method in compartments class)
  object@compartments <- object@compartments %>% add(x)

  return(object)
})

#' @rdname add
setMethod("add", signature = c("campsis_model", "parameter"), definition = function(object, x) {
  object@parameters <- object@parameters %>% add(x)
  return(object)
})

#' @rdname add
setMethod("add", signature = c("campsis_model", "covariate_definition"), definition = function(object, x) {
  object@covariates <- object@covariates %>% add(x)
  return(object)
})

#' @rdname add
setMethod("add", signature = c("campsis_model", "code_record"), definition = function(object, x) {
  object@model <- object@model %>% add(x)
  return(object)
})

#' @param pos position where x needs to be added in list
#' @rdname add
setMethod("add", signature = c("campsis_model", "model_statement"), definition = function(object, x, pos = NULL) {
  if (is(x, "ode")) {
    object@compartments <- object@compartments %>% add_ode_compartment(ode = x)
  }
  object@model <- object@model %>% add(x, pos = pos)
  return(object)
})

#' @rdname add
setMethod("add", signature = c("campsis_model", "campsis_model"), definition = function(object, x) {
  object <- object %>% appendModel(x)
  return(object)
})

#' @rdname add
setMethod("add", signature = c("campsis_model", "list"), definition = function(object, x) {
  for (elem in x) {
    object <- object %>% add(elem)
  }
  return(object)
})

#' Append model (or simply add).
#'
#' @param model1 base model
#' @param model2 model to append
#' @return the resulting Campsis model
#' @keywords internal
#'
appendModel <- function(model1, model2) {
  # Append compartments (and included properties)
  model1@parameters <- model1@parameters %>% add(model2@parameters)

  # Append code records
  model1@model <- model1@model %>% add(model2@model)

  # Append compartments (and included properties)
  model1@compartments <- model1@compartments %>% add(model2@compartments)

  # Append covariate definitions
  model1@covariates <- model1@covariates %>% add(model2@covariates)

  return(model1)
}

#_______________________________________________________________________________
#----                             add_rse                                    ----
#_______________________________________________________________________________

#' @rdname add_rse
setMethod(
  "add_rse",
  signature = c("campsis_model", "parameter", "numeric"),
  definition = function(object, parameter, value, ...) {
    object@parameters <- object@parameters %>%
      add_rse(parameter = parameter, value = value, ...)
    return(object)
  }
)

#_______________________________________________________________________________
#----                        auto_detect_nonmem                             ----
#_______________________________________________________________________________

#' @rdname auto_detect_nonmem
setMethod("auto_detect_nonmem", signature = c("campsis_model"), definition = function(object, ...) {
  main <- object@model %>% get_by_name("MAIN")
  numberOfCmts <- object@compartments %>% length()

  for (cmtIndex in seq_len(numberOfCmts)) {
    # Search for bioavailability
    fVar <- paste0("F", cmtIndex)
    if (main %>% find(Equation(fVar)) %>% length() > 0) {
      object <- object %>% add(Bioavailability(cmtIndex, rhs = fVar))
    }
    # Search for infusion duration
    dVar <- paste0("D", cmtIndex)
    if (main %>% find(Equation(dVar)) %>% length() > 0) {
      object <- object %>% add(InfusionDuration(cmtIndex, rhs = dVar))
    }
    # Search for infusion rate
    rVar <- paste0("R", cmtIndex)
    if (main %>% find(Equation(rVar)) %>% length() > 0) {
      object <- object %>% add(InfusionRate(cmtIndex, rhs = rVar))
    }
    # Search for lag time
    alagVar <- paste0("ALAG", cmtIndex)
    if (main %>% find(Equation(alagVar)) %>% length() > 0) {
      object <- object %>% add(LagTime(cmtIndex, rhs = alagVar))
    }
  }

  return(object)
})

#_______________________________________________________________________________
#----                               contains                                ----
#_______________________________________________________________________________

#' @rdname contains
setMethod("contains", signature = c("campsis_model", "pmx_element"), definition = function(object, x) {
  return(!is.null(object %>% find(x)))
})

#_______________________________________________________________________________
#----                               delete                                  ----
#_______________________________________________________________________________

#' @rdname delete
setMethod("delete", signature = c("campsis_model", "compartment_property"), definition = function(object, x) {
  object@compartments <- object@compartments %>% delete(x)
  return(object)
})

#' @rdname delete
setMethod("delete", signature = c("campsis_model", "parameter"), definition = function(object, x) {
  object@parameters <- object@parameters %>% delete(x)
  return(object)
})

#' @rdname delete
setMethod("delete", signature = c("campsis_model", "covariate_definition"), definition = function(object, x) {
  object@covariates <- object@covariates %>% delete(x)
  return(object)
})

#' @rdname delete
setMethod("delete", signature = c("campsis_model", "code_record"), definition = function(object, x) {
  object@model <- object@model %>% delete(x)
  return(object)
})

#' @rdname delete
setMethod("delete", signature = c("campsis_model", "model_statement"), definition = function(object, x) {
  object@model <- object@model %>% delete(x)
  return(object)
})

#_______________________________________________________________________________
#----                              disable                                  ----
#_______________________________________________________________________________

#' @rdname disable
setMethod("disable", signature = c("campsis_model", "character"), definition = function(object, x, ...) {
  object@parameters <- object@parameters %>% disable(x = x, ...)
  return(object)
})

#_______________________________________________________________________________
#----                           export_type                                 ----
#_______________________________________________________________________________

#' RxODE/rxode2 export type class.
#'
#' @export
setClass(
  "rxode_type",
  representation(),
  contains = "export_type"
)

#' Mrgsolve export type class.
#'
#' @export
setClass(
  "mrgsolve_type",
  representation(),
  contains = "export_type"
)

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

#' @rdname export
setMethod("export", signature = c("campsis_model", "character"), definition = function(object, dest, ...) {
  if (is_rxode(dest)) {
    return(object %>% export(new("rxode_type")))
  } else if (dest == "mrgsolve") {
    return(object %>% export(new("mrgsolve_type"), ...))
  } else {
    stop("Only rxode2 (previously RxODE) and mrgsolve are currently supported")
  }
})

#_______________________________________________________________________________
#----                                find                                   ----
#_______________________________________________________________________________

#' @rdname find
setMethod("find", signature = c("campsis_model", "compartment"), definition = function(object, x) {
  return(object@compartments %>% find(x))
})

#' @rdname find
setMethod("find", signature = c("campsis_model", "compartment_property"), definition = function(object, x) {
  return(object@compartments %>% find(x))
})

#' @rdname find
setMethod("find", signature = c("campsis_model", "parameter"), definition = function(object, x) {
  return(object@parameters %>% find(x))
})

#' @rdname find
setMethod("find", signature = c("campsis_model", "covariate_definition"), definition = function(object, x) {
  return(object@covariates %>% find(x))
})

#' @rdname find
setMethod("find", signature = c("campsis_model", "code_record"), definition = function(object, x) {
  return(object@model %>% find(x))
})

#' @rdname find
setMethod("find", signature = c("campsis_model", "model_statement"), definition = function(object, x) {
  return(object@model %>% find(x))
})

#_______________________________________________________________________________
#----                          export_to_json                               ----
#_______________________________________________________________________________

#' @rdname export_to_json
#' @importFrom utils capture.output
setMethod("export_to_json", signature = c("campsis_model"), definition = function(object, ...) {
  # Delete error record if empty
  errorRecord <- object %>%
    find(ErrorRecord())
  if (!is.null(errorRecord) && length(errorRecord) == 0) {
    object <- object %>%
      delete(ErrorRecord())
  }
  lines <- capture.output(show(object@model %>% add_properties_records(model = object)))
  json <- list()
  json$code <- lines
  json$parameters <- export_to_json(object@parameters)@data
  if (length(object@covariates) > 0) {
    json$covariates <- export_to_json(object@covariates)@data
  }
  if (length(object@parameters@varcov) > 0) {
    json$varcov <- varcov_to_json(parameters = object@parameters)
  }
  return(JSONElement(json))
})

#_______________________________________________________________________________
#----                         get_compartment_index                         ----
#_______________________________________________________________________________

#' @rdname get_compartment_index
setMethod("get_compartment_index", signature = c("campsis_model", "character"), definition = function(object, name) {
  return(object@compartments %>% get_compartment_index(name = name))
})

#_______________________________________________________________________________
#----                          get_uncertainty                               ----
#_______________________________________________________________________________

#' @rdname get_uncertainty
setMethod("get_uncertainty", signature = c("campsis_model"), definition = function(object, ...) {
  return(object@parameters %>% get_uncertainty(...))
})

#_______________________________________________________________________________
#----                            get_var_cov                                ----
#_______________________________________________________________________________

#' @rdname get_var_cov
setMethod("get_var_cov", signature = c("campsis_model"), definition = function(object) {
  return(object@parameters %>% get_var_cov())
})

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

#' @rdname load_from_json
setMethod("load_from_json", signature = c("campsis_model", "json_element"), definition = function(object, json) {
  object <- json_to_campsis_model(object = object, json = json)
  return(object)
})

#' @rdname load_from_json
setMethod("load_from_json", signature = c("campsis_model", "character"), definition = function(object, json) {
  schema <- system.file("extdata", "campsismod.schema.json", package = "campsismod")
  return(load_from_json(object = object, json = open_json(json = json, schema = schema)))
})

#_______________________________________________________________________________
#----                              move                                     ----
#_______________________________________________________________________________

#' @rdname move
setMethod("move", signature = c("campsis_model", "ANY", "pmx_position"), definition = function(object, x, to, ...) {
  object@model <- object@model %>% move(x = x, to = to, ...)
  return(object)
})

#_______________________________________________________________________________
#----                                 read                                  ----
#_______________________________________________________________________________

#' Read a Campsis model.
#'
#' @param file path to folder (old format) or path to JSON file (new format)
#' @return Campsis model
#' @export
read.campsis <- function(file) {
  if (dir.exists(file)) {
    folder <- file

    # model.campsis and model.pmx are both accepted
    modelPath <- file.path(folder, "model.campsis")
    if (!file.exists(modelPath)) {
      modelPath <- file.path(folder, "model.pmx")
      warning("Please rename your model file to 'model.campsis'")
      if (!file.exists(modelPath)) {
        stop(paste0("Model file couln't be found."))
      }
    }

    # Construct Campsis model
    records <- read.model(file = modelPath)
    parameters <- read.allparameters(folder = folder)
    model <- new("campsis_model", model = records, parameters = parameters, compartments = Compartments())
    model <- model %>%
      update_compartments()
  } else if (file.exists(file)) {
    if (!endsWith(file, ".json")) {
      stop("Only JSON model files can be opened.")
    }
    model <- load_from_json(CampsisModel(), paste0(readLines(file), collapse = "\n"))
  } else {
    stop("file is not a JSON model or a valid Campsis model folder")
  }

  # Validate the whole model
  methods::validObject(model, complete = TRUE)

  return(model)
}


#' Read a Campsis model (deprecated).
#'
#' @param file path to folder
#' @return a Campsis model
#' @export
#' @keywords internal
read.pmxmod <- function(file) {
  .Deprecated("read.campsis")
  return(read.campsis(file = file))
}

#' Update compartments list from the persisted records.
#' Exported especially for package \code{campsistrans}.
#' However, this method should not be called.
#'
#' @param model Campsis model
#' @return an updated Campsis model, with an updated compartments list
#' @export
update_compartments <- function(model) {
  if (!is(model, "campsis_model")) {
    stop("model is not a Campsis model")
  }
  records <- model@model

  # Get list of compartments
  compartments <- records %>% get_compartments()

  # Extract characteristics
  compartments <- compartments %>% add_properties(records, "F", init = Bioavailability(0, rhs = ""))
  compartments <- compartments %>% add_properties(records, "LAG", init = LagTime(0, rhs = ""))
  compartments <- compartments %>% add_properties(records, "DURATION", init = InfusionDuration(0, rhs = ""))
  compartments <- compartments %>% add_properties(records, "RATE", init = InfusionRate(0, rhs = ""))
  compartments <- compartments %>% add_properties(records, "INIT", init = InitialCondition(0, rhs = ""))

  # Remove properties records because information is found in properties
  records@list <- records@list %>% purrr::keep(~ !is(.x, "properties_record"))

  model@model <- records
  model@compartments <- compartments
  return(model)
}

#_______________________________________________________________________________
#----                               replace                                 ----
#_______________________________________________________________________________

#' @rdname replace
setMethod("replace", signature = c("campsis_model", "compartment"), definition = function(object, x) {
  object@compartments <- object@compartments %>% replace(x)
  return(object)
})

#' @rdname replace
setMethod("replace", signature = c("campsis_model", "compartment_property"), definition = function(object, x) {
  object@compartments <- object@compartments %>% replace(x)
  return(object)
})

#' @rdname replace
setMethod("replace", signature = c("campsis_model", "parameter"), definition = function(object, x) {
  object@parameters <- object@parameters %>% replace(x)
  return(object)
})

#' @rdname replace
setMethod("replace", signature = c("campsis_model", "covariate_definition"), definition = function(object, x) {
  object@covariates <- object@covariates %>% replace(x)
  return(object)
})

#' @rdname replace
setMethod("replace", signature = c("campsis_model", "code_record"), definition = function(object, x) {
  object@model <- object@model %>% replace(x)
  return(object)
})

#' @rdname replace
setMethod("replace", signature = c("campsis_model", "model_statement"), definition = function(object, x) {
  object@model <- object@model %>% replace(x)
  return(object)
})

#_______________________________________________________________________________
#----                             replace_all                                ----
#_______________________________________________________________________________

#' @rdname replace_all
setMethod(
  "replace_all",
  signature = c("campsis_model", "pattern", "character"),
  definition = function(object, pattern, replacement, ...) {
    # Call 'replace_all' on each code record
    object@model@list <- object@model@list %>%
      purrr::map(~ .x %>% replace_all(pattern = pattern, replacement = replacement, ...))

    # Call 'replace_all' on each compartment property
    object@compartments@properties@list <- object@compartments@properties@list %>%
      purrr::map(~ .x %>% replace_all(pattern = pattern, replacement = replacement, ...))

    return(object)
  }
)

#' @rdname replace_all
setMethod(
  "replace_all",
  signature = c("campsis_model", "character", "character"),
  definition = function(object, pattern, replacement, ...) {
    # Call 'replace_all' on each code record
    object@model@list <- object@model@list %>%
      purrr::map(~ .x %>% replace_all(pattern = VariablePattern(pattern), replacement = replacement, ...))

    # Call 'replace_all' on each compartment property
    object@compartments@properties@list <- object@compartments@properties@list %>%
      purrr::map(~ .x %>% replace_all(pattern = VariablePattern(pattern), replacement = replacement, ...))

    return(object)
  }
)

#_______________________________________________________________________________
#----                           set_min_max                                 ----
#_______________________________________________________________________________

#' @rdname set_min_max
setMethod(
  "set_min_max",
  signature = c("campsis_model", "parameter", "numeric", "numeric"),
  definition = function(object, parameter, min, max, ...) {
    object@parameters <- object@parameters %>%
      set_min_max(parameter = parameter, min = min, max = max, ...)
    return(object)
  }
)

#' @rdname set_min_max
setMethod(
  "set_min_max",
  signature = c("campsis_model", "character", "numeric", "numeric"),
  definition = function(object, parameter, min, max, ...) {
    object@parameters <- object@parameters %>%
      set_min_max(parameter = parameter, min = min, max = max, ...)
    return(object)
  }
)

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature = c("campsis_model"), definition = function(object) {
  show(object@model %>% add_properties_records(model = object))
  cat("\n")
  show(object@parameters)
  cat("\n")
  show(object@compartments)
  if (length(object@covariates) > 0) {
    cat("\n")
    show(object@covariates)
  }
})

#_______________________________________________________________________________
#----                                  sort                                 ----
#_______________________________________________________________________________

#' @rdname sort
setMethod("sort", signature = c("campsis_model"), definition = function(x, decreasing = FALSE, ...) {
  # Sort code records
  x@model <- x@model %>% sort()

  # Sort compartments (properties will be sorted correctly)
  x@compartments <- x@compartments %>% sort()

  # Sort parameters
  x@parameters <- x@parameters %>% sort()

  # Sort covariate definitions
  x@covariates <- x@covariates %>% sort()

  return(x)
})

#_______________________________________________________________________________
#----                            standardise                                ----
#_______________________________________________________________________________

#' @rdname standardise
setMethod("standardise", signature = c("campsis_model"), definition = function(object, ...) {
  object@parameters <- object@parameters %>% standardise(...)
  return(object)
})

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

#' @rdname write
setMethod("write", signature = c("campsis_model", "character"), definition = function(object, file, ...) {
  if (endsWith(file, ".json")) {
    return(export_to_json(object) %>% campsismod::write(file = file))
  }

  zip <- process_extra_arg(args = list(...), name = "zip", default = FALSE)
  records <- object@model
  parameters <- object@parameters

  if (length(object@covariates) > 0) {
    warning("Covariates are not exported. Please use the newer JSON format (model %>% write('model.json')) to save your covariates.")
  }

  if (zip) {} else {
    if (dir.exists(file)) {
      # do nothing
    } else {
      dir.create(file)
    }
    records %>% write(file = file.path(file, "model.campsis"), model = object)
    parameters %>% write(file = file)
  }
  return(TRUE)
})

#' @rdname write
#' @param digits significant digits in JSON file, default is 12
setMethod("write", signature = c("json_element", "character"), definition = function(object, file, digits = 12, ...) {
  jsonlite::write_json(object@data, path = file, pretty = TRUE, auto_unbox = TRUE, digits = digits)
  return(TRUE)
})
