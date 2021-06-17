sessiondict = new.env()




#' @title Objective interface for ONNX Models.
#'
#' @description
#' Objective interface where user can pass an R function that works on an `data.table()`.
#'
# @template param_domain
# @template param_codomain
# @template param_xdt
# @template param_check_values
# @template param_constants
#' @export
ObjectiveONNX = R6Class("ObjectiveONNX",
  inherit = bbotk::ObjectiveRFunDt,
  public = list(
    trafo_dict = list(),
    session = list(),
    active_session = FALSE,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param model_path (`character`)\cr
    #'   Path to the onnx Model.
    #' @param trafo_dict (`character`)\cr
    #'   Dictionary containing feature transformations before beeing fed to the NN.
    #' @param id (`character(1)`).
    #' @param properties (`character()`).
    #' @param retrafo (`character`)\cr
    #'  Should params be trafoed back to their original range before return?
    initialize = function(model_path, trafo_dict, domain, full_codomain_names, codomain = NULL, task = NULL, id = "ONNX", active_session = TRUE, retrafo = FALSE,
      properties = character(), constants = NULL, check_values = FALSE) {
      self$check_values = check_values
      if (is.null(codomain)) {
        codomain = ParamSet$new(list(ParamDbl$new("y", tags = "minimize")))
      }
      if (!is.null(task)) {
        task_id = domain$params[[domain$ids(tags = "task_id")]]
        task_id$default = task
        task_id$tags = c(task_id$tags, "constant")

        # Drop the constant task_id from the domain
        # FIXME: would be way easier if we could drop params from the paramset
        new_domain = ParamSet$new(domain$params[names(domain$params) != domain$ids(tags = "task_id")])
        new_domain$trafo = domain$trafo
        new_domain$deps = domain$deps
        constants = ParamSet$new(list(task_id))
      } else {
        new_domain = domain
        constants = ParamSet$new()
      }
      # Store dictionary of feature transformations
      self$active_session = assert_flag(active_session)
      self$trafo_dict = assert_list(trafo_dict)
      if (self$active_session) {
        # Import runtime and start session
        if (!is.null(sessiondict[[model_path]])) {
          self$session = sess = sessiondict[[model_path]]
        } else {
          rt = reticulate::import("onnxruntime")
          opts = rt$SessionOptions()
          opts$inter_op_num_threads = 1L
          opts$intra_op_num_threads = 1L
          self$session = sess = rt$InferenceSession(model_path, sess_options = opts)
          sessiondict[[model_path]] = sess
        }
      }
      fun = function(xdt) {
        # Handle constants in-place
        if (!self$constants$is_empty) {
          for (constant in self$constants$params) {
            xdt[, constant$id := constant$default]
          }
        }
        # In the case of deps, params with NA will have been dropped internally
        # We re-add them here with the right storage type
        param_ids = self$domain$ids()
        to_add = param_ids[param_ids %nin% names(xdt)]
        for (i in seq_along(to_add)) {
          NA_storage_type = switch(self$domain$params[[to_add[i]]]$storage_type,
            "numeric" = NA_real_,
            "integer" = NA_integer_,
            "character" = NA_character_,
            "logical" = NA_integer_,
            "list" = NA,
          )
          xdt[, to_add[i] := NA_storage_type]
        }
        # Re-order columns in case the order changed through trafos.
        xdt = xdt[, (c(param_ids, self$constants$ids())), with = FALSE]

        li = c(
          mlr3misc::imap(mlr3misc::keep(xdt, function(x) is.character(x) || is.factor(x)), char_to_int, self$trafo_dict),
          # Below is a little odd but required as-is since otherwise autoconvert to float64 happens
          continuous = list(reticulate::r_to_py(trafo_numerics(xdt, self$trafo_dict))$astype("float32"))
        )

        if (!self$active_session) {
          if (!is.null(sessiondict[[model_path]])) {
            self$session = sess = sessiondict[[model_path]]
          } else {
            rt = reticulate::import("onnxruntime")
            opts = rt$SessionOptions()
            opts$inter_op_num_threads = 1L
            opts$intra_op_num_threads = 1L
            session = rt$InferenceSession(model_path, sess_options = opts)
            sessiondict[[model_path]] = session
          }
        } else {
          session = self$session
        }
        dt = session$run(NULL, li)[[1L]]
        if (retrafo) {
          dt = data.table(retrafo_predictions(dt, full_codomain_names, self$trafo_dict))
        } else {
          dt = setNames(data.table(dt, full_codomain_names))
        }
        return(dt)
      }
      super$initialize(id = id, fun = fun, domain = new_domain, codomain = codomain,
        properties = properties, constants = constants, check_values = check_values)
    }
  )
)

#' @export
convert_for_onnx = function(xdt, param_set, trafo_dict) {
  setDT(xdt)
  param_ids = param_set$ids()
  constant_ids = NULL
  ids = c(param_ids, constant_ids)  # FIXME: why would we treat constants differently (also above)?
  missing = ids[mlr3misc::`%nin%`(ids, names(xdt))]
  if (length(missing)) {
    xdt[, (missing) := NA]
  }
  xdt = xdt[, (c(param_ids, constant_ids)), with = FALSE]
  xdt = convert_storage_type(xdt, param_set = param_set)

  li = c(
    mlr3misc::imap(mlr3misc::keep(xdt, function(x) is.character(x) || is.factor(x)), char_to_int, trafo_dict),
    continuous = list(trafo_numerics(xdt, trafo_dict))
  )
  li
}

convert_storage_type = function(xdt, param_set = param_set) {
  imap(xdt, function(x, nm) {
    switch(param_set$storage_type[[nm]],
      "character" = as.character(x),
      "integer" = as.integer(x),
      "logical" = as.logical(x),
      "numeric" = as.numeric(x)
      # FIXME: list for uty not supported
    )
  })
}

