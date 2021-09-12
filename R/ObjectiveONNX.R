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
    model_path = NULL,
    data_order = NULL,
    trafo_dict = NULL,
    full_codomain = NULL,
    session = NULL,
    active_session = NULL,
    retrafo = FALSE,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param model_path (`character`)\cr
    #'   Path to the onnx Model.
    #' @param data_order (`character`)\cr
    #'   Order of columns in data.
    #' @param trafo_dict (`list`)\cr
    #'   Dictionary containing feature transformations before beeing fed to the NN.
    #' @param full_codomain ([paradox::ParamSet])\cr
    #'   Full codomain.
    #' @param task (`character(1)`)\cr
    #'   Name of a task to be fixed.
    #' @param id (`character(1)`).
    #' @param active_session (`logical(1)`)\cr
    #'   Whether an active session should be used.
    #' @param retrafo (`logical(1)`)\cr
    #'  Should params be trafoed back to their original range before return?
    #' @param properties (`character()`).
    initialize = function(model_path, data_order, trafo_dict, domain, full_codomain, codomain, task = NULL, id = "ONNX", active_session = TRUE, retrafo = FALSE,
      properties = character(), constants = NULL, check_values = FALSE) {
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
      # FIXME: assertions
      self$model_path = model_path
      self$data_order = data_order
      self$full_codomain = full_codomain
      self$retrafo = retrafo

      fun = function(xdt) {
        # remove all-missing cols
        #xdt[, (which(colSums(is.na(xdt)) != 0)) := NULL]
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
        # re-order columns in case the order changed through trafos
        xdt = xdt[, self$data_order, with = FALSE]

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
        dt = pmin(pmax(dt, 0 + .Machine$double.eps), 1 - .Machine$double.neg.eps)  # surrogate outputs MUST be in [0, 1] due to sigmoid
        full_codomain_names = self$full_codomain$ids()
        if (self$retrafo) {
          dt = data.table(retrafo_predictions(dt, target_names = full_codomain_names, codomain = self$full_codomain, trafo_dict = self$trafo_dict))
        } else {
          dt = setNames(data.table(dt), nm = full_codomain_names)
        }
        return(dt)
      }
      super$initialize(id = id, fun = fun, domain = new_domain, codomain = codomain,
        properties = properties, constants = constants, check_values = check_values)
    }
  )
)

#' @export
convert_for_onnx = function(xdt, data_order, param_set, trafo_dict) {
  setDT(xdt)
  ids = param_set$ids()
  missing = ids[mlr3misc::`%nin%`(ids, names(xdt))]
  if (length(missing)) {
    xdt[, (missing) := NA]
  }
  xdt = xdt[, data_order, with = FALSE]
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

