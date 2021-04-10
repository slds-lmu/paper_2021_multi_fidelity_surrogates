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
    initialize = function(model_path, trafo_dict, domain, full_codomain_names, codomain = NULL, id = "ONNX", active_session = TRUE,
      properties = character(), constants = ParamSet$new(), check_values = TRUE) {
      if (is.null(codomain)) {
        codomain = ParamSet$new(list(ParamDbl$new("y", tags = "minimize")))
      }
      # Store dictionary of feature transformations
      self$active_session = assert_flag(active_session)
      self$trafo_dict = assert_list(trafo_dict)
      if (self$active_session) {
        # Import runtime and start session
        rt = reticulate::import("onnxruntime")
        self$session = sess = rt$InferenceSession(model_path)
      }
      fun = function(xdt) {
        browser()
        li = c(
          mlr3misc::imap(mlr3misc::keep(xdt, is.character), char_to_int, self$trafo_dict),
          # Below is a little odd but required as-is since otherwise autoconvert to float64 happens
          continuous = list(reticulate::r_to_py(as.matrix(trafo_numerics(xdt, self$trafo_dict)))$astype("float32"))
        )

        if (!self$active_session) {
          rt = reticulate::import("onnxruntime")
          session = rt$InferenceSession(model_path)
        } else {
          session = self$session
        }
        setNames(data.table(session$run(NULL, li)[[1L]]), nm = full_codomain_names)
      }

      super$initialize(id = id, fun = fun, domain = domain, codomain = codomain,
        properties = properties, constants = constants, check_values = check_values)
    }
  )
)
