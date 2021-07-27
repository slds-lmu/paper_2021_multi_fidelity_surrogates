devtools::load_all()
library(bbotk)
library(paradox)
library(mlr3)
library(mlr3learners)
library(mlr3mbo)
library(miesmuschel)
library(mlr3hyperband)
library(ggplot2)

# Branin

set.seed(123)
cfg_branin = cfgs("branin")
data_tabular = readRDS("../../multifidelity_data/branin_surrogate/data.rds")
cfg_branin_surrogate = cfgs("branin_surrogate", workdir = "../../multifidelity_data")

#ins_real = OptimInstanceSingleCrit$new(
#  objective = cfg_branin$get_objective(),
#  terminator = trm("none")
#)
#design = setDT(expand.grid(x1 = seq(from = -5, to = 10, length.out = 100), x2 = seq(from = 0, to = 15, length.out = 100), fidelity = 1 / (2 ^ (0:9))))  # due to hyperband eta = 2
#ins_real$eval_batch(design)
#saveRDS(ins_real$archive$data[, c("x1", "x2", "fidelity", "y"), with = FALSE], "../../multifidelity_data/branin_surrogate/data.rds") # this is the data.rds in the BenchmarkConfig directory
#cfg_branin_surrogate$fit_surrogate(overwrite = TRUE)

# Hartmann

set.seed(123)
cfg_hartmann = cfgs("hartmann")
data_tabular = readRDS("../../multifidelity_data/hartmann_surrogate/data.rds")
cfg_hartmann_surrogate = cfgs("hartmann_surrogate", workdir = "../../multifidelity_data")

#ins_real = OptimInstanceSingleCrit$new(
#  objective = cfg_hartmann$get_objective(),
#  terminator = trm("none")
#)
#x = seq(from = 0, to = 1, length.out = 5)
#design = setDT(expand.grid(x1 = x, x2 = x, x3 = x, x4 = x, x5 = x, x6 = x, fidelity = 1 / (2 ^ (0:9))))  # due to hyperband eta = 2
#ins_real$eval_batch(design)
#saveRDS(ins_real$archive$data[, c(paste0("x", 1:6), "fidelity", "y"), with = FALSE], "../../multifidelity_data/hartmann_surrogate/data.rds") # this is the data.rds in the BenchmarkConfig directory
#cfg_hartmann_surrogate$fit_surrogate(overwrite = TRUE)

SamplerRandomTabular = R6Class("SamplerRandomTabular",
  inherit = Sampler,
  public = list(
    #' @field table [data.table::data.table].
    table = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param table ([data.table::data.table]).
    #' @param x_cols (`character()`).
    #' @param y_cols (`character(1)`).
    initialize = function(table, param_set) {
      self$table = assert_data_table(table)

      super$initialize(
        param_set = param_set
      )
    },

    #' @description
    #' Sample `n` values from the distribution.
    #'
    #' @param n (`integer(1)`).
    #' @return [Design].
    sample = function(n) {
      assert_count(n) # we do argcheck on toplevel
      ids = sample(seq_len(NROW(self$table)), size = min(n, NROW(self$table)), replace = FALSE)
      list(data = self$table[ids, ])
    }
  )
)


OptimizerRandomTabular = R6Class("OptimizerRandomTabular",
  inherit = Optimizer,
  public = list(
    #' @field table [data.table::data.table].
    table = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.

    #' @param table ([data.table::data.table]).
    initialize = function(table) {
      self$table = assert_data_table(table)
      param_set = ps(
        batch_size = p_int(default = 1L, tags = "required")
      )
      param_set$values = list(batch_size = 1L)

      super$initialize(
        param_set = param_set,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit", "multi-crit")
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      batch_size = self$param_set$values$batch_size
      table = copy(self$table)
      x_cols = inst$search_space$ids()
      # FIXME: assert column names
      if (batch_size > NROW(table)) batch_size = NROW(table)
      repeat { # iterate until we have an exception from eval_batch
        if (NROW(table) == 0L) stop("No points left.")
        ids = sample(seq_len(NROW(table)), size = min(batch_size, NROW(table)), replace = FALSE)
        design = table[ids, ]
        table = table[-ids, ]
        inst$eval_batch(design)
      }
    }
  )
)

mlr_optimizers$add("random_tabular", OptimizerRandomTabular)

OptimInstanceSingleCritTabular = R6Class("OptimInstanceSingleCritTabular",
  inherit = OptimInstance,
  public = list(
    #' @field table [data.table::data.table].
    table = NULL,

    #' @field x_cols `character()`.
    x_cols = NULL,

    #' @field y_col `character(1)`.
    y_col = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param table ([data.table::data.table]).
    #' @param x_cols (`character()`).
    #' @param y_cols (`character(1)`).
    #' @param terminator ([Terminator]).
    #' @param check_values (`logical(1)`)\cr
    #' Should x-values that are added to the archive be checked for validity?
    #' Search space that is logged into archive.
    initialize = function(table, x_cols, y_col, search_space, direction, terminator, keep_evals = "all", check_values = TRUE) {
      self$x_cols = assert_subset(x_cols, choices = colnames(table))
      self$y_col = assert_choice(y_col, choices = setdiff(colnames(table), x_cols))
      setkeyv(table, c(x_cols, y_col))
      self$table = assert_data_table(table)

      assert_r6(search_space, "ParamSet")
      assert_choice(direction, choices = c("minimize", "maximize"))

      objective = ObjectiveRFunDt$new(
        fun = function(xdt) {
          self$table[xdt, self$y_col, with = FALSE]
        },
        domain = search_space,
        codomain = ParamSet$new(list(ParamDbl$new(id = self$y_col, tags = direction)))
      )

      super$initialize(objective, NULL, terminator, keep_evals, check_values)
    },

    #' @description
    #' The [Optimizer] object writes the best found point
    #' and estimated performance value here. For internal use.
    #'
    #' @param y (`numeric(1)`)\cr
    #' Optimal outcome.
    assign_result = function(xdt, y) {
      # FIXME: We could have one way that just lets us put a 1xn DT as result directly.
      assert_data_table(xdt)
      assert_names(names(xdt), must.include = self$search_space$ids())
      assert_number(y)
      assert_names(names(y), permutation.of = self$objective$codomain$ids())
      x_domain = unlist(transform_xdt_to_xss(xdt, self$search_space), recursive = FALSE)
      if (is.null(x_domain)) x_domain = list()
      private$.result = cbind(xdt, x_domain = list(x_domain), t(y)) # t(y) so the name of y stays
    }
  )
)

get_ins = function(method = c("real", "tabular", "surrogate"), budget = 100) {
  switch(method,
    "real" =
    OptimInstanceSingleCrit$new(
      objective = cfg_branin$get_objective(),
      terminator = trm("budget", budget = budget)
    ),
    "tabular" =
    OptimInstanceSingleCritTabular$new(
      table = data_tabular,
      x_cols = c("x1", "x2", "fidelity"),
      y_col = "y",
      search_space = cfg_branin$param_set,
      direction = "minimize",
      terminator = trm("budget", budget = budget)
    ),
    "surrogate" =
    OptimInstanceSingleCrit$new(
      objective = cfg_branin_surrogate$get_objective(),
      terminator = trm("budget", budget = budget)
    )
  )
}

get_trace = function(archive, m, o) {
  tmp = archive$data
  tmp[, cumbudget := cumsum(fidelity)]
  tmp[, iteration := seq_len(.N)]
  tmp$best = map_dbl(tmp$iteration, function(i) {
    min(tmp[iteration %in% 1:i][["y"]])
  })
  tmp[, method := m]
  tmp[, optimizer := o]
  tmp[, c("cumbudget", "iteration", "best", "method", "optimizer")]
}

