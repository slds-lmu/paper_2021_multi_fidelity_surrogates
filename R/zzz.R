#' @import data.table
#' @import mlr3misc
#' @import paradox
#' @import reticulate
#' @import keras
#' @importFrom forcats fct_explicit_na fct_drop
#' @importFrom bbotk ObjectiveRFunDt
#' @import checkmate
#' @importFrom R6 R6Class
"_PACKAGE"

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  backports::import(pkgname)
}
