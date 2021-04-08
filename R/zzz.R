#' @import data.table
#' @import keras
#' @import tensorflow
#' @import mlr3
#' @import mlr3misc
#' @import paradox
#' @import mlr3keras
#' @import reticulate
#' @importFrom forcats fct_explicit_na fct_drop
#' @import checkmate
#' @importFrom R6 R6Class
"_PACKAGE"

# FIXME: cleaner import above
.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  backports::import(pkgname)
}
