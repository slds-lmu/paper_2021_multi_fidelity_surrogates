run_with_mlflow = function(xs) {
  mlflow::mlflow_start_run()
  mlr3misc::imap(xs, function(value, name) {
    mlflow::mlflow_log_param(name, as.character(value))
  })
  output = callr::r_safe(run_fit_svm, xs)
  mlr3misc::imap(output, function(value, name) {
    mlflow::mlflow_log_metric(name, as.numeric(value))
  })
  mlflow::mlflow_end_run()
  return(output)
}
