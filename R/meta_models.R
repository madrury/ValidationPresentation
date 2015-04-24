fit_many_sinmodels <- function(obj, n_to_fit) {

  models <- list()

  for(i in 1:n_to_fit) {
    models[[i]] <- make_sinmodel(n_train_sample=obj$n_train_sample, y_std=obj$y_std, degree=obj$degree)
  }

  models
}