plot_many_sinmodels <- function(many_sinmodels, plot_line=TRUE) {
  p <- ggplot() + signal_plot()
  for(model in many_sinmodels) p <- p + fitted_plot(model, alpha=.2)
  if(plot_line) {
    p <- p  + true_linear_fit_plot()
  }
  p
}

plot_irreducible_error <- function() {
  e <- sin(1.5) + rnorm(100, 0, .5)
  d <- data.frame(X=rep(1.5, 100), Y=e)
  ggplot() + signal_plot() + geom_point(data=d, aes(x=X, y=Y), alpha=.25)
}

plot_variance_of_degree <- function(d=1, n_train_sample=50, n_to_fit=100, y_std = .75) {
  sm <- make_sinmodel(degree=d, y_std=y_std, n_train_sample=n_train_sample)
  smm <- fit_many_sinmodels(sm, n_to_fit=n_to_fit)
  p <- plot_many_sinmodels(smm, plot_line=FALSE) + train_data_scatter(sm, alpha=.25)
  p
}

