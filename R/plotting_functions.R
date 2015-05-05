plot_many_sinmodels <- function(many_sinmodels) {
  p <- ggplot() + signal_plot()
  for(model in many_sinmodels) p <- p + fitted_plot(model, alpha=.2)
  p <- p  + true_linear_fit_plot()
  p
}

plot_irreducible_error <- function() {
  e <- sin(1.5) + rnorm(100, 0, .5)
  d <- data.frame(X=rep(1.5, 100), Y=e)
  ggplot() + signal_plot() + geom_point(data=d, aes(x=X, y=Y), alpha=.25)
}

