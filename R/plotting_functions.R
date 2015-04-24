plot_many_sinmodels <- function(many_sinmodels) {
  p <- ggplot() + signal_plot()
  print(class(p))
  for(model in many_sinmodels) p <- p + fitted_plot(model, alpha=.25) + train_data_scatter(model, alpha=.02)
  p
}