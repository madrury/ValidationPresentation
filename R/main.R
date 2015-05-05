# Make plots for presentation

setwd("/Users/matthewdrury/Presentations/model_validation")
setwd("./R")
source("toy_model.R")
source("meta_models.R")
source("plotting_functions.R")

setwd("..")
dir.create("./plots")
setwd("./plots")


sm <- make_sinmodel(degree=1, n_train_sample=50)
smm <- fit_many_sinmodels(sm, n_to_fit=100)

p <- plot_irreducible_error()
ggsave("irreducible_error.png", p)

p <- ggplot() + area_between_true_and_linear_fit() + signal_plot() + true_linear_fit_plot()
ggsave("model_bias.png")

p <- ggplot() + area_between_true_and_cubic_fit() + signal_plot() + true_cubic_fit_plot()
ggsave("cubic_model_bias.png")

p <- ggplot() + signal_plot() + true_linear_fit_plot()
ggsave("best_linear_fit.png")

p <- plot_many_sinmodels(smm)
ggsave("model_variance.png")


# A plot of the true signal, along with a sample from X, Y
p <- ggplot() + train_data_scatter(sm) + signal_plot()
ggsave("true_signal.png", p)

# True signal and sample, along with the resulting linear regression
p <- p + fitted_plot(sm)
ggsave("single_fitted_line.png", p)