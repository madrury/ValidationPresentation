# Make plots for presentation

setwd("/Users/matthewdrury/Presentation/model_validation")
setwd("./R")
source("toy_model.R")
setwd("..")
dir.create("./plots")
setwd("./plots")


sm <- make_sinmodel(degree=1, n_train_sample=50)

# A plot of the true signal, along with a sample from X, Y
p <- ggplot() + train_data_scatter(sm) + signal_plot()
ggsave("true_signal.png", p)

# True signal and sample, along with the resulting linear regression
p <- p + fitted_plot(sm)
ggsave("single_fitted_line.png", p)
