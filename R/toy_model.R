library(ggplot2)

#-----------------------------------------------------------------------------
# Constructor for new toy model object.
#-----------------------------------------------------------------------------
# Models the following specification for a data generation process:
#   - X ~ U(0, 2*pi)
#   - Y = sin(X) + N(0, y_std)
#   - hat(Y) = polynomial_regression(X)
#
# The object has the following attributes upon creation:
#   - X: Sampled vector of X's for training data.
#   - Y: Sampled vector of Y's for training data.
#   - poly: poly object generated using R's builtin poly object (orthogonal
#       mode).  x_degree determines the degree of polynomial basis to
#       create.
#   - train_data_frame: A training data frame created by combining the
#       polynomial basis with the Y vector.
#   - lm: A polynomial regression fit using the polynomial basis stored
#         in poly, and the response stored in Y.
make_sinmodel <- function(obj=NULL, n_train_samples=10, y_std=.3, x_degree=1) {
  new_model_obj <- structure(list(), class="sinmodel")

  if(is.null(obj)) {
    XY <- .sample_XY(n_samples=n_train_samples, y_std=y_std)
    new_model_obj$X <- XY$X
    new_model_obj$Y <- XY$Y
  } else {
    if(class(obj) != "sinmodel") stop("Only sinmodel's may be used as obj.")
    new_model_obj$X <- obj$X
    new_model_obj$Y <- obj$Y
  }

  new_model_obj$poly <- poly(x=new_model_obj$X, degree=x_degree)
  new_model_obj$train_data_frame <- .sinmodel_data_frame(new_model_obj)
  new_model_obj$lm <- lm(Y ~ . - Y, new_model_obj$train_data_frame)

  new_model_obj
}

predict.sinmodel <- function(sinmodel, newdata=NULL) {
  if(is.null(newdata)) {
    preds <- predict(sinmodel$lm, newdata=sinmodel$train_data_frame)
  } else {
    preds_data_frame <- .sinmodel_data_frame(sinmodel, newdata=newdata)
    preds <- predict(sinmodel$lm, newdata=preds_data_frame)
  }
  preds
}

#-----------------------------------------------------------------------------
# Ploting functions.
#-----------------------------------------------------------------------------
# Make a scatter plot of the underlying training data
train_data_scatter <- function(sinmodel, alpha=.5) {
  plot_data <- data.frame(X=sinmodel$X, Y=sinmodel$Y)
  geom_point(data=plot_data, aes(x=X, y=Y), alpha=alpha)
}

# Plot the underlying sinusoidal signal
signal_plot <- function(alpha=.5, color="blue") {
  X <- .linspace_X()
  plot_data <- data.frame(X=X, Y=sin(X))
  geom_line(data=plot_data, aes(x=X, y=Y), alpha=alpha, color=color)
}

# Make a plot of the fitted model on a set of equally spaced points
fitted_plot <- function(sinmodel, alpha=1, color="black") {
  linspace <- .linspace_X()
  preds <- predict(sinmodel, newdata=linspace)
  plot_data <- data.frame(X=linspace, Y=preds)
  geom_line(data=plot_data, aes(x=X, y=Y), alpha=alpha, color=color)
}


# Private helper functions
#-----------------------------------------------------------------------------

# Make a model data frame from a sinmodel object
# Has two modes, depending on whether newdata is or is not supplied.
#   - If newdata is not supplied, make a data frame out of the stored
#     polynomial basis and the stored X sample.
#   - If newdata is supplied, use the stored polynomial basis to create
#     a new data frame from the passed newdata.
.sinmodel_data_frame <- function(sinmodel, newdata=NULL) {
  if(is.null(newdata)) {
    newdata <- data.frame(sinmodel$poly)
    newdata$Y <- sinmodel$Y
  } else {
    newdata <- data.frame(predict(sinmodel$poly, newdata=newdata))
    newdata$Y <- NA
  }
  newdata
}

# Draw a sample of a specified size from U(0, 2*pi)
.sample_X <- function(n_samples=10) {
  runif(n=n_samples, min=0, max=2*pi)
}

# Draw a conditional sample from sin(X) + N(0, y_std)
.sample_Y <- function(X, y_std=.1) {
  n_samples <- length(X)
  sin(X) + rnorm(n=n_samples, mean=0, sd=y_std)
}

# Combine samples in data frame
.sample_XY <- function(n_samples=10, y_std=.1) {
  X <- .sample_X(n_samples=n_samples)
  Y <- .sample_Y(X=X, y_std=y_std)
  data.frame(X=X, Y=Y)
}

# Simple linspace of equally spaced points on [0, 2*pi]
.linspace_X <- function() {
  seq(from=0, to=2*pi, length.out=100)
}

# Coefficents from a numerical optimization using scipy.
.true_linear_fit <- function(x) {
  -0.30396357*x +  0.95492969
}