#' Fit a Bayesian Linear Regression Model and a plot if requested
#'
#' This function fits a Bayesian linear regression model and returns the results.
#' It also generates a plot of the posterior distribution if requested.
#'
#' @param data A data frame containing the dataset.
#' @param formula A formula specifying the model.
#' @param plot A logical value indicating whether to generate a plot of the posterior distribution (default is TRUE).
#' @return An object of class \code{BFBayesFactor} containing the results of the Bayesian linear regression.
#' @examples
#' bayesian_lm(mtcars, mpg ~ wt)
#' @export
bayesian_lm <- function(data, formula, plot = TRUE) {
  # Ensure the BayesFactor package is available --> very important!
  if (!requireNamespace("BayesFactor", quietly = TRUE)) {
    stop("The BayesFactor package is required but not installed. Please try again after installation.")
  }

  # Fit the Bayesian linear model
  bf_result <- BayesFactor::lmBF(formula = formula, data = data)

  # Generate the plot if requested
  if (plot) {
    plot(bf_result)
  }

  return(bf_result)
}
