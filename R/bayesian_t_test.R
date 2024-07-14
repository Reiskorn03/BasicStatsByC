#' Perform Bayesian T-Test with Optional Plot if requested
#'
#' This function performs a Bayesian t-test to compare means between two groups using the BayesFactor package.
#' It also generates a density plot of the posterior distribution if requested.
#'
#' @param data A data frame containing the dataset.
#' @param group_col The name of the column in the data frame that defines the groups (factor or character).
#' @param value_col The name of the numeric column in the data frame for which the t-test is to be performed.
#' @param plot A logical value indicating whether to generate a density plot of the posterior distribution (default is TRUE).
#' @return An object of class \code{BFBayesFactor} containing the results of the Bayesian t-test.
#' @examples
#' bayesian_t_test(mtcars, "am", "mpg")
#' @export
bayesian_t_test <- function(data, group_col, value_col, plot = TRUE) {
  # Ensure the BayesFactor package is available --> very important
  if (!requireNamespace("BayesFactor", quietly = TRUE)) {
    stop("The BayesFactor package is required but not installed. Please try again after the installation.")
  }

  # Extract the groups and values
  groups <- data[[group_col]]
  values <- data[[value_col]]

  # Perform the Bayesian t-test
  bf_result <- BayesFactor::ttestBF(formula = as.formula(paste(value_col, group_col, sep = "~")), data = data)

  # Generate the plot if requested
  if (plot) {
    plot(bf_result)
  }

  return(bf_result)
}
