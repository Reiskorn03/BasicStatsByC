#' Compute Bayesian Correlation with Optional Plot if requested
#'
#' This function computes the Bayesian correlation between two continuous variables using the BayesFactor package.
#' It also generates a scatter plot with the correlation line if requested.
#'
#' @param data A data frame containing the dataset.
#' @param x The name of the first continuous variable.
#' @param y The name of the second continuous variable.
#' @param plot A logical value indicating whether to generate a scatter plot with the correlation line (default is TRUE).
#' @return An object of class \code{BFBayesFactor} containing the results of the Bayesian correlation analysis.
#' @examples
#' bayesian_correlation(mtcars, "mpg", "wt")
#' @export
bayesian_correlation <- function(data, x, y, plot = TRUE) {
  # Ensure the BayesFactor package is available -->very important!
  if (!requireNamespace("BayesFactor", quietly = TRUE)) {
    stop("The BayesFactor package is required but not installed. Please try again after installation.")
  }

  # Compute the Bayesian correlation
  bf_result <- BayesFactor::correlationBF(data[[x]], data[[y]])

  # Generate the scatter plot if requested
  if (plot) {
    plot(data[[x]], data[[y]], main = paste("Scatter Plot of", x, "and", y),
         xlab = x, ylab = y)
    abline(lm(data[[y]] ~ data[[x]]), col = "red")
  }

  return(bf_result)
}
