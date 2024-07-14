#' Fit a Simple Linear Regression Model with a optional plot (if requested)
#'
#' This function fits a simple linear regression model and returns the summary of the model.
#' It also generates a scatter plot with the regression line and a residual plot if requested.
#'
#' @param data A data frame containing the dataset.
#' @param formula A formula specifying the model.
#' @param plot A logical value indicating whether to generate plots (default is TRUE).
#' @return A summary of the linear regression model.
#' @examples
#' simple_lm(mtcars, mpg ~ wt)
#' @export
simple_lm <- function(data, formula, plot = TRUE) {
  # Fit the linear model
  lm_model <- lm(formula, data = data)

  # Generate the summary
  lm_summary <- summary(lm_model)

  # Generate the plots if requested (Scatter plot and residual plot)
  if (plot) {
    par(mfrow = c(1, 2)) # Set up the plotting area for 2 plots side-by-side, for better visuasazion

    # Scatter plot
    plot(data[[all.vars(formula)[2]]], data[[all.vars(formula)[1]]],
         main = "Scatter Plot with Regression Line",
         xlab = all.vars(formula)[2], ylab = all.vars(formula)[1])
    abline(lm_model, col = "red")

    # Residual plot
    plot(lm_model$fitted.values, lm_model$residuals,
         main = "Residual Plot",
         xlab = "Fitted Values", ylab = "Residuals")
    abline(h = 0, col = "red")

    par(mfrow = c(1, 1)) # Reset plotting area
  }

  return(lm_summary)
}
