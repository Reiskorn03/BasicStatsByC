#' Perform Chi-Square Test with a optional Plot
#'
#' This function performs a chi-square test of independence for two categorical variables and optionally generates a mosaic plot for visualazation.
#'
#' @param data A data frame containing the dataset.
#' @param col1 The name of the first categorical column in the data frame.
#' @param col2 The name of the second categorical column in the data frame.
#' @param plot A logical value indicating whether to generate a mosaic plot (default is TRUE).
#' @return An object of class \code{htest} containing the results of the chi-square test.
#' @examples
#' chi_square_test(mtcars, "cyl", "gear")
#' @export
chi_square_test <- function(data, col1, col2, plot = TRUE) {
  # Extract the categorical columns
  cat1 <- data[[col1]]
  cat2 <- data[[col2]]

  # Create a contingency table
  contingency_table <- table(cat1, cat2)

  # Perform the chi-square test
  chi_result <- chisq.test(contingency_table)

  # Generate the plot if requested (default --> generates one plot)
  if (plot) {
    mosaicplot(contingency_table, main = paste("Mosaic Plot of", col1, "and", col2),
               xlab = col1, ylab = col2)
  }

  return(chi_result)
}
