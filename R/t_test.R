#' Perform T-Test with a optional integrated plot
#'
#' This function performs a t-test to compare means between two groups and optionally generates a boxplot to visualize the distribution.
#'
#' @param data A data frame containing the dataset.
#' @param group_col The name of the column in the data frame that defines the groups (factor or character).
#' @param value_col The name of the numeric column in the data frame for which the t-test is to be performed.
#' @param plot A logical value indicating whether to generate a boxplot (default is TRUE).
#' @return An object of class \code{htest} containing the results of the t-test.
#' @examples
#' t_test(mtcars, "am", "mpg")
#' @export
t_test <- function(data, group_col, value_col, plot = TRUE) {
  # Extract the groups and values of the given dataset
  groups <- data[[group_col]]
  values <- data[[value_col]]

  # Perform the t-test
  t_result <- t.test(values ~ groups)

  # Generate the boxplot if requested
  if (plot) {
    boxplot(values ~ groups, main = paste("Boxplot of", value_col, "by", group_col),
            xlab = group_col, ylab = value_col)
  }

  return(t_result)
}
