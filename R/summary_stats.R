#' Summary Statistics
#'
#' This function gives back summary statistics (mean, median, standard deviation, Min and Max) for numeric columns in a dataset.
#'
#' @param data A data frame.
#' @return A data frame of summary statistics for each numeric column.
#' @examples
#' summary_stats(mtcars)
#' @export
summary_stats <- function(data) {
  # Identify numeric columns, this function only supports numerical data
  numeric_cols <- sapply(data, is.numeric)
  
  # summary statistics for each numeric column with mean (Mittelwert), median (Medianwert), standard deviation (Standardabweichung), min and max value
  stats <- data.frame(
    Mean = sapply(data[, numeric_cols], mean, na.rm = TRUE),
    Median = sapply(data[, numeric_cols], median, na.rm = TRUE),
    StdDev = sapply(data[, numeric_cols], sd, na.rm = TRUE),
    Min = sapply(data[, numeric_cols], min, na.rm = TRUE),
    Max = sapply(data[, numeric_cols], max, na.rm = TRUE)
  )
  
  # Set row names as the names of the numeric columns in the original data
  rownames(stats) <- names(data)[numeric_cols]
  
  return(stats)
}