#' Truncate Data Columns
#'
#' This function truncates the columns of a data frame. It looks for the
#' values closest to the upper and lower limits provided and removes the
#' columns from the dataframe between the values.
#'
#' @param df Dataframe to be truncated
#' @param non_num The number of non-numeric variables at the start of
#' the dataframe
#' @param upper The upper limit of the value range to be removed
#' @param lower The lower limit of the value range to be removed
#' @return A truncated version of the input dataframe
#' @export
data_trunc <- function(df, non_num, upper, lower){
  names <- as.data.frame(t(colnames(df)))
  colnames(names) <- colnames(df)
  first_num = non_num + 1
  for (i in first_num:ncol(names)){
    names[i] <- as.numeric(unlist((names[i])))
  }
  df <- rbind(names, df)
  close_upper <- max.col(-abs(upper - df[1, -c(1:non_num)])) + non_num
  close_lower <- max.col(-abs(lower - df[1, -c(1:non_num)])) + non_num
  Raw_Spectra <- df[, -c(close_lower:close_upper)]
  Raw_Spectra <- Raw_Spectra %>%
    janitor::row_to_names(row_number = 1)
  Raw_Spectra
}
