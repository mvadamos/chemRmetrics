#' Interpolate Variables in Dataframe
#'
#' This function interpolates the variables in a dataframe, reducing the
#' number of variables. The number of variables is reduced to a range
#' using the lower bound, upper bound, and step size selected.After the
#' data has been interpolated, the first row holding the variables is
#' converted to column names.
#'
#' @param df Dataframe to be interpolated
#' @param non_num Number of non-numeric variables in the first n columns of
#' the dataset
#' @param upper The upper bound of the interpolation range
#' @param lower The lower bound of the interpolation range
#' @param step The step size for the interpolation
#' @return A dataframe with interpolated variables
#' @export
data_interp <- function(df, non_num, upper, lower, step){
  # Creating a dataframe with the column names (x-axis values) of df as a rows
  names <- as.data.frame(t(colnames(df)))
  colnames(names) <- colnames(df)
  first_num = non_num + 1
  for (i in first_num:ncol(names)){
    names[i] <- as.numeric(unlist((names[i])))
  }
  df <- rbind(names, df)
  Variables <- df[, c(1:non_num)]
  df_Num <- df[, -c(1:non_num)]
  df_Num_t <- as.data.frame(t(df_Num))
  # Interpolating x-axis values
  Bins = aggregate(df_Num_t,
                   by = list(cut(df_Num_t[, 1], seq(lower, upper, step))),
                   mean)
  Bins <- Bins[, -1]
  # Recreating original dataframe with interpolated x-axis values
  Raw_Spectra <- as.data.frame(t(Bins))
  Raw_Spectra <- cbind(Variables, Raw_Spectra)
  Raw_Spectra <- Raw_Spectra |>
    janitor::row_to_names(row_number = 1)
  Raw_Spectra
}
