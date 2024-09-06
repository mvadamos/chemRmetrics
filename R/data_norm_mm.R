#' Min-Max Normalise Values in Dataframe
#'
#' This function performs a min-max normlisation on a dataframe. A nested
#' function is utilised to define the min-max normalising.
#'
#' @param df Dataframe with variables to be min-max normalised
#' @param non_num The number of columns with non-numeric variables
#' at the start of the dataset
#' @return A dataframe with min-max normalised values
#' @export
data_norm_mm <- function(df, non_num){
  Variables <- df[, c(1:non_num)]
  norm <- function(x){
    (x-min(x))/(max(x)-min(x))
  }
  norm_scale <- t(apply(df[, -c(1:non_num)], 1, norm))
  Raw_Spectra <- cbind(Variables, norm_scale)
}
