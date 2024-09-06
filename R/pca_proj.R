#' Project a New Dataset Onto an Existing Principal Component Analysis Model
#'
#' This function projets a new dataset onto a PCA model creating using a
#' separate dataset. The new data is distinguished using '_P' after each
#' variable entry for the non-numeric variables. The output of this function
#' is a dataframe containing the original and projection dataset and their
#' respetive PC scores. This combined dataframe should be used for any plotting
#' functions when wanting to compare the original data to the projected data.
#'
#' @param df The dataframe holding the original dataset, used to create the
#' PCA model
#' @param df_proj The dataframe holding the dataset to be projected onto the
#' existing PCA model
#' @param non_num The number of columns at the start of the datasets that holds
#' non-numeric variables
#' @param num_pca The number of principal components that the user would like
#' to consider
#' @return A dataframe holding the original dataset with the projection data
#' appended. This dataframe also holds the PC scores for both datasets
pca_proj <- function(df, df_proj, non_num, num_pcs){
  myPr <- prcomp(df[, -c(1:non_num)], rank. = num_pcs)
  data_pc <- cbind(df, myPr$x[,1:num_pcs])

  for (i in 1:non_num){
    df_proj[[colnames(df_proj)[i]]] <- paste(df_proj[[colnames(df_proj)[i]]], '_P', sep = '')
  }

  data_pc_proj <- scale(df_proj[, -c(1:non_num)], myPr$center, myPr$scale) %*% myPr$rotation
  data_pc_proj <- cbind(df_proj, data_pc_proj)
  data_pc_combined <- rbind(data_pc, data_pc_proj)
  return(data_pc_combined)
}
