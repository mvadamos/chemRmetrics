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
#' @export
pca_proj <- function(df, df_proj, non_num, num_pcs){
  # Performing PCA using prcomp() function in base R
  myPr <- prcomp(df[, -c(1:non_num)], rank. = num_pcs)
  # Appending PC scores to dataset
  data_pc <- cbind(df, myPr$x[,1:num_pcs])

  # Projecting new dataset onto PCA model created using original dataset
  data_pc_proj <- scale(df_proj[, -c(1:non_num)], myPr$center, myPr$scale) %*% myPr$rotation

  # Labelling objects from original dataset as 'Original'
  original <- data.frame(c('Created'))
  colnames(original)[1] <- 'proj'
  for (i in 1:nrow(data_pc)){
    original[i, ] <- 'Original'
  }
  data_pc <- cbind(original, data_pc)

  # Labelling objects from projected dataset as 'Projected'
  projected <- data.frame(c('Created'))
  colnames(projected)[1] <- 'proj'
  for (i in 1:nrow(data_pc_proj)){
    projected[i, ] <- 'Projected'
  }
  data_pc_proj <- cbind(projected, data_pc_proj)

  # Combining original dataset (and PC scores) with projected dataset (and PC scores)
  data_pc_proj <- cbind(df_proj, data_pc_proj)
  data_pc_combined <- rbind(data_pc, data_pc_proj)

  # Extracting cumulative proportions from myPr
  myPr_sum <- summary(myPr)
  cumulative_prop <- as.data.frame(myPr_sum$importance)
  cumulative_prop <- cumulative_prop[3, 1:num_pcs]
  cumulative_prop$PC0 <- c(0)
  cumulative_prop <- cumulative_prop %>% relocate(PC0)
  cumulative_prop <- t(cumulative_prop)
  cumulative_prop_rns <- as.data.frame(row.names(cumulative_prop))
  scree_data <- cbind(cumulative_prop_rns, cumulative_prop)
  colnames(scree_data)[1] = 'Principal Component'

  # Appending the %varience explained by each PC to combined dataset
  scree_col <- ncol(data_pc_combined) + 1
  for (i in 2:nrow(scree_data)){
    data_pc_combined[i, scree_col] <- paste(round((scree_data[i, 2] - scree_data[i - 1, 2])*100, digits = 2), '%', sep = '')
  }
  data_pc_combined[1, ncol(data_pc_combined)] <- '0%'
  colnames(data_pc_combined)[ncol(data_pc_combined)] <- 'PC %'

  return(data_pc_combined)
}
