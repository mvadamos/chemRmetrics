#' Perform Principal Component Analysis and Produce Scree and Loadings Data
#'
#' This function performs principal component analysis on data in a dataframe
#' using the 'prcomp()' function. Loadings data and plots are exported for all
#' principal components up to the number of principal components listed in the
#' 'num_pcs' parameter. The scree data and a scree plot are also exported.
#' The PC scores are appended to the original dataframe which is then returned
#' as a new dataframe.
#'
#' @param df The input dataframe
#' @param non_num The number of columns with non-numeric variables at the start
#' of the dataframe
#' @param num_pcs The number of principal components that the user would like
#' to consider
#' @return An Excel spreadsheet with loadings data
#' @return An Excel spreadsheet with scree data
#' @return PDF files of the loadings plot up to the number of PCs selected
#' using the 'num_pcs' parameter
#' @return A PDF file with the scree plot
#' @export
pca <- function(df, non_num, num_pcs){
  # Performing PCA using prcomp() function in base R
  myPr <- prcomp(df[, -c(1:non_num)], rank. = num_pcs)
  # Appending PC scores to dataset
  data_pc <- cbind(df, myPr$x[,1:num_pcs])

  # Extracting cumulative proportions from myPr
  myPr_sum <- summary(myPr)
  cumulative_prop <- as.data.frame(myPr_sum$importance)
  cumulative_prop <- cumulative_prop[3, 1:num_pcs]
  cumulative_prop$PC0 <- c(0)
  cumulative_prop <- cumulative_prop |> dplyr::relocate(PC0)
  cumulative_prop <- t(cumulative_prop)

  # Creating scree plot and exporting it as a PDF
  pdf('Scree Plot.pdf', width = 1000, height = 600, paper = 'USr')

  plot(cumulative_prop, type = "b", col = "black", pch = 19, xlab = "Principal Component", ylab = "Cumulative Proportion", ylim = c(0, 1), main = 'Scree Plot', xaxt = 'n', lwd = 2) +
    axis(1, at = 1:(num_pcs + 1), labels = row.names(cumulative_prop)) +
    text(cumulative_prop, labels = cumulative_prop[1:(num_pcs + 1)], cex = 0.7, pos = 3)

  dev.off()

  # Exporting cumulative proportions as an Excel file to edit with other software
  cumulative_prop_rns <- as.data.frame(row.names(cumulative_prop))
  scree_data <- cbind(cumulative_prop_rns, cumulative_prop)
  colnames(scree_data)[1] = 'Principal Component'
  writexl::write_xlsx(scree_data, 'Scree Data.xlsx')

  # Extracting loadings data from myPr
  for (i in 1:num_pcs){
    loadings <- as.data.frame(myPr$rotation)
    Wavenumbers <- rownames(loadings)
    loadings <- cbind(loadings, Wavenumbers)

    # Creating a loadings plots up to the number of PCs specified with the num_pcs argumnet
    # then exporting the plots as PDFs
    pdf(paste(paste(paste('PC', i, sep = ''), 'Loadings', sep = ' '), '.pdf', sep = ''), width = 1000, height = 600, paper = 'USr')

    plot(loadings$Wavenumbers, loadings[, i], xlim = rev(c(400, 4000)), type = 'l', xlab = expression("Wavenumber (cm"^-1*")"), ylab = "Loadings", main = paste(paste('PC', i, sep = ''), 'Loadings', sep = ' '), yaxs = "i", xaxs = "i", lwd = 2) +
      abline(h = 0, col = "blue")

    dev.off()
  }
  # Exporting the loadings data as an Excel file to edit with other software
  writexl::write_xlsx(loadings, 'Loadings_Data.xlsx')

  # Appending the %varience explained by each PC to dataset
  scree_col <- ncol(data_pc) + 1
  for (i in 2:nrow(scree_data)){
    data_pc[i, scree_col] <- paste(round((scree_data[i, 2] - scree_data[i - 1, 2])*100, digits = 2), '%', sep = '')
  }
  data_pc[1, ncol(data_pc)] <- '0%'
  colnames(data_pc)[ncol(data_pc)] <- 'PC %'
  return(data_pc)
}
