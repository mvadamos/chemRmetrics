#' Perform PCA for MALDI-MS Data Producing Kendrick Loading Plots
#'
#' This function performs Principal Component Analysis as is performed by
#' the 'pca()' function, however, loadings plots are output as Kendrick plots.
#' Kendrick plots are produced for both the positive and negative loadings
#' separately for each PC to aid comparison to Kendrick plots of the samples. The Kendrick
#' plots are produced using the 'plot_Kendrick()' function.
#'
#' @param df The input dataframe
#' @param num_pcs The number of principal components that the user would like
#' to consider
#' @param unit_mass Mass of repeating unit for Kendrick plots
#' @param unit_name Name of repeating unit for Kendrick plots
#' @param SNR Minimum signal-to-noise ratio used function when performing peak-picking.
#' If peak_pick = 'manual', 'SNR' refers to minimum normalised value (values are normalised
#' to unity then multiplied by 'scale')
#' @param scale Value used to adjust the size of data points in Kendrick plot (scale will also
#' affect the SNR value when peak_pick = 'manual')
#' @param trunc Used to designate if the user would like to reduce the mass range of
#' the spectra (Default = FALSE)
#' @param max_mass The maximum mass desired for the mass spectra which will be used to
#' truncate the mass range if 'trunc' = TRUE
#' @param method The peak picking method used for the 'detectPeaks()' function in the
#' MALDIquant package (see MALDIquant package guide for further details)
#' @param halfWindowSize The halfwindow size used during the peak picking (see MALDIquant
#' package guide for further details)
#' @param peak_pick Can be set to 'auto' or 'manual'. When setting to 'auto' peak picking
#' is performed using the 'detectPeaks()' function within the MALDIquant package. Use 'manual'
#' if m/z value are nominal, all masses with a normalised intensity values greater than 'SNR'
#' will be included as a peak
#' @param ymin Minimum y-value for KMD and KMR PDF plots
#' @param ymax Maximum y-value for KMD and KMR PDF plots
#' @return An Excel spreadsheet with loadings data
#' @return An Excel spreadsheet with scree data
#' @return A PDF file with the scree plot
#' @return A folder labelled 'KMD Plots' in which all Kendrick mass defect plots will be output
#' @return A folder labelled 'KMR Plots' in which all Kendrick mass remainder plots will be output
#' @return A kendrick mass defect plot as a '.pdf' file for each PC loading up to 'num_pcs'
#' @return A kendrick mass remainder plot as a '.pdf' file for each PC loading up to 'num_pcs'
#' @return A kendrick mass defect plot as a '.html' file for each PC loading up to 'num_pcs'
#' @return A kendrick mass remainder plot as a '.html' file for each PC loading up to 'num_pcs'
#' @return All data for each kendrick mass defect plot in '.csv' format
#' @return All data for each kendrick mass remainder plot in '.csv' format
#' @export
Kendrick_pca <- function(df, num_pcs, unit_mass, unit_name, SNR, scale, trunc = 'FALSE', max_mass, method = 'MAD', halfWindowSize = '10', peak_pick = 'auto', ymin, ymax){
  # Determine Number of Non-Numeric Columns as the Start of a Data Frame
  suppressWarnings(
    for (i in 1:ncol(df)){
      if (is.na(as.numeric(colnames(df)[i])) != TRUE){
        non_num <- i - 1
        break
      }
    })

  # Truncating Data
  if (trunc == TRUE){
    step <- as.numeric(colnames(df)[non_num + 2]) - as.numeric(colnames(df)[non_num + 1])

    gap <- max_mass - as.numeric(colnames(df)[non_num + 1])

    num_steps = round(gap/step)

    df <- df[, -c(num_steps:ncol(df))]
  }

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
  loadings_raw <- as.data.frame(t(myPr$rotation))
  loadings <- as.data.frame(matrix(ncol = ncol(loadings_raw), nrow = nrow(loadings_raw) * 2))
  colnames(loadings) <- colnames(loadings_raw)
  for (i in 1:nrow(loadings)){
    if (i %% 2 == 0){
      for (x in 1:ncol(loadings)){
        if (loadings_raw[i/2, x] < 0){
          loadings[i, x] <- abs(loadings_raw[i/2, x])
        } else{
          loadings[i, x] <- 0
        }
      }
    } else{
      for (x in 1:ncol(loadings)){
        if (loadings_raw[i/2 + 0.5, x] > 0){
          loadings[i, x] <- loadings_raw[i/2 + 0.5, x]
        } else{
          loadings[i, x] <- 0
        }
      }
    }
  }
  loadings_ID <- as.data.frame(matrix(nrow = nrow(loadings), ncol = 1))
  colnames(loadings_ID)[1] <- 'Sample ID'
  for (i in 1:(2 * num_pcs)){
    if (i %% 2 == 0){
      loadings_ID[i, 1] <- paste(paste(' - PC', i/2, sep = ''), '(-ve ) Loadings', sep = ' ')
    } else{
      loadings_ID[i, 1] <- paste(paste(' - PC', i/2 + 0.5, sep = ''), '(+ve) Loadings', sep = ' ')
    }
  }
  loadings <- cbind(loadings_ID, loadings)

  # Creating a Kendrick plot for each loadings plot up to the number of PCs specified with the num_pcs argumnet
  # then exporting the plots as PDFs
  chemRmetrics::plot_Kendrick(loadings, unit_mass, unit_name, SNR, scale, trunc, max_mass, method, halfWindowSize, peak_pick, ymin, ymax)

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
