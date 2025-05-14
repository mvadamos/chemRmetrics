#' Overlay Principal Component Analysis Loadings Plots
#'
#' This function overlays two loadings plots using the loadings data in
#' the excel output file from the 'pca' function within the 'UnscramblR'
#' package. The overlain laodings plots will be exported as a PDF document.
#'
#' @param infile The path for the input file holding the loadings data
#' @param pc_num_1 The first PC's loadings to be overlain
#' @param pc_num_2 The second PC's loadings to be overlain
#' @return A PDF file with a figure of the overlain loadings plots
#' @export
loadings_olay <- function(infile, pc_num_1, pc_num_2){
  # Reads in loadings data xlsx file (created using pca() function)
  loadings <- readxl::read_excel(infile)
  loadings <- as.data.frame(loadings)

  # Creating a plot with the loadings for two specified PCs (pc_num_1 and pc_num_2) overlain,
  # then exporting the overlain plot as a PDF
  pdf(paste(paste(paste(paste('PC', pc_num_1, sep = ''), 'and PC', sep = ''), pc_num_2, sep = ' '), 'Loadings Overlay', sep = ' '), width = 1000, height = 600, paper = 'USr')

  plot(loadings$Wavenumbers, loadings[, pc_num_1], xlim = rev(c(400, 4000)), ylim = range(loadings[, pc_num_1], loadings[, pc_num_2]), type = 'l', xlab = expression("Wavenumber (cm"^-1*")"), ylab = "Loadings", main = paste(paste(paste(paste('PC', pc_num_1, sep = ''), 'and PC', sep = ''), pc_num_2, sep = ' '), 'Loadings Overlay', sep = ' '), yaxs = "i", xaxs = "i", lwd = 2) +
    abline(h = 0, col = "blue") +
    lines(loadings$Wavenumbers, loadings[, pc_num_2], col = 'red', lwd = 2) +
    as.numeric(unlist(legend('topright', c(paste('PC', pc_num_1, sep = ''), paste('PC', pc_num_2, sep = '')), col = c('black', 'red'), pch = 19)))

  dev.off()
}
