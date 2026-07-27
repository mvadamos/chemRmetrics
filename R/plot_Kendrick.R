#' Create Kendrick Mass Defect and Kendrick Mass Remainder Plots for MALDI-MS Mass Spectra
#'
#' This function creates Kednrick mass defect and Kendrick mass remainder plots for
#' MALDI-MS mass spectra stored in a dataframe. This function assumes that the first
#' 'n' columns of the dataframe contain non-numeric variables with the remainder
#' containing mass spectra data with each row corresponding to a different sample.
#' Data imported using the 'load_MALDI()' function will already be in the correct format.
#' This function creates two folders in the working directory in which the Kendrick plots
#' will be exported.
#'
#' @param df Dataframe containing MALDI-MS mass spectra data
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
#' @param xmin Minimum x-value for KMD and KMR PDF plots
#' @param xmax Maximum x-value for KMD and KMR PDF plots
#' @return A folder labelled 'KMD Plots' in which all Kendrick mass defect plots will be output
#' @return A folder labelled 'KMR Plots' in which all Kendrick mass remainder plots will be output
#' @return A kendrick mass defect plot as a '.pdf' file for each mass spectrum in 'df'
#' @return A kendrick mass remainder plot as a '.pdf' file for each mass spectrum in 'df'
#' @return A kendrick mass defect plot as a '.html' file for each mass spectrum in 'df'
#' @return A kendrick mass remainder plot as a '.html' file for each mass spectrum in 'df'
#' @return All data for each kendrick mass defect plot in '.csv' format
#' @return All data for each kendrick mass remainder plot in '.csv' format
#' @export
plot_Kendrick <- function(df, unit_mass, unit_name, SNR, scale, trunc = FALSE, max_mass, method = 'MAD', halfWindowSize = 10, peak_pick = 'auto', ymin, ymax, xmin, xmax){
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

  # Calculate Unit mass Ratio

  unit_ratio <-round(unit_mass)/unit_mass

  # Generate Peak Lists and Kendrick Plots
  for (i in 1:nrow(df)){
    if (peak_pick == 'auto'){
      spec <- MALDIquant::createMassSpectrum(as.numeric(colnames(df[, -c(1:non_num)])), as.numeric(df[i, -c(1:non_num)]))
      peaks <- MALDIquant::detectPeaks(spec, halfWindowSize = halfWindowSize, method = method, SNR = SNR)
      peaks_mass <- as.data.frame(MALDIquant::mass(peaks))

      peaks_intensity <- as.data.frame(matrix(ncol = 2, nrow = nrow(peaks_mass)))

      peaks_intensity[, 1] <- as.data.frame(MALDIquant::intensity(peaks))
    } else if (peak_pick == 'manual'){
      df_peaks <- as.data.frame(matrix(nrow = nrow(df), ncol = 1))
      w = 2
      for (p in 1:ncol(df[, -c(1:non_num)])){
        if (df[i, p + non_num] > SNR){
          df_peaks <- cbind(df_peaks, df[i, p + non_num])
          colnames(df_peaks)[w] <- colnames(df)[p + non_num]
          w = w + 1
        }
      }
      df_peaks <- df_peaks[, -1]
      peaks_mass <- as.data.frame(as.numeric(colnames(df_peaks)))
      peaks_intensity <- as.data.frame(t(df_peaks))
    }

    for (z in 1:nrow(peaks_intensity)){
      peaks_intensity[z, 2] <- ((peaks_intensity[z, 1] - min(peaks_intensity[, 1]))/(max(peaks_intensity[, 1]) - min(peaks_intensity[, 1]))) * scale
    }

    peaks_KM <- as.data.frame(matrix(ncol = 1, nrow = nrow(peaks_mass)))

    peaks_KMD <- as.data.frame(matrix(ncol = 2, nrow = nrow(peaks_mass)))

    peaks_KMR <- as.data.frame(matrix(ncol = 1, nrow = nrow(peaks_mass)))

    for (x in 1:nrow(peaks_mass)){
      peaks_KM[x, 1] <- peaks_mass[x, 1] * unit_ratio
    }

    for (y in 1:nrow(peaks_KM)){
      peaks_KMD[y, 1] <- floor(peaks_KM[y, 1])
      peaks_KMD[y, 2] <- peaks_KM[y, 1] - round(peaks_KM[y, 1])
    }

    for (r in 1:nrow(peaks_KMD)){
      peaks_KMR[r, 1] <- peaks_KMD[r, 1] %% round(unit_mass)
    }

    # Kendrick Plots
    dir.create('KMD Plots')
    pdf(paste('KMD Plots/', paste(unit_name, paste(paste('Sample #', df$`Sample ID`[i], sep = ''), paste(paste('KMD Plot_', rownames(df)[i], sep = ''), '.pdf', sep = ''), sep = ' '), sep = '_'), sep = ''))
    plot(peaks_KMD[, 1], peaks_KMD[, 2], pch = 19, cex = peaks_intensity[, 2], xaxs = 'i', yaxs = 'i', main = paste('Repeating Unit:', unit_name, sep = ' '), xlab = 'Nominal Kendrick Mass', ylab = 'Kendrick Mass Defect', xlim = c(xmin, xmax), ylim = c(ymin, ymax))
    dev.off()

    dir.create('KMR Plots')
    pdf(paste('KMR Plots/', paste(unit_name, paste(paste('Sample #', df$`Sample ID`[i], sep = ''), paste(paste('KMR Plot_', rownames(df)[i], sep = ''), '.pdf', sep = ''), sep = ' '), sep = '_'), sep = ''))
    plot(peaks_mass[x, 1], peaks_KMR[, 1], pch = 19, cex = peaks_intensity[, 2], xaxs = 'i', yaxs = 'i', main = paste('Repeating Unit:', unit_name, sep = ' '), xlab = 'm/z', ylab = 'Kendrick Mass Remainder', xlim = c(xmin, xmax), ylim = c(ymin, ymax))
    dev.off()

    fig_2D_KMD <- plotly::plot_ly(df,
                                  type = 'scatter',
                                  mode = 'markers') |>
      plotly::add_markers(x = peaks_KMD[, 1],
                          y = peaks_KMD[, 2],
                          marker = list(size = peaks_intensity[, 2]*5))

    fig_2D_KMD <- fig_2D_KMD |>
      plotly::layout(
        title = paste('Repeating Unit:', unit_name, sep = ' '),
        scene = list(bgcolor = "#FFFFFF"),
        xaxis = list(zeroline = F,showline = T, showgrid = FALSE, ticks = 'outside', title = 'Nominal Kendrick Mass'),
        yaxis = list(zeroline = F, showline = T, showgrid = FALSE, ticks = 'outside', title = 'Kendrick Mass Defect'))

    fig_2D_KMD

    htmlwidgets::saveWidget(fig_2D_KMD, file = paste('KMD Plots/', paste(unit_name, paste(paste('Sample #', df$`Sample ID`[i], sep = ''), paste(paste('KMD Plot_', rownames(df)[i], sep = ''), '.html', sep = ''), sep = ' '), sep = '_'), sep = ''))

    fig_2D_KMR <- plotly::plot_ly(df,
                                  type = 'scatter',
                                  mode = 'markers') |>
      plotly::add_markers(x = peaks_mass[x, 1],
                          y = peaks_KMR[, 1],
                          marker = list(size = peaks_intensity[, 2]*5))

    fig_2D_KMR <- fig_2D_KMR |>
      plotly::layout(
        title = paste('Repeating Unit:', unit_name, sep = ' '),
        scene = list(bgcolor = "#FFFFFF"),
        xaxis = list(zeroline = F,showline = T, showgrid = FALSE, ticks = 'outside', title = 'Nominal Kendrick Mass'),
        yaxis = list(zeroline = F, showline = T, showgrid = FALSE, ticks = 'outside', title = 'Kendrick Mass Remainder'))

    fig_2D_KMR

    htmlwidgets::saveWidget(fig_2D_KMR, file = paste('KMR Plots/', paste(unit_name, paste(paste('Sample #', df$`Sample ID`[i], sep = ''), paste(paste('KMR Plot_', rownames(df)[i], sep = ''), '.html', sep = ''), sep = ' '), sep = '_'), sep = ''))

    # Export KMR Data '.csv'
    KMR_data <- cbind(peaks_KMD[r, 1], peaks_KMR)
    KMR_data <- cbind(KMR_data, peaks_intensity[, 2]/2)

    write.csv(KMR_data, paste(paste(paste('KMR Plots/KMR Data Sample #_', df$`Sample ID`[i], sep = ''), rownames(df)[i], sep = ''),  '.csv', sep = ''))
  }
}
