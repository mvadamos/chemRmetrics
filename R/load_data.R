#' Load Raw Data from Excel file or Directory with JDX Files
#'
#' This function loads spectral data from an excel spreadsheet or a directory
#' with jdx files.It assumes that the first n columns of the excel files contain
#' non-numeric variables,while subsequent columns hold spectral x-axis values
#' (e.g. wavenumbers).All columns with numeric variables will be converted to
#' numeric values. The JDX files are assumed to have a filename pattern,
#' which is used to extract information about variables.
#'
#' @param inpath Path to the raw data input file or directory containing raw
#' jdx files
#' @param non_num The number of columns with non-numeric varibales at the
#' start of the Excel spreadsheet
#' @param ftype The file type the will be imported. Currently only 'xlsx'
#' and 'jdx' file types are supported
#' @param fstruc The filename structure for the jdx files with variables labelled
#' @param delim The delimiter used to separate descripters in the jdx filenames
#' @param skip The number of rows at the top of the text or csv file to be skipped when
#' reading in the data (default = 0)
#' @param yvar The column that contains the y-varaible in the text or csv file
#' (e.g. the column that contains the wavenumbers for infrared spectral data)
#' @param xvar The column that contains the x-varaible in the text or csv file
#' (e.g. the column that contains the absorbance/transmittance for infrared spectral data)
#' @param SOFC 'Stop on Failed Check' for 'readJDX' package (Please refer to 'read_JDX'
#' package reference manual before changeing this value)
#' @param shift_correct Corrects for instrument drift. Currently only corrects using PLA
#' spectra by setting argument to 'PLA'
#' @return A dataframe of the input data
#' @export
load_data <- function(inpath, non_num, ftype, fstruc, delim, skip = 0, yvar, xvar, SOFC = TRUE, shift_correct = FALSE){
  # Extracting variables from file name structure
  variables <- as.data.frame(strsplit(fstruc, delim))
  num_var <- nrow(variables)
  # Determining list of files from specified folder using 'inpath' argument
  file_list <- as.data.frame(list.files(path = inpath))
  file_list_path <- as.data.frame(list.files(path = inpath, full.names = TRUE))
  # Reading in first file and creating dataframe
  first_entry <- readJDX::readJDX(file = file_list_path[1, 1], SOFC = SOFC)
  raw_spectra <- as.data.frame(t(first_entry[[4]]))

  # Corrects for instrument drift
  if (shift_correct == 'PLA'){
    lower <- which.min(abs(raw_spectra[1, ] - 2925.6))
    upper <- which.min(abs(raw_spectra[1, ] - 2965.6))
    max <- raw_spectra[1, which.max(raw_spectra[2, lower:upper]) + lower]
    shift <- 2945.6 - max
    raw_spectra[1, ] <- raw_spectra[1, ] + shift
  }

  if (shift_correct == 'PETG'){
    lower <- which.min(abs(raw_spectra[1, ] - 1595.1))
    upper <- which.min(abs(raw_spectra[1, ] - 1635.1))
    max <- raw_spectra[1, which.max(raw_spectra[2, lower:upper]) + lower]
    shift <- 1615.1 - max
    raw_spectra[1, ] <- raw_spectra[1, ] + shift
  }

  if (shift_correct == 'ABS'){
    lower <- which.min(abs(raw_spectra[1, ] - 985.3))
    upper <- which.min(abs(raw_spectra[1, ] - 1025.3))
    max <- raw_spectra[1, which.max(raw_spectra[2, lower:upper]) + lower]
    shift <- 1005.3 - max
    raw_spectra[1, ] <- raw_spectra[1, ] + shift
  }

  raw_spectra <- raw_spectra[1, ]
  raw_spectra <- raw_spectra |>
    janitor::row_to_names(row_number = 1)

  # Adding data from all remaining files to dataframe
  for (i in 1:nrow(file_list_path)) {
    JDX_data <- readJDX::readJDX(file = file_list_path[i, 1], SOFC = SOFC)
    spec_data <- as.data.frame(t(JDX_data[[4]]))

    # Corrects for instrument drift
    if (shift_correct == 'PLA'){
      lower <- which.min(abs(spec_data[1, ] - 2925.6))
      upper <- which.min(abs(spec_data[1, ] - 2965.6))
      max <- spec_data[1, which.max(spec_data[2, lower:upper]) + (lower - 1)]
      shift <- 2945.6 - max
      spec_data[1, ] <- spec_data[1, ] + shift

      if (which(spec_data[1, ] == 2945.6) != which(as.data.frame(t(colnames(raw_spectra))) == 2945.6)){
        if (which(spec_data[1, ] == 2945.6) > which(as.data.frame(t(colnames(raw_spectra))) == 2945.6)){
          dif <- which(spec_data[1, ] == 2945.6) - which(as.data.frame(t(colnames(raw_spectra))) == 2945.6)
          spec_data <- spec_data[, -c(1:dif)]
          if (ncol(spec_data) > ncol(raw_spectra)){
            spec_data <- spec_data[, 1:ncol(raw_spectra)]
          }
          if (ncol(raw_spectra) > ncol(spec_data)){
            raw_spectra <- raw_spectra[, 1:ncol(spec_data)]
          }
        }
        if (which(spec_data[1, ] == 2945.6) < which(as.data.frame(t(colnames(raw_spectra))) == 2945.6)){
          dif <- which(as.data.frame(t(colnames(raw_spectra))) == 2945.6) - which(spec_data[1, ] == 2945.6)
          raw_spectra <- raw_spectra[, -c(1:dif)]
          if (ncol(spec_data) > ncol(raw_spectra)){
            spec_data <- spec_data[, 1:ncol(raw_spectra)]
          }
          if (ncol(raw_spectra) > ncol(spec_data)){
            raw_spectra <- raw_spectra[, 1:ncol(spec_data)]
          }
        }
      }
    }

    if (shift_correct == 'PETG'){
      lower <- which.min(abs(spec_data[1, ] - 1595.1))
      upper <- which.min(abs(spec_data[1, ] - 1635.1))
      max <- spec_data[1, which.max(spec_data[2, lower:upper]) + (lower - 1)]
      shift <- 1615.1 - max
      spec_data[1, ] <- spec_data[1, ] + shift

      if (which(spec_data[1, ] == 1615.1) != which(as.data.frame(t(colnames(raw_spectra))) == 1615.1)){
        if (which(spec_data[1, ] == 1615.1) > which(as.data.frame(t(colnames(raw_spectra))) == 1615.1)){
          dif <- which(spec_data[1, ] == 1615.1) - which(as.data.frame(t(colnames(raw_spectra))) == 1615.1)
          spec_data <- spec_data[, -c(1:dif)]
          if (ncol(spec_data) > ncol(raw_spectra)){
            spec_data <- spec_data[, 1:ncol(raw_spectra)]
          }
          if (ncol(raw_spectra) > ncol(spec_data)){
            raw_spectra <- raw_spectra[, 1:ncol(spec_data)]
          }
        }
        if (which(spec_data[1, ] == 1615.1) < which(as.data.frame(t(colnames(raw_spectra))) == 1615.1)){
          dif <- which(as.data.frame(t(colnames(raw_spectra))) == 1615.1) - which(spec_data[1, ] == 1615.1)
          raw_spectra <- raw_spectra[, -c(1:dif)]
          if (ncol(spec_data) > ncol(raw_spectra)){
            spec_data <- spec_data[, 1:ncol(raw_spectra)]
          }
          if (ncol(raw_spectra) > ncol(spec_data)){
            raw_spectra <- raw_spectra[, 1:ncol(spec_data)]
          }
        }
      }
    }

    if (shift_correct == 'ABS'){
      lower <- which.min(abs(spec_data[1, ] - 985.3))
      upper <- which.min(abs(spec_data[1, ] - 1025.3))
      max <- spec_data[1, which.max(spec_data[2, lower:upper]) + (lower - 1)]
      shift <- 1005.3 - max
      spec_data[1, ] <- spec_data[1, ] + shift

      if (which(spec_data[1, ] == 1005.3) != which(as.data.frame(t(colnames(raw_spectra))) == 1005.3)){
        if (which(spec_data[1, ] == 1005.3) > which(as.data.frame(t(colnames(raw_spectra))) == 1005.3)){
          dif <- which(spec_data[1, ] == 1005.3) - which(as.data.frame(t(colnames(raw_spectra))) == 1005.3)
          spec_data <- spec_data[, -c(1:dif)]
          if (ncol(spec_data) > ncol(raw_spectra)){
            spec_data <- spec_data[, 1:ncol(raw_spectra)]
          }
          if (ncol(raw_spectra) > ncol(spec_data)){
            raw_spectra <- raw_spectra[, 1:ncol(spec_data)]
          }
        }
        if (which(spec_data[1, ] == 1005.3) < which(as.data.frame(t(colnames(raw_spectra))) == 1005.3)){
          dif <- which(as.data.frame(t(colnames(raw_spectra))) == 1005.3) - which(spec_data[1, ] == 1005.3)
          raw_spectra <- raw_spectra[, -c(1:dif)]
          if (ncol(spec_data) > ncol(raw_spectra)){
            spec_data <- spec_data[, 1:ncol(raw_spectra)]
          }
          if (ncol(raw_spectra) > ncol(spec_data)){
            raw_spectra <- raw_spectra[, 1:ncol(spec_data)]
          }
        }
      }
    }

    if (ncol(spec_data) > ncol(raw_spectra)){
      spec_data <- spec_data[, 1:ncol(raw_spectra)]
    }

    if (ncol(raw_spectra) > ncol(spec_data)){
      raw_spectra <- raw_spectra[, 1:ncol(spec_data)]
    }

    colnames(spec_data) <- colnames(raw_spectra)
    spec_data <- spec_data [2, ]
    raw_spectra <- rbind(raw_spectra, spec_data)
  }

  # Adding varaible information from filename to dataframe
  var_db <- as.data.frame(t(variables))
  var_db <- var_db |>
    janitor::row_to_names(row_number = 1)

  for (i in 1:nrow(file_list)) {
    file_split <- strsplit(file_list[i, 1], delim)
    file_var <- as.data.frame(t(file_split[[1]]))
    colnames(file_var)[1:num_var] <- colnames(var_db)[1:num_var]
    var_db <- rbind(var_db, file_var[ , 1:num_var])
  }
  var_db <- cbind(file_list, var_db)
  raw_spectra <- cbind(var_db, raw_spectra)
  colnames(raw_spectra)[1] <- 'Filename'
  raw_spectra
}
