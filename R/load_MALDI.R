#' Load MALDI-MS Data using the MALDIquantForeign and MALDIquant Packages
#'
#' This function utilises the MALDIquantForeign and MALDIquant packages to import
#' MALDI-MS data in the '.MzMl' format. A baseline correction is applied to the data
#' using the 'SNIP' method. The spectra are filtered to ensure a consistent desired
#' number of chanels to allow for chemometric analysis. The 'target ID' for each
#' sample is extracted from the MzMl file. A separate Microsoft Excel spreadsheet
#' is imported containing the varaible information and 'target ID' for each sample.
#' The 'target ID' is matched to associate variable information in the Microsoft
#' Excel spreadsheet with data from the MzMl file. Varaible information and mass
#' spectra are then organised and exported as a dataframe.
#'
#' @param inpath Path to the MzMl file holding the MALDI-MS data
#' @param ref Path to Microsoft Excel spreadsheet holding variable data and 'target ID'
#' for each sample (all information stored in a row for each sample)
#' @param nmass The number of m/z chanels that the mass spectrum should contain (used to
#' filter spectra of interest)
#' @param trunc Used to designate if the user would like to truncate the dataset (default = 'FALSE')
#' @param lower Minimum mass for spectra
#' @param upper Maximum mass for spectra
#' @return Dataframe containing mass spectra and variable information for each sample
#' @export
load_MALDI <- function(inpath, ref, nmass, trunc = FALSE, lower, upper){
  # Load MzMl File
  raw_data <- MALDIquantForeign::importMzMl(inpath)

  # Truncate Spectra
  if (trunc == TRUE){
    raw_data_blc <- MALDIquant::trim(raw_data, lower, upper)
  }

  # Perform Baseline Correction
  raw_data_blc <- MALDIquant::removeBaseline(raw_data_blc, method = 'SNIP')

  # Perform TIC Normalisation
  raw_data_blc <- MALDIquant::calibrateIntensity(raw_data_blc, method = 'TIC')

  # Extract Mass and Intensity Data
  mass_list <- as.data.frame(t(MALDIquant::mass(raw_data_blc[[1]]))) |>
    janitor::row_to_names(1)

  sample_num_list <- as.data.frame(matrix(ncol = 1, nrow = length(raw_data_blc)))

  x = 1

  for (i in 1:length(raw_data_blc)){
    temp <- as.data.frame(t(MALDIquant::intensity(raw_data_blc[[i]])))
    if (length(colnames(temp)) == nmass){ #nmass typically set to 573440
      colnames(temp) <- colnames(mass_list)
      mass_list <- rbind(mass_list, temp)
      sample_num_list[x, 1] <- i
      x = x + 1
    }
  }

  # Extract Sample Information
  first_cols <- as.data.frame(matrix(ncol = 6, nrow = nrow(mass_list)))

  sample_ref <- readxl::read_excel(ref, col_names = TRUE)

  for (i in 1:nrow(mass_list)){
    fn <- strsplit(MALDIquant::metaData(raw_data_blc[[sample_num_list[i, 1]]])$id, '_')[[1]]

    target_id <- fn[length(fn) - 6]

    for (x in 1:nrow(sample_ref)){
      if (grepl(paste(paste('^', target_id, sep = ''), '$', sep  = ''), sample_ref[x, 1]) == TRUE){
        first_cols[i, ] <- sample_ref[x, ]
      }
    }
  }

  colnames(first_cols) <- colnames(sample_ref)

  mass_list <- cbind(first_cols, mass_list)

  mass_list
}
