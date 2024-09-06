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
#' @return A dataframe of the input data
#' @export
load_data <- function(inpath, non_num, ftype, fstruc, delim){
  if (grepl('xlsx', ftype, ignore.case = T)){
    Raw_Spectra <- readxl::read_excel(inpath)
    first_num <- non_num + 1
    for (i in first_num:ncol(Raw_Spectra)){
      Raw_Spectra[i] <- as.numeric(unlist((Raw_Spectra[i])))
    }
    Raw_Spectra
  } else if(grepl('jdx', ftype, ignore.case = T)){

    variables <- as.data.frame(strsplit(fstruc, delim))
    num_var <- nrow(variables)
    file_list <- as.data.frame(list.files(path = inpath))
    file_list_path <- as.data.frame(list.files(path = inpath, full.names = TRUE))
    first_entry <- readJDX::readJDX(file = file_list_path[1, 1])
    raw_spectra <- as.data.frame(t(first_entry[[4]]))
    raw_spectra <- raw_spectra[1, ]
    raw_spectra <- raw_spectra %>%
      janitor::row_to_names(row_number = 1)

    for (i in 1:nrow(file_list_path)) {
      JDX_data <- readJDX::readJDX(file = file_list_path[i, 1])
      spec_data <- as.data.frame(t(JDX_data[[4]]))
      colnames(spec_data) <- colnames(raw_spectra)
      spec_data <- spec_data [2, ]
      raw_spectra <- rbind(raw_spectra, spec_data)
    }
    var_db <- as.data.frame(t(variables))
    var_db <- var_db %>%
      janitor::row_to_names(row_number = 1)

    for (i in 1:nrow(file_list)) {
      file_split <- strsplit(file_list[i, 1], '_')
      file_var <- as.data.frame(t(file_split[[1]]))
      colnames(file_var)[1:num_var] <- colnames(var_db)[1:num_var]
      var_db <- rbind(var_db, file_var[ , 1:num_var])
    }
    var_db <- cbind(file_list, var_db)
    raw_spectra <- cbind(var_db, raw_spectra)
    colnames(raw_spectra)[1] <- 'Filename'
    raw_spectra
  } else{
    print(paste(paste("Error: File type '", ftype, sep = ''), "' is not supported", sep = ''))
  }
}
