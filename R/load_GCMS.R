#' Load GC-MS Data from CSV file output produced by MZMine
#'
#' This function loads GC-MS data from a CSV file produced by MZMine, in which
#' all retention times have been aligned. Each CSV files must be accompanied by
#' a Microsoft Excel spreadsheet containing the corresponding reference code and
#' variable information.
#'
#' @param inpath Path to the CSV file containing the GC-MS data
#' @param ref Path to the Microsoft Excel spreadsheet containing the sample
#' reference codes and variable information
#' @return A dataframe containing GC-MS data and variable information for each
#' sample in rows
#' @export
load_GCMS <- function(inpath, ref){
  # Reading GC-MS data
  raw_data <- read.csv(inpath)

  # Separating sample data
  sample_data <- raw_data[, -c(1:22)]

  # Extracting sample file names
  sample_names <- as.data.frame(matrix(ncol = 1, nrow = ncol(sample_data)/16))
  colnames(sample_names) <- 'Filename'
  for (i in 1:(ncol(sample_data)/16)){
    temp_name <- strsplit(strsplit(colnames(sample_data)[i*16], '_')[[1]][[2]], '\\.')[[1]][[1]]
    sample_names[i, ] <- temp_name
  }

  # Reading reference list
  ref_data <- as.data.frame(readxl::read_excel(ref))
  ref_data <- ref_data[, -2]
  ref_names <- as.data.frame(strsplit(ref_data[, 1], '_'))

  # Extracting variables for each sample
  variables <- as.data.frame(matrix(ncol = 6, nrow = nrow(sample_names)))
  colnames(variables) <- colnames(ref_data)

  for (i in 1:nrow(sample_names)){
    if (sample_names[i, 1] %in% ref_names[2, ]){
      variables[i, ] <- ref_data[which(ref_data[, 1] == paste('rwb_', sample_names[i, 1], sep = '')), ]
    }
  }

  # Extracting retention times
  rt_data <- as.data.frame(t(raw_data$rt))

  chrom_data <- as.data.frame(matrix(nrow = nrow(sample_names) + 1, ncol = ncol(rt_data)))

  chrom_data[1, ] <- rt_data[1, ]

  rep <- 0
  repeat{
    for (i in 1:(ncol(chrom_data) - 1)){
      if(chrom_data[1, i] == chrom_data[1, i + 1]){
        chrom_data[1, i] <- chrom_data[1, i] - 0.00001
      }
    }
    rep <- rep + 1
    if (rep == 3){
      break
    }
  }

  chrom_data <- chrom_data |>
    janitor::row_to_names(1)

  # Extracting peak areas for detected peaks
  for (i in 1:nrow(chrom_data)){
    for (x in 1:nrow(sample_data)){
      if (sample_data[x, i * 16 - 15] == 'DETECTED'){
        chrom_data[i, x] <- sample_data[x, i * 16 - 7]
      } else{
        chrom_data[i, x] <- 0
      }
    }
  }

  # Combining variables with data
  gcms_data <- cbind(variables, chrom_data)

  gcms_data
}
