#' Load DSC Data from Directory with CSV Files
#'
#' This function loads thermal data from an a directory with csv files exported
#' using the TRIOS software. The CSV files are assumed to have a filename pattern,
#' which is used to extract information about variables.
#'
#' @param inpath Path to the directory containing csv files
#' @param upper The maximum temperature to be included when importing thermal data
#' @param lower The minimum temperature to be included when importing thermal data
#' @param fstruc The filename structure for the csv files with variables labelled
#' @param delim The delimiter used to separate descriptors in the csv filenames
#' @param stepsize The desired stepsize between temperature values
#' @return A dataframe containing all data from the input files
#' @export
load_dsc <- function(inpath, upper, lower, fstruc, delim, stepsize){
  # Loading file path
  file_list_path <- as.data.frame(list.files(path = inpath, full.names = TRUE))
  file_list <- as.data.frame(list.files(path = inpath))

  # Creating output dataframe
  num_temp = (upper - lower)*(1/stepsize)
  raw_data <- as.data.frame(matrix(nrow = nrow(file_list_path), ncol = num_temp))

  # Extracting and interpolating dsc data from csv files
  for (i in 1:nrow(file_list_path)){
    file <- read.csv(file_list_path[i, ], skip = 9)

    file_data <- as.data.frame(file$X.C)
    file_data[, 2] <- as.data.frame(file$W.g)
    colnames(file_data)[1] <- 'Temperature'
    colnames(file_data)[2] <- 'Heat Flow'

    Bins = aggregate(file_data,
                     by = list(cut(file_data[, 1], seq(lower, upper, stepsize))),
                     mean)
    Bins <- Bins[, -1]

    Bins_round <- Bins |> dplyr::mutate_at(dplyr::across(c(Temperature)), dplyr::across(c(funs(round(., 2)))))

    data_processed <- Bins_round[1:num_temp, ]

    data_processed_t <- as.data.frame(t(data_processed))

    raw_data[i, ] <- data_processed_t[2, ]
  }

  # Adding interpolated temperature data to output dataframe
  colnames(raw_data) <- colnames(data_processed_t)
  raw_data <- rbind(data_processed_t[1, ], raw_data)

  # Correcting duplicate interpolated groups (smoothing)
  try(for (i in 1:ncol(raw_data)){
    if (raw_data[1, i+1] == raw_data[1, i]){
      raw_data[1, i+1] <- raw_data[1, i+1] + stepsize
    }
  })

  # Correcting for 0.5 C temperature value shift during interpolation - Not always necessary
  #for (i in 1:ncol(raw_data)){
  #  raw_data[1, i] <- raw_data[1, i] - 0.5
  #}

  # Adding temperature values to column names
  for (i in 1:ncol(raw_data)){
    colnames(raw_data)[i] <- raw_data[1, i]
  }
  raw_data <- raw_data[-1, ]

  # Creating a variables dataframe using user-input file structure as column names
  variables <- as.data.frame(strsplit(fstruc, delim))
  num_var <- nrow(variables)
  var_db <- as.data.frame(t(variables))
  var_db <- var_db |>
    janitor::row_to_names(row_number = 1)

  # Adding varaible information from file names to variable dataframe and combining
  # with output dataframe
  for (i in 1:nrow(file_list)) {
    file_split <- strsplit(file_list[i, 1], delim)
    file_var <- as.data.frame(t(file_split[[1]]))
    colnames(file_var)[1:num_var] <- colnames(var_db)[1:num_var]
    var_db <- rbind(var_db, file_var[ , 1:num_var])
  }
  var_db <- cbind(file_list, var_db)
  raw_data <- cbind(var_db, raw_data)
  colnames(raw_data)[1] <- 'Filename'

  # Ensures all numeric values are numeric
  first_num <- ncol(var_db) + 1
  for (i in first_num:ncol(raw_data)){
    raw_data[i] <- as.numeric(unlist((raw_data[i])))
  }

  # Checking for empty columns
  repeat {
    if (any(is.na(raw_data[, ncol(raw_data)])) == TRUE){
      raw_data <- raw_data[, -ncol(raw_data)]
    } else{
      break
    }
  }

  # Returning output dataframe
  raw_data
}
