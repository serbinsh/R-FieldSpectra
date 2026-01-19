#--------------------------------------------------------------------------------------------------------------#
##' Function to read .sig files and extract spectral data
##'
##'This function will read ".sig" files and extract the speczral data. The resulting data frame will
##'have the option for all data as well as selected data , which can be obtained by providing the
##'boolean parameter.
##'
##' @name read.sig
##' @title read .sig files from SVC
##'
##' @param path A character string specifying the path to the directory or a single sig file.
##' @param default A logical value indicating whether to include all columns from the `.sig` files
#'            (`TRUE`) or only selected columns (`FALSE`). Default is `TRUE`.
#'
##'
##' @return A data data frame with the following columns :
##' \describe{
##' \item{Wavelengths(nm)}{Numeric vector representing the wavelengths in nanometers.}
##' \item{file}{gives information on the file name}
##' \item{Reflectance(\%)}{Numeric vector of reflectance percentages.}
##' \item{Target Values (optional)}{Provides the targer values}
##' \item{Reference Values (optional)}{Provides information on the reference values}
##' }
##'
##' @details
##' The function can read a single file or multiple files in a directory.
##' The file is then read line by line and the spectral data is extracted from the line after "data=".
##' The extracted data is then converted into a data frame with appropriate names.
##'
##'@examples
##'\dontrun{
##'path <- "/path/to/your/file.sig"
##'data <- read.sig(path, default = FALSE)
##'print(head(data))
##'}
##'
##'@export
##'
##'@author Methun George, Steffen Neumann
##'





read.sig <- function(path, default = TRUE) {
  # Check if the path exists
  if (!file.exists(path)) {
    stop("The path does not exist: ", path)
  }

  # Check if the path is a directory or a single file
  path_info <- file.info(path)
  if (path_info$isdir) {
    sig_files <- list.files(path = path, pattern = "\\.sig$", full.names = TRUE)
  } else {
    sig_files <- path
  }

  # An empty list to store the data frames
  all_data <- list()

  # Read each file and then combine the data
  for (file in sig_files) {
    file_content <- readLines(file)
    data_start <- grep("^data=", file_content)
    data_lines <- file_content[(data_start + 1):length(file_content)]
    data <- read.table(text = data_lines, header = FALSE, fill = TRUE)

    # Add the filename
    name_line <- grep("^name=", file_content, value = TRUE)
    if (length(name_line) > 0) {
      file_name <- sub("^name=", "", name_line)
    } else {
      file_name <- basename(file)  # Default to file name if not found in metadata
    }

    if (default) {
      # If parameter,'all' is TRUE, include all columns with original column names
      data$file_name <- file_name
      if (ncol(data) >= 4) {
        colnames(data) <- c("Wavelengths(nm)", "Reference Values", "Target Values", "Reflectance(%)", "file_name")
      }
      } else {
      # If 'all' is FALSE, only select the wavelength and reflectance columns
      if (ncol(data) >= 4) {
        data <- data[, c(1, 4), drop = FALSE]
        colnames(data) <- c("Wavelengths(nm)", "Reflectance(%)")
      } else {
        stop("Data does not have enough columns to extract the required columns!!!")
      }
      data$file_name <- file_name
    }

    # Store the data frame in the list
    all_data[[length(all_data) + 1]] <- data
  }

  # Combine all data frames
  combined_data <- do.call(rbind, all_data)
  return(combined_data)
}


