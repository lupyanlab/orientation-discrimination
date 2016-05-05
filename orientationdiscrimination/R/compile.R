#' Combine all the per-subject data files in a directory.
#'
#' @param data_dir Name of directory
#' @param regex_key Optional regular expression to specify matches
#'        (e.g., "MWP1*")
#' @param header_file Optional file containing column headers
#' @export
compile <- function(data_dir, regex_key, header_file) {
  if(missing(regex_key)) regex_key <- "*"
  data_files <- list.files(data_dir, regex_key, full.names = TRUE)

  if(missing(header_file)) {
    frame <- plyr::ldply(data_files, readr::read_csv)
  } else {
    header_file <- ifelse(file.exists(header_file), header_file,
                          file.path(data_dir, header_file))
    header <- colnames(readr::read_tsv(header_file))

    # hack!!!
    if(sum(header == "response") == 2) {
      header[header == "response"] <- c("response", "response.1")
    }

    data_files <- list.files(data_dir, regex_key, full.names = TRUE)
    frame <- plyr::ldply(data_files, readr::read_tsv, col_names = header)
  }

  frame
}
