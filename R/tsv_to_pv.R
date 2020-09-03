#' Convert TSV to paramval
#'
#' @param filename Character. Path to the file.
#' @param param_cols Character vector. Column names which are to be taken as parameters. All other columns are taken as values.
#' @param ... Arguments passed to `read.table` and `df_to_paramval`.
#'
#' @importFrom utils read.table
#' @export
tsv_to_paramval <- function (filename, param_cols, ...) {
    data <- read.table(filename, header = TRUE, stringsAsFactors = FALSE, ...)
    
    df_to_paramval(data, param_cols, ...)
}


#' Convert TSV to parameters
#'
#' @param filename Character. Path to the file.
#'
#' @importFrom utils read.table
#' @export
tsv_to_param <- function (filename) {
    paramval(param = read.table(filename, header = TRUE, stringsAsFactors = FALSE))
}
