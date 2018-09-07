#' Convert TSV to paramval
#'
#' @export
tsv_to_paramval <- function (filename, param_cols, ...) {
    data <- read.table(filename, header = TRUE, stringsAsFactors = FALSE, ...)
    
    df_to_paramval(data, param_cols, ...)
}

