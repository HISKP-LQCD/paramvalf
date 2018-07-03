#' Convert TSV to paramval
#'
#' @export
tsv_to_paramval <- function (filename, param_cols, ...) {
    data <- read.table(filename, header = TRUE, stringsAsFactors = FALSE, ...)

    value_cols <- setdiff(colnames(data), param_cols)

    param <- data[param_cols]
    value <- as.list(data[value_cols])

    list(param = param, value = value)
}
