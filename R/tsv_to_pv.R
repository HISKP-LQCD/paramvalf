#' Convert TSV to paramval
#'
#' @export
tsv_to_paramval <- function (filename, param_cols, ...) {
    data <- read.table(filename, header = TRUE, stringsAsFactors = FALSE, ...)

    value_cols <- setdiff(colnames(data), param_cols)

    param <- data[param_cols]
    value <- lapply(1:nrow(data), function (i) as.list(data[i, value_cols, drop = FALSE]))

    list(param = param, value = value)
}

