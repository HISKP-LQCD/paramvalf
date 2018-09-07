#' Convert data frame to PV
#'
#' @param df Data frame containing parameters and values dependent upon the parameters
#' @param param_cols Names (not numeric indices!) of the columns in df corresponding
#'                   to parameters
#' @export
df_to_paramval <- function (df, param_cols, ...) {
    value_cols <- setdiff(colnames(df), param_cols)

    param <- df[param_cols]
    value <- lapply(1:nrow(df), function (i) as.list(df[i, value_cols, drop = FALSE]))

    list(param = param, value = value)
}

