#' Apply `dplyr::filter` on `param`
#'
#' @export
filter_paramval <- function (pv, ...) {
    pv$param$.idx <- 1:nrow(pv$param)

    param_filtered <- dplyr::filter(pv$param, ...)
    indices <- param_filtered$.idx
    param_filtered$.idx <- NULL

    list(param = param_filtered,
         value = pv$value[indices])
}
