#' Apply `dplyr::filter` on `param`
#'
#' @export
filter_paramval <- function (pv, ...) {
    pv$param$.idx <- 1:nrow(pv$param)

    param_filtered <- dplyr::filter(pv$param, ...)

    list(param = param_filtered,
         value = pv$value[param_filtered$.idx])
}
