#' Apply `dplyr::filter` on `param`
#'
#' This applies the filter to the parameter section, then also filters the
#' value section accordingly and returns a filtered paramval object.
#'
#' @param pv PV object
#' @param ... Passed to `dplyr::filter`
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
