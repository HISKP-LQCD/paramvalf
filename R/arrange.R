#' Wrapper for `dplyr::arrange`
#'
#' This function takes the `dplyr::arrange` functionality and applies it to the
#' `param` section of a PV object. The `value` section is accordingly sorted.
#'
#' @param pv PV container.
#' @param ... Arguments passed to `dplyr::arrange`.
#'
#' @export
pv_arrange <- function (pv, ...) {
    pv$param$.idx <- 1:nrow(pv$param)
    pv$param <- pv$param %>%
        arrange(...)
    pv$value <- pv$value[pv$param$.idx]
    pv$param$.idx <- NULL
    return (pv)
}
