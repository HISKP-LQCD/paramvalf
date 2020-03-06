#' @export
pv_arrange <- function (pv, ...) {
    pv$param$.idx <- 1:nrow(pv$param)
    pv$param <- pv$param %>%
        arrange(...)
    pv$value <- pv$value[pv$param$.idx]
    pv$param$.idx <- NULL
    return (pv)
}
