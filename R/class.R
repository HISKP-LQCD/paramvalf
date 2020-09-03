#' paramval constructor
#'
#' @param param data.frame with parameters
#' @param value list with named lists.
#'
#' @export
paramval <- function (param, value) {
    pv <- list(param = param)
    if (!missing(value)) {
        pv$value <- value
    }
    class(pv) <- append(class(pv), 'paramval')
    return (pv)
}
