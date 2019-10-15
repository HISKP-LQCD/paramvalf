#' paramval constructor
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
