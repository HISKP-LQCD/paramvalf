paramval <- function (param, value) {
    pv <- list(param = param, value = value)
    pv$param$.idx <- 1:nrow(pv$param)
    class(pv) <- append(class(pv), 'paramval')
    return (pv)
}
