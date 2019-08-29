paramval <- function (param, value) {
    pv <- list(param = param, value = value)
    class(pv) <- append(class(pv), 'paramval')
    return (pv)
}
