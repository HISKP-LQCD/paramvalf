#' get an overview over all available param values in a paramval object
#'
#' @param pv paramval object
#' @return named list with names corresponding to \code{names(pv$param)}
#' containing the unique values for each column of \code{pv$param}
#'
#' @export
param_overview <- function (pv) {
  stopifnot(!is.null(pv$param))
  unique_params <- lapply(pv$param, unique)
  class(unique_params) <- append(class(unique_params), "param_overview")
  return (unique_params)
}

#' print `param_overview` object
#'
#' @param x object of class `param_overview`
#' @export
print.param_overview <- function (x) {
  stopifnot("param_overview" %in% class(x))
  for (p_name in names(x)) {
    cat(p_name, ": ", x[[p_name]], "\n")
  }
}

#' summarize `param_overview` object
#'
#' @param x object of class `param_overview`
#' @export
summary.param_overview <- function (x) {
  stopifnot("param_overview" %in% class(x))
  print(x)
}

