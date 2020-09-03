#' get an overview over all available param values in a paramval object
#'
#' @param pv paramval object
#' @return named list with names corresponding to `names(pv$param)`
#' containing the unique values for each column of `pv$param`
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
#' @param ... Ignored.
#'
#' @export
print.param_overview <- function (x, ...) {
  stopifnot("param_overview" %in% class(x))
  for (p_name in names(x)) {
    cat(p_name, ": ", paste(x[[p_name]], collapse = ' '), "\n", sep = '')
  }
}

#' summarize `param_overview` object
#'
#' @param object Object of class `param_overview`
#' @param ... Ignored.
#'
#' @export
summary.param_overview <- function (object, ...) {
  stopifnot("param_overview" %in% class(object))
  print(object, ...)
}
