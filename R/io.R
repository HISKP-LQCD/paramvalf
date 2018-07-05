#' Get path to root directory
#'
#' Traverses the directory hierarchy upwards until it has the found the root of
#' the analysis. This root must be marked with the file `.paramvalf-root`.
#'
#' @export
get_root_dir <- function() {
    d <- getwd()

    while (!file.exists(paste0(d, '/.paramvalf-root'))) {
        dd <- dirname(d)

        if (dd == d) {
            stop('The root directory of the analysis must be marked with the file `.paramvalf-root`. Such a file could not be found on any level of file system from the current working directory upward. Please create this file in the appropriate location.')
        }

        d <- dd
    }

    return (d)
}

#' @export
make_filename <- function (cluster, varname) {
    sprintf('%s/output/%s/%s.Rdata', get_root_dir(), cluster, varname)
}

#' @export
pv_save <- function (cluster, x) {
    varname <- deparse(substitute(x))
    filename <- make_filename(cluster, varname)
    save(list = varname, file = filename)
}

#' @export
pv_load <- function (cluster, x) {
    varname <- deparse(substitute(x))
    filename <- make_filename(cluster, varname)
    #cat('Loading from ', filename, '\n', sep = '')
    e = parent.frame()
    load(filename, envir = e)
}
