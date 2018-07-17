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

    if (want_verbose()) {
        cat('Saving', varname, '...')
    }

    start_time <- Sys.time()
    save(list = varname, file = filename)
    end_time <- Sys.time()

    if (want_verbose()) {
        cat(' took', sprintf('%.2f', end_time - start_time), 'seconds.\n')
    }
}

#' @export
pv_load <- function (cluster, x, eager = TRUE) {
    varname <- deparse(substitute(x))
    filename <- make_filename(cluster, varname)
    #cat('Loading from ', filename, '\n', sep = '')
    e = parent.frame()

    if (want_verbose()) {
        cat('Loading', varname, '...')
    }
    start_time <- Sys.time()

    vars <- load(filename, envir = e)

    if (eager) {
        for (var in vars) {
            e[[var]]$value <- lapply(e[[var]]$value, load_lazy_value.list)
        }
    }

    end_time <- Sys.time()
    if (want_verbose()) {
        cat(' took', sprintf('%.2f', end_time - start_time), 'seconds.\n')
    }
}

#' Create a lazy value
#'
#' The given value is stored on disk (using `save`) and a handle for retrieving
#' it will be returned.
#'
#' @export
lazy_value <- function (sub_value) {
    path <- sprintf('%s/output/%s.Rdata', get_root_dir(), uuid::UUIDgenerate())
    save(sub_value = sub_value, file = path)

    self <- list(path = path)
    class(self) <- append(class(self), 'lazy_value')
    return (self)

    #rlang::env_bind_exprs(environment(), lv = { load.lazy_value(self) })
    #return (lv)
}

#' Load a lazy value
#'
#' @export
load.lazy_value <- function (self) {
    stopifnot(inherits(self, 'lazy_value'))

    vars <- load(self$path)
    stopifnot(any('sub_value' %in% vars))

    return (sub_value)
}

load_lazy_value.list <- function (self) {
    lapply(self, function (x) {
        if (inherits(x, 'lazy_value')) {
            return (load.lazy_value(x))
        } else {
            return (x)
        }
    })
}

get_lazy_threshold <- function () {
    getOption('paramvalf_lazy_threshold', 0.5 * 2^30)
}
