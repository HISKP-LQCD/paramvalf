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
pv_save <- function (cluster, x, name, write_unchanged = TRUE) {
    if (missing(name)) {
        varname <- deparse(substitute(x))
    } else {
        varname <- name
    }
    filename <- make_filename(cluster, varname)

    if (file.exists(filename) && !write_unchanged) {
        # The user has requested us to only write then something has changed,
        # so we first load the old value. The problem is that this might shadow
        # a local variable here if we just use `pv_load`. Therefore we load
        # into a fresh environment and extract the value.
        env <- new.env()
        pv_load(cluster, name = varname, envir = env)

        # We expect only a single variable to be loaded.
        n <- names(env)
        stopifnot(length(n) == 1)

        old_value <- env[[n[[1]]]]

        # In case the old and new value are the same, we can just quit here and
        # need no further work.
        if (identical(x, old_value)) {
            cat(sprintf('No changes have been made to %s, not overwriting.\n', varname))
            return ()
        }
    }

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
pv_load <- function (cluster, x, name, envir = NULL) {
    if (missing(name)) {
        varname <- deparse(substitute(x))
    } else {
        varname <- name
    }
    filename <- make_filename(cluster, varname)
    #cat('Loading from ', filename, '\n', sep = '')

    if (is.null(envir)) {
        envir = parent.frame()
    }

    if (want_verbose()) {
        cat('Loading', varname, '...')
    }
    start_time <- Sys.time()

    vars <- load(filename, envir = envir)

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
lazy_value <- function (sub_value, cluster, name, index, value_name) {
    path <- sprintf('%s/output/%s/%s.Rdata.dir/%d-%s.Rdata', get_root_dir(), cluster, name, index, value_name)
    if (!dir.exists(dirname(path))) {
        stopifnot(dir.create(dirname(path)))
    }
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
    getOption('paramvalf_lazy_threshold', 1000 * 2^20)
}

delete_rdata_directory <- function (cluster, name) {
    path <- sprintf('%s/output/%s/%s.Rdata.dir', get_root_dir(), cluster, name)
    if (dir.exists(path)) {
        stopifnot(unlink(path, recursive = TRUE) == 0)
    }
}
