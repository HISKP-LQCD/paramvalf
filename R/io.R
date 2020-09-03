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

#' Generate a path to Rdata
#'
#' Generates the path to an Rdata file based on the current cluster and the
#' variable name.
#'
#' @param cluster Character. Name of the project.
#' @param varname, Character. Name of the variable.
#'
#' @export
make_filename <- function (cluster, varname) {
    sprintf('%s/output/%s/%s.Rdata', get_root_dir(), cluster, varname)
}

#' Saves a variable to a path based on its name.
#'
#' Stores the given variable in an Rdata file.
#'
#' @param cluster Character. Name of the project.
#' @param x Variable
#' @param name Character. If given, it will determine the filename, otherwise the expression `x` is deparsed.
#' @param write_unchanged Logical. Shall the contents be written even if nothing has changed?
#' @param compress Logical. Shall the contents be compressed? The compression does not make much sense when writing random numerical data, therefore it is turned off by default.
#' @export
pv_save <- function (cluster, x, name, write_unchanged = TRUE, compress = FALSE) {
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
    save(list = varname, file = filename, compress = compress)
    end_time <- Sys.time()

    if (want_verbose()) {
        cat(' took', sprintf('%.2f', end_time - start_time), 'seconds.\n')
    }
}

#' Loads a variable
#'
#' The variables are assigned a path based on their variable name. This function loads it and puts them into the global environment.
#'
#' @param cluster Character. Name of the project.
#' @param x Expression. Name of the variable, without quites.
#' @param name Character. Name of the variable as character, can be used instead of the `x` parameter.
#' @param envir Environment. Can be used to load to somewhere else.
#'
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

    end_time <- Sys.time()
    if (want_verbose()) {
        cat(' took', sprintf('%.2f', end_time - start_time), 'seconds.\n')
    }
}
