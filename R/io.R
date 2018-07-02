#' Get path to current script file
#'
#' https://stackoverflow.com/a/15373917/653152
this_file <- function() {
    cmdArgs <- commandArgs(trailingOnly = FALSE)
    needle <- "--file="
    match <- grep(needle, cmdArgs)
    if (length(match) > 0) {
        # Rscript
        return(normalizePath(sub(needle, "", cmdArgs[match])))
    } else {
        # 'source'd via R console
        return(normalizePath(sys.frames()[[1]]$ofile))
    }
}

#' @export
make_filename <- function (varname) {
    dir_path <- dirname(this_file())
    dir_name <- basename(dir_path)
    p_dir_path <- dirname(dir_path)
    p_dir_name <- basename(p_dir_path)
    pp_dir_path <- dirname(p_dir_path)
    pp_dir_name <- basename(pp_dir_path)

    root_dirs <- c('paramval', 'vignette')

    if (p_dir_name %in% root_dirs) {
        cluster <- dir_name
        path <- sprintf('%s/output/%s/%s.Rdata', pp_dir_path, cluster, varname)
    } else if (dir_name %in% root_dirs) {
        path <- sprintf('%s/output/%s.Rdata', p_dir_path, varname)
    } else {
        stop('Path could not be properly resolved. Ask Martin.')
    }

    return (path)
}

#' @export
pv_save <- function (x) {
    varname <- deparse(substitute(x))
    filename <- make_filename(varname)
    save(list = varname, file = filename)
}

#' @export
pv_load <- function (x) {
    filename <- make_filename(deparse(substitute(x)))
    #cat('Loading from ', filename, '\n', sep = '')
    e = parent.frame()
    load(filename, envir = e)
}
