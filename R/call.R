#' Calls a function for each row of the parameters.
#'
#' The given PV containers are first joined into a single one. Then for each row
#' in the intermediate PV container the function is called with the row of
#' `param` passed as first argument and the matching row of `value` passed as
#' second.
#'
#' @param func Function which takes a `param` and a `value` parameter
#' @param ... One or more PV containers
#' @param serial Do not use `mclapply` but a plain `for`-loop. This is useful
#'   if one wants to create a plot within the function `func`.
#' @param convert Character vector containing the names of the parameter
#'   colums that are to be converted into values. The parameter data frame that
#'   is created after the inner-outer-join will be grouped by all columns except
#'   the ones listed here.
#' @param dynamic_scheduling Logical, if true the each work packet will be
#'   assigned to a newly forked process. This provides best load balancing with
#'   a high cost of overhead. Should only be used for expensive tasks.
#'
#' @return PV container with the results, same number of rows as the
#'   intermediate PV container.
#'
#' @export
pv_call <- function(func, ..., serial = FALSE, convert = c(), dynamic_scheduling = FALSE) {
    stopifnot(inherits(func, 'function'))

    if (exists('paramval_rval')) {
        paramval_rval <<- NULL
    }

    rvar_name <- deparse(substitute(rvar))

    lapply(list(...), function (pv) stopifnot_rows_unique(pv$param))
    joined <- inner_outer_join(...)

    if (length(convert) > 0) {
        joined <- parameter_to_data(joined, convert)
    }

    indices <- 1:nrow(joined$param)

    closure <- function (i) {
        param_row <- get_row(joined$param, i)
        value_row <- joined$value[[i]]

        result <- func(param_row, value_row)

        return (result)
    }

    pp <- post_process(indices, closure, serial, dynamic_scheduling, joined)

    if (length(pp$value) == 0) {
        stop('There are no results. This could be because every single function call returned `NA` and was therefore discarded.')
    }

    result <-
        list(param = joined$param[pp$not_na, , drop = FALSE],
             value = pp$value)

    if (length(names(result$value[[1]])) == 1 && names(result$value[[1]]) == c('summary')) {
        result <- make_summary(result)
    }

    e <- parent.frame()
    e[[rvar_name]] <- result

    invisible(result)
}

#' Converts parameters to values.
#'
#' Say the lattice spacing of the ensemble is a parameter. Transformations are
#' done separately for each lattice spacing. At some point one wants to combine
#' data sets with different lattice spacing. This function will combine value
#' sets for different lattice spacings into one value set, making the lattice
#' spacing part of the value.
#'
#' The number of parameters is reduced, the number of value colums is increased.
#' The number of rows in the parameter and value data frames is reduced, but
#' each row of the value obtains more columns.
#'
#' @param pv Container
#' @param param_cols_del Character vector with the column names in the `param`
#'   data frame that are to be converted into `value` columns
#' @return Another container
#'
#' @export
#' @import dplyr
#'
#' @examples
#' pv <- list(param = data.frame(a = c(1, 1), b = 3:4),
#'            value = list(list(c = 1), list(c = 2)))
#' print(pv)
#'
#' pv2 <- parameter_to_data(pv, c('b'))
#' print(pv2)
parameter_to_data <- function (pv, param_cols_del) {
    pv$param <- as.data.frame(pv$param)

    # Figure out which parameter columns are to be kept.
    param_cols_all <- colnames(pv$param)
    param_cols_keep <- setdiff(param_cols_all, param_cols_del)

    # Label the parameter rows such that we can select the needed elements from
    # the `value` later on.
    pv$param$.idx <- 1:nrow(pv$param)

    # Group the data by the columns that are kept and attach a list of
    # corresponding row indices.
    grouped <- pv$param %>%
        group_by_at(param_cols_keep) %>%
        summarize(.indices = list(.idx))

    # We do not want to carry the indices in the `grouped` thing to the return
    # value, therefore we extract it here and delete it from the tibble.
    indices <- grouped$.indices
    grouped$.indices <- NULL

    # The new parameters are just the grouped parameters.
    new_param <- grouped

    # For the value we need to extract the corresponding values to each index
    # set for each group of the parameters.
    new_value <- lapply(indices, function (is) {
        # The indices were stored as a list to make it the payload of a tibble,
        # but we know that it is just an integer vector, therefore unpack it.
        is <- unlist(is)

        # The new values from the old values are just ones corresponding to the
        # current index set. In order to provide the user with names again, we
        # need to transpose the selection of value lists.
        value_value <- list_transpose(pv$value[is])

        # From the parameters we need to extract the columns that shall be
        # converted into values. The `value` shall be a named list, therefore
        # we create a bunch of these named lists.
        cols_as_list <- lapply(
            param_cols_del,
            function (col) {
                l <- list()
                l[[col]] <- pv$param[is, col]
                return (l)
            }
        )

        # This bunch of named lists gets merged into a single larger named
        # list.
        param_value <- do.call(c, cols_as_list)

        # Both parts make up the value, so we return the merge of them.
        c(value_value, param_value)
    })

    list(param = new_param,
         value = new_value)
}

post_process <- function (indices, closure, serial, dynamic_scheduling, joined) {
    if (exists('debug_mode') && debug_mode) {
        serial <- TRUE
    } else {
    }

    if (serial) {
        applied <- lapply(indices, closure)
    } else {
        applied <- pbmcapply::pbmclapply(indices,
                                         closure,
                                         mc.cores = num_cores(),
                                         ignore.interactive = want_verbose(),
                                         mc.preschedule = !dynamic_scheduling)
    }

    # Sometimes the closure fails to be evaluated and the return value is just
    # a `try-error` instance. We want to abort if that is the case.
    is_failed <- unlist(lapply(applied, function (x) inherits(x, 'try-error')))

    if (!serial && !dynamic_scheduling && any(is_failed)) {
        cat('\nSome of the function calls failed in the parallel evaluation without dynamic scheduling. There is a known side effect in parallel::mclapply which causes all or most other values from that process to have failed as well. We now re-do them with dynamic scheduling.\n')
        cat('We have to redo', sum(is_failed), 'of', length(is_failed), 'calls.\n')
        applied[is_failed] <- pbmcapply::pbmclapply(
            indices[is_failed],
            closure,
            mc.cores = num_cores(),
            ignore.interactive = want_verbose(),
            mc.preschedule = FALSE)
    }

    is_failed <- unlist(lapply(applied, function (x) inherits(x, 'try-error')))

    if (any(is_failed)) {
        cat('\nSome of the function calls failed. The following parameter row indices are problematic:\n')
        print(which(is_failed))
        cat('These correspond to the following parameters:\n')
        print(joined$param[is_failed, , drop = FALSE])

        cat('The joined paramval object, the return value and the failure vector have been written to the variable `paramval_rval` in the global scope.\n')
        paramval_rval <<- list(is_failed = is_failed,
                               joined = joined,
                               applied = applied)
        stop()
    }

    # The user function is allowed to return `NA` here to signal that the
    # combination of the parameters is not sensible. We must therefore remove
    # the row from the parameter data frame and the value list.
    is_na <- unlist(lapply(applied, function (x) identical(x, NA)))

    # For some reason since 2018-06-11 I see that some elements of `value` are
    # just `NULL` when running the parallel version with `pbmclapply`. The
    # assertion in the closure does not seem to suffice in detecting this. This
    # leads to hard to understand follow-up errors, therefore we also do a
    # check here to assert that nothing is `NULL`.
    is_null <- unlist(lapply(applied, function (x) is.null(x)))

    if (!serial && any(is_null)) {
        cat('Warning: Some return values from parallel processing are NULL. This could be some strange race condition error in mclapply. The following are the indices of the faulty parameter sets:\n')
        print(which(is_null))
        applied[is_null] <- lapply(indices[is_null], closure)
        cat('Warning: The faulty parameter sets will be computed again with a serial lapply.\n')
    }

    is_null <- unlist(lapply(applied, function (x) is.null(x)))

    if (any(is_null)) {
        cat('Error: Some return values are `NULL`. The following are the indices of the faulty parameter sets:\n')
        print(which(is_null))
        stop('Some return values are `NULL`, even after re-running with a serial lapply.')
    }

    list(value = applied[!is_na],
         not_na = !is_na)
}

#' @export
pv_unnest <- function (pv) {
    param <- pv$param
    param$new_param <- lapply(pv$value, function (v) v$paramval$param)
    res <- list(param = tidyr::unnest(param))
    
    if ('value' %in% names(pv$value[[1]]$paramval)) {
        values <- lapply(pv$value, function (v) v$paramval$value)
        res$value <- do.call(c, values)
    }
    
    return (res)
}

#' Detects the number of cores to use
#'
#' This framework often runs on workstations and people have not properly
#' configured the `mc.cores` option. Therefore it is easier to just use all of
#' the cores for them. But then on the `qbig` frontend we do not want to use
#' all of them but just 8 cores. And on the nodes provisioned by SLURM we want
#' to honor the `SLURM_CPUS_PER_TASK` environment variable.
#'
#' @export
num_cores <- function () {
    if (isTRUE(Sys.info()['nodename'] == 'qbig')) {
        8
    } else {
        as.integer(Sys.getenv('SLURM_CPUS_PER_TASK', unset = parallel::detectCores()))
    }
}
