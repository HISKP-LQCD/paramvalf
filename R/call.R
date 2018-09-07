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
#'
#' @return PV container with the results, same number of rows as the
#'   intermediate PV container.
#'
#' @export
pv_call <- function(cluster, rvar, func, ..., serial = FALSE, convert = c(), store = TRUE) {
    stopifnot(inherits(func, 'function'))

    rvar_name <- deparse(substitute(rvar))

    joined <- inner_outer_join(...)

    if (length(convert) > 0) {
        joined <- parameter_to_data(joined, convert)
    }

    indices <- 1:nrow(joined$param)

    closure <- function (i) {
        param_row <- get_row(joined$param, i)
        value_row <- joined$value[[i]]

        value_loaded <- load_lazy_value.list(value_row)

        result <- func(param_row, value_loaded)

        rm(value_loaded)

        if (store &&
            object.size(result) * length(indices) >= get_lazy_threshold() &&
            !(length(names(result)) == 1 && names(result) == c('summary'))) {
            for (name in names(result)) {
                result[[name]] <- lazy_value(result[[name]], cluster, rvar_name, i, name)
            }
        }

        return (result)
    }

    delete_rdata_directory(cluster, rvar_name)

    pp <- post_process(indices, closure, serial)

    result <-
        list(param = joined$param[pp$not_na, , drop = FALSE],
             value = pp$value)

    if (length(names(result$value[[1]])) == 1 && names(result$value[[1]]) == c('summary')) {
        result <- make_summary(result)
    }

    e <- parent.frame()
    e[[rvar_name]] <- result

    if (store) {
        pv_save(cluster, rvar, name = rvar_name)
    }

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
    # Figure out which parameter columns are to be kept.
    param_cols_all <- colnames(pv$param)
    param_cols_keep <- setdiff(param_cols_all, param_cols_del)

    # Label the parameter rows such that we can select the needed elements from
    # the `value` later on.
    pv$param$.idx <- 1:nrow(pv$param)

    grouped <- pv$param %>%
        group_by_at(param_cols_keep) %>%
        summarize(.indices = list(.idx))

    indices <- grouped$.indices
    grouped$.indices <- NULL

    new_param <- grouped
    new_value <- lapply(indices, function (is) {
        is <- unlist(is)
        c(list_transpose(pv$value[is]), do.call(c, lapply(param_cols_del, function (col) { l <- list(); l[[col]] <- pv$param[is, col]; l})))
    })

    list(param = new_param,
         value = new_value)
}

post_process <- function (indices, closure, serial) {
    if (exists('debug_mode') && debug_mode) {
        serial <- TRUE
    } else {
    }

    if (serial) {
        applied <- lapply(indices, closure)
    } else {
        applied <- pbmcapply::pbmclapply(indices,
                                         closure,
                                         ignore.interactive = want_verbose())
        #applied <- parallel::mclapply(indices, closure)
    }

    # Sometimes the closure fails to be evaluated and the return value is just
    # a `try-error` instance. We want to abort if that is the case.
    is_failed <- unlist(lapply(applied, function (x) inherits(x, 'try-error')))

    if (any(is_failed)) {
        cat('Some of the function calls failed. Here is the return value:\n')
        print(applied)
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
