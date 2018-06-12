library(dplyr)

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
#'
#' @return PV container with the results, same number of rows as the
#'   intermediate PV container.
#'
#' @export
pvcall <- function(func, ..., serial = FALSE) {
    joined <- inner_outer_join(...)

    if (exists('debug_mode') && debug_mode || serial) {
        value <- list()

        for (i in 1:nrow(joined$param)) {
            param_row <- get_row(joined$param, i)
            value_row <- joined$value[[i]]

            value[[i]] <- func(param_row, value_row)

            if (!(is.list(value[[i]]) || is.na(value[[i]]))) {
                stop('Return value must be a list or NA.')
            }
        }
    }
    else {
        value <- pbmcapply::pbmclapply(1:nrow(joined$param), function (i) {
            param_row <- get_row(joined$param, i)
            value_row <- joined$value[[i]]

            v <- func(param_row, value_row)

            if (!(is.list(v) || is.na(v))) {
                stop('Return value must be a list, or NA.')
            }

            return (v)
        })

    }

    joined$value <- NULL
    gc()

    # The user function is allowed to return `NA` here to signal that the
    # combination of the parameters is not sensible. We must therefore remove
    # the row from the parameter data frame and the value list.
    not_na <- unlist(lapply(value, function (x) !identical(x, NA)))

    # For some reason since 2018-06-11 I see that some elements of `value` are
    # just `NULL` when running the parallel version with `pbmclapply`. The
    # assertion in the closure does not seem to suffice in detecting this. This
    # leads to hard to understand follow-up errors, therefore we also do a
    # check here to assert that nothing is `NULL`.
    not_null <- unlist(lapply(value, function (x) !is.null(x)))

    if (!all(not_null)) {
        cat('Some return values are `NULL`. This might be an issue with `pbmclapply`. In the following, every `FALSE` marks a `NULL` value:\n')
        print(not_null)
        stop()
    }

    list(param = joined$param[not_na, , drop = FALSE],
         value = value[not_na])
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
#' @import dplyr
parameter_to_data <- function(pv, func, param_cols_del, serial = FALSE) {
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

    if (exists('debug_mode') && debug_mode || serial) {
        applied <- list()
        for (i in 1:length(indices)) {
            is <- indices[[i]]
            is <- unlist(is)
            applied[[i]] <- func(pv$param[is, ], list_transpose(pv$value[is]))
        }
    } else {
        applied <- pbmcapply::pbmclapply(indices, function (is) {
            is <- unlist(is)
            func(pv$param[is, ], list_transpose(pv$value[is]))
        })
    }

    # The user function is allowed to return `NA` here to signal that the combination of the parameters is not sensible. We must therefore remove the row from the parameter data frame and the value list.
    not_na <- unlist(lapply(applied, function (x) !identical(x, NA)))

    list(param = grouped[not_na, ],
         value = applied[not_na])
}


#' Converts parameters to data and then does the same as pvcall.
#'
#' @param func A function which expects \code{param} and \code{value} as
#'   parameters. These are lists with the names of from the \code{...} parameter
#'   here. Each element contains a list with the number of elements that have
#'   the same set of parameters after the grouping by \code{param_cols_del}.
#' @param param_cols_del Character vector containing the names of the parameter
#'   colums that are to be converted into values. The parameter data frame that
#'   is created after the inner-outer-join will be grouped by all columns except
#'   the ones listed here.
#' @param ... One or more pvcontainer objects.
#' @param serial If true, execution will be distributed onto multiple cores.
#'
#' @return A pvcontainer object.
#' @export
pvcall_group <- function(func, param_cols_del, ..., serial = FALSE) {
    joined <- inner_outer_join(...)
    rval <- parameter_to_data(joined, func, param_cols_del, serial)

    rm(joined)
    gc()

    return (rval)
}
