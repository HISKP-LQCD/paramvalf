#' Calls a function for each row of the parameters.
#'
#' The given PV containers are first joined into a single one. Then for each row
#' in the intermediate PV container the function is called with the row of
#' `param` passed as first argument and the matching row of `value` passed as
#' second.
#'
#' @param func Function which takes a `param` and a `value` parameter
#' @param ... One or more PV containers
#'
#' @return PV container with the results, same number of rows as the
#'   intermediate PV container.
call.pvcontainer <- function(func, ...) {
    joined <- inner_outer_join(...)

    debug_mode <- TRUE

    if (exists('debug_mode') && debug_mode) {
        value <- list()

        for (i in 1:nrow(joined$param)) {
            param_row <- get_row(joined$param, i)
            value_row <- get_row(joined$value, i)

            value[[i]] <- func(param_row, value_row)

            if (!is.list(value[[i]])) {
                stop('Return value must be a list.')
            }
        }
    }
    else {
        value <- parallel::mclapply(1:nrow(joined$param), function (i) {
            param_row <- get_row(joined$param, i)
            value_row <- get_row(joined$value, i)

            v <- func(param_row, value_row)

            if (!is.list(v)) {
                stop('Return value must be a list.')
            }

            return (v)
        })

    }

    list(param = joined$param,
         value = do.call(rbind, value))
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
parameter_to_data <- function(pv, func, param_cols_del) {
    # Figure out which parameter columns are to be kept.
    param_cols_all <- colnames(pv$param)
    param_cols_keep <- setdiff(param_cols_all, param_cols_del)

    # Label the parameter rows such that we can select the needed rows from the
    # `value` later on.
    pv$param$.idx <- 1:nrow(pv$param)

    grouped <- pv$param %>%
        group_by_at(param_cols_keep) %>%
        summarize(.indices = list(.idx))

    indices <- grouped$.indices
    grouped$.indices <- NULL

    applied <- dplyr::mclapply(indices, function (is) {
        is <- unlist(is)
        func(pv$param[is, ], pv$value[is, ])
    })

    list(param = grouped,
         value = do.call(rbind, applied))
}

pvcall_group <- function(func, param_cols_del, ...) {
    joined <- inner_outer_join(...)
    parameter_to_data(joined, func, param_cols_del)
}
