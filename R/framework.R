source('R/support.R')

library(dplyr)

#' Print expression and its value.
#' 
#' The output is a header with the code of the expression and then the value of
#' the expression. It is closed by a footer which also contains the expression.
#' 
#' @param expr Any expression
debug_print <- function(expr) {
    e <- enquo(expr)
    name <- deparse(substitute(e))
    cat('>>>>', name, '>>>>\n')
    print(expr)
    cat('<<<<', name, '<<<<\n\n')
}


#' Joins two PV containers.
#' 
#' The two containers `a` and `b` may have common and distinct parameters. The
#' combination will be an inner join (inner product) on the common parameters
#' and an outer join (outer product) on the distinct parameters. This way all
#' possible combinations are created without creating redundancy.
#' 
#' @param a PV container
#' @param b PV container
#'   
#' @return PV container
inner_outer_join_impl <- function(a, b) {
    # Extract the parameter sets from the containers.
    ap <- a$param
    bp <- b$param

    # Label the rows of the parameters such that we know which value rows to
    # use later.
    ap$.id_a <- 1:nrow(ap)
    bp$.id_b <- 1:nrow(bp)

    ap$.dummy <- TRUE
    bp$.dummy <- TRUE

    cols_common <- intersect(colnames(ap), colnames(bp))

    cols_only_a <- setdiff(colnames(ap), cols_common)
    cols_only_b <- setdiff(colnames(bp), cols_common)

    # Let `dplyr` perform the inner join. It will automatically detect common
    # columns and to an inner product to them. Non-common columns are joined as
    # in outer product.
    param <- inner_join(ap, bp, by = cols_common)

    param$.dummy <- NULL

    if (!is.null(a$value) && !is.null(b$value)) {
        # For the values we just take the relevant rows from the individual
        # containers and bind them to a larger data frame.
        value <- cbind(a$value[param$.id_a, ], b$value[param$.id_b, ])

        # We need to manually reset the column names because the column name
        # gets lost when we have a data frame with only one column.
        colnames(value) <- c(colnames(a$value), colnames(b$value))
    }
    else if (!is.null(a$value)) {
        value <- cbind(a$value[param$.id_a, ])
        colnames(value) <- colnames(a$value)
    }
    else if (!is.null(b$value)) {
        value <- cbind(b$value[param$.id_b, ])
        colnames(value) <- colnames(b$value)
    }
    else {
        value <- data.frame()
    }

    # The row names are of no concern, so we reset them.
    rownames(value) <- NULL

    # The index columns are no longer needed.
    param$.id_a <- NULL
    param$.id_b <- NULL

    list(param = param,
         value = value)
}

#' Generalization of `inner_outer_join_impl` to arbitrary many objects.
#' 
#' This is a left fold of the given parameters.
#'
#' @param a PV container
#' @param ... Zero or more PV containers
#'
#' @return PV container
inner_outer_join <- function(a, ...) {
    bs = list(...)

    out <- a

    for (b in bs) {
        out <- inner_outer_join_impl(out, b)
    }

    return (out)
}

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

    if (exists('debug_mode') && debug_mode) {
        value <- list()

        for (i in 1:nrow(joined$param)) {
            value[[i]] <- func(joined$param[i, ], joined$value[i, ])

            if (!is.list(value[[i]])) {
                stop('Return value must be a list.')
            }
        }
    }
    else {
        value <- mclapply(1:nrow(joined$param), function (i) {
                              param <- joined$param[i, ]
                              value <- joined$value[i, ]

                              cat(make_name(param), '\n', sep = '')

                              v <- func(param, value)

                              if (!is.list(v)) {
                                  stop('Return value must be a list.')
                              }

                              return (v)
         })

    }

    list(param = joined$param,
         value = do.call(rbind, value))
}

#' Converts a set of parameters into a human readable string.
#'
#' The format of the resulting string is:
#'
#'     [col1=val1 col2=val2 col3=val3]
#' 
#' @param param One line of a data frame
#' @return Name
make_name <- function(param) {
    one <- paste(names(param), param, sep = '=', collapse = NULL)
    o <- order(one)
    one <- one[o]
    one <- paste(one, sep = ' ', collapse = ' ')
    paste('[', one, ']', sep = '')
}

make_summary <- function(pv) {
    res <- list()

    for (i in 1:nrow(pv$param)) {
        res[[i]] <- cbind(pv$param[i, ], pv$value[i, ][[1]])
        rownames(res[[i]]) <- NULL
    }

    bound <- do.call(rbind, res)
    rownames(bound) <- NULL

    return (bound)
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
    
    applied <- mclapply(indices, function (is) {
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
