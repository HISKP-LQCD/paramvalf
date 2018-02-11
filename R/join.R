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

    cols_common <- dplyr::intersect(colnames(ap), colnames(bp))

    cols_only_a <- dplyr::setdiff(colnames(ap), cols_common)
    cols_only_b <- dplyr::setdiff(colnames(bp), cols_common)

    # Let `dplyr` perform the inner join. It will automatically detect common
    # columns and to an inner product to them. Non-common columns are joined as
    # in outer product.
    param <- dplyr::inner_join(ap, bp, by = cols_common)

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
