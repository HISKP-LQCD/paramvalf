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

    # For the values we just take the relevant lists from the individual values
    # and concatenate them into a single large list.
    value <- list()
    for (i in 1:nrow(param)) {
        id_a <- param$.id_a[i]
        id_b <- param$.id_b[i]
        value[[i]] <- c(a$value[[id_a]], b$value[[id_b]])
    }

    if (length(value) == 0) {
        value <- NULL
    }

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
