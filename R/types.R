is.pvcontainer <- function(pv) {
    is.list(pv) &&
        is.data.frame(pv$param) &&
        (is.null(pv$value) ||
             is.list(pv$value) &&
             all_names_equal(pv$value))
}

all_names_equal <- function(l) {
    if (length(l) == 0) {
        return (TRUE)
    }

    names <- lapply(l, names)
    if (all(sapply(names, is.null))) {
        return (TRUE)
    }
    all_names <- do.call(c, names)
    name_matrix <- matrix(all_names, nrow = length(names[[1]]))
    u <- apply(name_matrix, 1, function (row) { all(row == row[1]) })
    all(u)
}

stopifnot_rows_unique <- function (df) {
    dups <- duplicated(df)
    if (any(dups)) {
        stop(sprintf('The following rows are duplicates: %s',
                     paste(which(dups), collapse = ', ')))
    }
}
