#' Print expression and its value.
#'
#' The output is a header with the code of the expression and then the value of
#' the expression. It is closed by a footer which also contains the expression.
#'
#' @param expr Any expression
#'
#' @export
debug_print <- function(expr) {
    e <- rlang::enquo(expr)
    name <- deparse(substitute(e))
    cat('>>>>', name, '>>>>\n')
    print(expr)
    cat('<<<<', name, '<<<<\n\n')
}

#' Extract row from data frame.
#'
#' The usual `[.data.frame` requires a `drop = FALSE` in order to return a data
#' frame even if only a single column is selected. Since we do not want to
#' distinguish between the a single and multiple parameters, we need to have
#' consistent behavior.
#'
#' @param df Data frame.
#' @param i Row index.
#'
#' @return A data frame with only the selected row.
get_row <- function(df, i) {
    if (is.null(df)) {
        row <- NULL
    } else if (nrow(df) == 0) {
        row <- NULL
    } else {
        row <- df[i, , drop = FALSE]
    }

    return (row)
}

#' Converts a set of parameters into a human readable string.
#'
#' The format of the resulting string is:
#'
#'     [col1=val1 col2=val2 col3=val3]
#'
#' @param param One line of a data frame
#' @return Name
#'
#' @export
make_name <- function(param) {
    one <- paste(names(param), param, sep = '=', collapse = NULL)
    o <- order(one)
    one <- one[o]
    one <- paste(one, sep = ' ', collapse = ' ')
    paste0('[', one, ']')
}

#' Converts a special pvcontainer into large data frame.
#'
#' When one wants to compare variables across parameter sets, it is desired to
#' obtain one large data frame which contains all the parameters and certain
#' summarizing variables. This function takes a specially prepared pvcontainer
#' which only has the value field `summary` which is a data frame. Each row is
#' prefixed with the corresponding row from the parameter data frame and all
#' intermediate results will be row-joined.
#'
#' @param pv pvcontainer object where each element of `$value` only contains a
#'   data frame at `$summary`.
#'
#' @return A long data frame which is the concatenation of all the
#'   `$value[[i]]$summary` data frames and the `$param` columns.
#'
#' @export
make_summary <- function(pv) {
    res <- list()

    # The `$param` can also be a tibble, if it has been worked on with `dplyr`
    # commands. For some reason the following code will fail to produce a
    # simple data frame. It works when the parameter data frame is forced to be
    # a simple data frame.
    pv$param <- as.data.frame(pv$param)

    for (i in 1:nrow(pv$param)) {
        s <- pv$value[[i]]$summary
        stopifnot(!is.null(s),
                  is.data.frame(s))
        res[[i]] <- cbind(pv$param[i, , drop = FALSE], s, row.names = NULL)
        rownames(res[[i]]) <- NULL
    }

    bound <- do.call(rbind, res)
    rownames(bound) <- NULL

    return (bound)
}

#' Transposes a list of lists.
#'
#' @param ll A numbered list of named lists. It is expected that all the inner
#'   lists have the same names. The outer list is expected to be just numbered,
#'   no names.
#'
#' @return A named list of numbered lists. The names of the outer list are
#'   copied from the first inner list.
#'
#' Heavily inspired by a
#' [Stack Overflow post of Victor K.](https://stackoverflow.com/a/16179574/653152).
#' The snippet is implictly licensed under the MIT/Expat license.
#'
#' @export
list_transpose <- function(ll) {
    stopifnot(all_names_equal(ll))
    n <- length(ll[[1]])
    ll2 <- lapply(1:n, function(i) lapply(ll, '[[', i))
    names(ll2) <- names(ll[[1]])
    return (ll2)
}
