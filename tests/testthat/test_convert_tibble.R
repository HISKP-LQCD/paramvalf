context('convert')

test_that('dataframe', {
    pv <- list(param = data.frame(a = c(1, 1, 1), b = 1:3))

    .func <- function (param, value) {
        expect_equal(value$b, 1:3)
        list()
    }

    pv_call(.func, pv, convert = c('b'), serial = TRUE)
})

test_that('tibble', {
    pv <- list(param = tibble(a = c(1, 1, 1), b = 1:3))

    .func <- function (param, value) {
        expect_equal(value$b, 1:3)
        list()
    }

    pv_call(.func, pv, convert = c('b'), serial = TRUE)
})
