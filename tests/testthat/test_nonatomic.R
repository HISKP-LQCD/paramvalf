# A collections of tests that makes sure that non-trivial value types still work.

context('Non-Atomic')

p0 <- list(param = data.frame(a = c(3, 5),
                              b = c('a', 'b')))

test_that('list', {
    # We have a function which returns a list with non-atomic vectors.
    f <- function(param, value) {
        list(v1 = list(3, 'X'),
             v2 = p0)
    }

    v <- pv_call(f, p0)

    # Now we need to make sure that we can easily access the variables within
    # another function, i.e. the expression `value$v1` must work, we do not want
    # to need `value[[1, 'v1']]` in order to get the variable.
    f2 <- function(param, value) {
        expect(!is.null(value$v1), 'value$v1 must not be NULL')
        expect_equal(value$v1, list(3, 'X'))

        list()
    }

    v2 <- pv_call(f2, v)

    expect(is.pvcontainer(v2), 'v2 must be a PV')
})
