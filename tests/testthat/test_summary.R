context('Summary')

pv2 <- list(param = data.frame(p5 = c(-1, -2)),
            value = list(list(v3 = 'A',
                              v4 = 'B'),
                         list(v3 = 'C',
                              v4 = 'D')))

test_that('summary', {
    f <- function(param, value) {
        list(summary = data.frame(x = 1:10,
                                  y = 11:20,
                                  z = value$v3))
    }

    s <- pv_call(f, pv2)

    expect(is.data.frame(s))
    expect_equal(nrow(s), 10 * 2)
    expect_equal(colnames(s), c('p5', 'x', 'y', 'z'))
})

test_that('summary with empty', {
    f <- function(param, value) {
        if (param$p5 == -1) {
            list(summary = data.frame(x = 1:10,
                                      y = 11:20,
                                      z = value$v3))
        } else {
            list(summary = data.frame())
        }
    }

    s <- pv_call(f, pv2)

    expect(is.data.frame(s))
    expect_equal(nrow(s), 10)
    expect_equal(colnames(s), c('p5', 'x', 'y', 'z'))
})

test_that('summary only empty', {
    f <- function(param, value) {
        list(summary = data.frame())
    }

    s <- pv_call(f, pv2)

    expect(is.data.frame(s))
    expect_equal(nrow(s), 0)
})
