context('Call')

p0 <- list(param = data.frame(p0 = 3.2))
p1 <- list(param = data.frame(p1 = c(4)))
p2 <- list(param = data.frame(p2 = c(TRUE, FALSE)))
p3 <- list(param = data.frame(p3a = 'foo',
                              p3b = 5))

# We may want to pass only one structure with only one variable. The name of the
# variable must not get lost.
test_that('one single parameter', {
    f <- function(param, value) {
        list(v0 = param$p0)
    }

    pv <- call.pvcontainer(f, p0)

    expect_equal(colnames(pv$param), c('p0'))
    expect_equal(names(pv$value[[1]]), c('v0'))
    expect_equal(pv$value[[1]]$v0, 3.2)
})

# Similarly we do two structures with only one parameter each.
test_that('two single parameter', {
    f <- function(param, value) {
        list(v0 = param$p0,
             v1 = param$p1)
    }

    pv <- call.pvcontainer(f, p0, p1)

    expect_equal(colnames(pv$param), c('p0', 'p1'))
    expect_equal(names(pv$value[[1]]), c('v0', 'v1'))
})

test_that('one two parameters', {
    f <- function(param, value) {
        list(v0 = param$p3a,
             v1 = param$p3b)
    }

    pv <- call.pvcontainer(f, p3)

    expect_equal(colnames(pv$param), c('p3a', 'p3b'))
    expect_equal(names(pv$value[[1]]), c('v0', 'v1'))
})
