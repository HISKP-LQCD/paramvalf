context('Join')

p0 <- list(param = data.frame(p0 = 3.2))
p1 <- list(param = data.frame(p1 = c(4)))
p2 <- list(param = data.frame(p2 = c(TRUE, FALSE)))
p3 <- list(param = data.frame(p3a = 'foo',
                              p3b = c(5, 7)))

pv1 <- list(param = data.frame(p4 = TRUE),
            value = list(list(v1 = 3,
                              v2 = 5)))
pv2 <- list(param = data.frame(p5 = c(-1, -2)),
            value = list(list(v3 = 'A',
                              v4 = 'B'),
                         list(v3 = 'C',
                              v4 = 'D')))

test_that('single', {
    j <- inner_outer_join(p0)

    expect(is.pvcontainer(j), 'must be a PV')
    expect_equal(j, p0)
})

test_that('two', {
    j <- inner_outer_join(p0, p3)

    expect(is.pvcontainer(j), 'must be a PV')
    expect_equal(nrow(j$param), 2)
    expect_equal(colnames(j$param), c('p0', 'p3a', 'p3b'))
})

test_that('with value', {
    j <- inner_outer_join(pv1, pv2)

    expect(is.pvcontainer(j), 'must be a PV')
    expect_equal(nrow(j$param), 2)
    expect_equal(colnames(j$param), c('p4', 'p5'))
    expect_equal(names(j$value[[1]]), c('v1', 'v2', 'v3', 'v4'))
})
