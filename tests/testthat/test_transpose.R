context('List transpose')

ll <- list(list(a = 5, b = 3, c = 2),
           list(a = 3, b = 5, c = 1))

test_that('transpose', {
    ll2 <- list_transpose(ll)

    expect_equal(length(ll2), 3)
    expect_equal(names(ll2), c('a', 'b', 'c'))
    expect_equal(ll2$a, list(5, 3))
})
