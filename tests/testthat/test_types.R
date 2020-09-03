context('Types')

l <- list(list(a = 3, b = 4, c = 4),
          list(a = 2, b = 9, c = 1),
          list(a = 1, b = 0, c = 3))

test_that('all_names_equal true', {
    expect(all_names_equal(l), 'all names must be equal')
})
