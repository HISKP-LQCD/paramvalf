context('unique')

test_that('unique', {
    df <- data.frame(a = 1:3, b = 4:6)
    stopifnot_rows_unique(df)
})

test_that('non-unique', {
    df <- data.frame(a = c(1, 1:3), b = c(4, 4:6))
    expect_error(stopifnot_rows_unique(df), 'The following rows are duplicates: 2')
})
