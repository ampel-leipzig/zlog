test_that("set_missing_limits throws errors", {
    expect_error(set_missing_limits(1:10), ".*data.frame.*")
    expect_error(set_missing_limits(data.frame(foo = 1, bar = 2)), "columns")
    expect_error(
        set_missing_limits(data.frame(upper = 1, lower = 2), fraction = 1:3),
        "length 2"
    )
})

test_that("set_missing_limits works", {
    reftbl <- data.frame(
        param = c("foo1", "foo2", "bar1", "bar2"),
        lower = c(1, NA, 1, NA),
        upper = c(2, 2, NA, NA),
        stringsAsFactors = FALSE
    )
    r <- reftbl
    r$lower <- c(1, 0.3, 1, NA)
    r$upper <- c(2, 2, 20/3, NA)
    expect_equal(set_missing_limits(reftbl), r)

    r$lower <- c(1, 0.2, 1, NA)
    r$upper <- c(2, 2, 2, NA)
    expect_equal(set_missing_limits(reftbl, fraction = c(0.1, 2)), r)
})
