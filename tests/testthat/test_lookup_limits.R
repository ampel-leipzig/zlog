test_that("lookup_limits throws errors", {
    expect_error(lookup_limits(), "missing")
    expect_error(lookup_limits(age = "foo"), "numeric")
    expect_error(lookup_limits(age = 1:3, sex = 1:2), "length")
    expect_error(lookup_limits(age = 1:3, sex = 1:3), "2 levels")
    expect_error(
        lookup_limits(age = 1:2, sex = 1:2, table = data.frame(foo = 1)),
        "columns"
    )
    expect_error(
        lookup_limits(
            age = 1:2, sex = 1:2,
            table = data.frame(age = 1, sex = 1:4, lower = 1, upper = 2)
        ),
        "3 levels"
    )
})

test_that("lookup_limits works", {
    reftbl <- data.frame(
        age = c(0, 3, 9), sex = "both", lower = 1:3, upper = 4:6
    )
    limits <- cbind(lower = c(1, 1, 2, 3, 3), upper = c(4, 4, 5, 6, 6))
    expect_equal(
        lookup_limits(
            age = c(0, 1, 5, 9, 25), sex = rep(c("f", "m"), 3:2), reftbl
        ),
        limits
    )
    expect_equal(
        lookup_limits(
            age = c(0, 1, 5, 9, 25), sex = rep(c("f", "m"), 3:2), reftbl[3:1,]
        ),
        limits
    )

    reftbl <- data.frame(
        age = c(0, 5, 9), sex = c("both", "f", "f"), lower = 1:3, upper = 4:6
    )
    limits <- cbind(lower = c(1, 1, 2, 1, 1), upper = c(4, 4, 5, 4, 4))
    expect_equal(
        lookup_limits(
            age = c(0, 1, 5, 9, 25), sex = rep(c("f", "m"), 3:2), reftbl
        ),
        limits
    )
})
