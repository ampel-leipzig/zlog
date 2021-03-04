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

test_that("lookup_limits works for single parameter", {
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

test_that("lookup_limits works for multiple parameters", {
    reftbl <- data.frame(
        age = c(0, 3, 9, 0, 3, 9, 0),
        sex = c(rep("both", 3), rep("f", 3), "m"),
        param = rep(c("foo", "bar"), c(3, 4)),
        lower = c(1:7),
        upper = c(4:10)
    )
    limits <- cbind(
        lower = c(1, 1, 2, 3, 3, 4, 4, 5, 7, 7),
        upper = c(4, 4, 5, 6, 6, 7, 7, 8, 10, 10)
    )
    rownames(limits) <- rep(c("foo", "bar"), c(5, 5))
    expect_equal(
        lookup_limits(
            age = c(0, 1, 5, 9, 25), sex = rep(c("f", "m"), 3:2), reftbl
        ),
        limits
    )

    # bug in zlog 0.0.13 (rev 25527b2), lookup_limits resorts param depending on
    # age
    reftbl <- data.frame(
        age = 1:2,
        sex = rep("both", 2),
        param = c("bar", "foo"),
        lower = 1:2,
        upper = 3:4
    )
    limits <- cbind(lower = 1:2, upper = 3:4)
    rownames(limits) <- reftbl$param

    expect_equal(lookup_limits(age = 10, sex = "f", reftbl), limits)
})
