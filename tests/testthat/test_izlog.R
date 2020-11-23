test_that("iz throws errors", {
    expect_error(iz())
    expect_error(iz(1:3), "\"limits\" is missing")
    expect_error(iz(1:3, limits = "foo"), "numeric")
    expect_error(iz(1:3, limits = 1), "length 2")
    expect_error(iz(1:3, limits = cbind(1:2, 1:2)), "number of rows")
    expect_error(iz(1:3, limits = c(1, 2), probs = "foo"), "numeric")
    expect_error(iz(1:3, limits = c(1, 2), probs = 1), "length 2")
    expect_error(iz(1:3, limits = cbind(1:3, 1:3), probs = cbind(1:2, 1:2)),
                 "number of rows")
})

test_that("izlog throws errors", {
    expect_error(izlog())
    expect_error(izlog(1:3), "\"limits\" is missing")
    expect_error(izlog(1:3, limits = "foo"), "numeric")
    expect_error(izlog(1:3, limits = 1), "length 2")
    expect_error(izlog(1:3, limits = c(1, 2), probs = "foo"), "numeric")
    expect_error(izlog(1:3, limits = c(1, 2), probs = 1), "length 2")
})

test_that("iz/izlog reproduce the results in Hoffmann et al. 2017", {
    # from Hoffmann et al. 2017
    albz <- c(-0.15, -2.25, -1.15, 0.08, 1.57, -0.15, -4.53, -3.16, -5.70)
    alb <- c(42, 34, 38, 43, 50, 42, 27, 31, 24)
    expect_equal(izlog(albz, limits = c(35, 52)), alb, tolerance = 0.002)

    biliz <- c(0.85, 0.57,
               -1.96,
               # the zlog in Hoffmann et al. 2017 is -1.66 but seems to be wrong
               # cause the value 2 is exactly the lower limit which should be
               # the lower limit (-1.96)
               -0.43, 2.04, 3.12, 2.90, 5.72, 1.88)
    bili <- c(11, 9, 2, 5, 22, 42, 37, 200, 20)
    nalb <- length(alb)
    nbili <- length(bili)
    nn <- c(nalb, nbili)
    limits <- cbind(ll = rep(c(35, 2), nn), ul = rep(c(52, 21), nn))
    expect_equal(
        izlog(c(albz, biliz), limits = limits), c(alb, bili), tolerance = 0.005
    )
})

test_that("iz/izlog is the inverse to z/zlog", {
    expect_equal(
        iz(
            z(c(2, 5), limits = c(2, 5), probs = c(0.1, 0.9)),
            limits = c(2, 5), probs = c(0.1, 0.9)
        ), c(2, 5)
    )
    expect_equal(
        izlog(
            zlog(c(2, 5), limits = c(2, 5), probs = c(0.1, 0.9)),
            limits = c(2, 5), probs = c(0.1, 0.9)
        ), c(2, 5)
    )
})
