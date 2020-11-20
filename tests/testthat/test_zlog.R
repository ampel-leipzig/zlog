test_that("z throws errors", {
    expect_error(z())
    expect_error(z(1:3))
    expect_error(z(1:3, limits = "foo"), "numeric")
    expect_error(z(1:3, limits = 1), "length 2")
    expect_error(z(1:3, limits = c(1, 2), probs = "foo"), "numeric")
    expect_error(z(1:3, limits = c(1, 2), probs = 1), "length 2")
})

test_that("z", {
    # from Hoffmann et al. 2017
    alb <- c(42, 34, 38, 43, 50, 42, 27, 31, 24)
    albz <- c(-0.15, -2.25, -1.15, 0.08, 1.57, -0.15, -4.53, -3.16, -5.70)
    expect_equal(z(log(alb), limits = log(c(35, 52))), albz, tolerance = 0.002)
    expect_equal(zlog(alb, limits = c(35, 52)), albz, tolerance = 0.002)
})
