test_that("z throws errors", {
    expect_error(z())
    expect_error(z(1:3), "\"limits\" is missing")
    expect_error(z(1:3, limits = "foo"), "numeric")
    expect_error(z(1:3, limits = 1), "length 2")
    expect_error(z(1:3, limits = cbind(1:2, 1:2)), "number of rows")
    expect_error(z(1:3, limits = c(1, 2), probs = "foo"), "numeric")
    expect_error(z(1:3, limits = c(1, 2), probs = 1), "length 2")
    expect_error(z(1:3, limits = cbind(1:3, 1:3), probs = cbind(1:2, 1:2)),
                 "number of rows")
})

test_that("zlog throws errors", {
    expect_error(zlog())
    expect_error(zlog(1:3), "\"limits\" is missing")
    expect_error(zlog(1:3, limits = "foo"), "numeric")
    expect_error(zlog(1:3, limits = 1), "length 2")
    expect_error(zlog(1:3, limits = c(1, 2), probs = "foo"), "numeric")
    expect_error(zlog(1:3, limits = c(1, 2), probs = 1), "length 2")
})

test_that("z/zlog reproduce the results in Hoffmann et al. 2017", {
    # from Hoffmann et al. 2017
    alb <- c(42, 34, 38, 43, 50, 42, 27, 31, 24)
    albz <- c(-0.15, -2.25, -1.15, 0.08, 1.57, -0.15, -4.53, -3.16, -5.70)
    expect_equal(z(log(alb), limits = log(c(35, 52))), albz, tolerance = 0.002)
    expect_equal(zlog(alb, limits = c(35, 52)), albz, tolerance = 0.002)

    bili <- c(11, 9, 2, 5, 22, 42, 37, 200, 20)
    biliz <- c(0.85, 0.57,
               -1.96,
               # the zlog in Hoffmann et al. 2017 is -1.66 but seems to be wrong
               # cause the value 2 is exactly the lower limit which should be
               # the lower limit (-1.96)
               -0.43, 2.04, 3.12, 2.90, 5.72, 1.88)
    nalb <- length(alb)
    nbili <- length(bili)
    nn <- c(nalb, nbili)
    limits <- cbind(ll = rep(c(35, 2), nn), ul = rep(c(52, 21), nn))
    expect_equal(
        zlog(c(alb, bili), limits = limits), c(albz, biliz), tolerance = 0.005
    )
})

test_that("z/zlog handles a matrix as probs", {
    expect_equal(
        zlog(c(2, 5),
          limits = cbind(rep(2, 2), rep(5, 2)),
          probs = cbind(c(0.1, 0.2), c(0.9, 0.8))
        ),
        qnorm(c(0.1, 0.8))
    )
})
