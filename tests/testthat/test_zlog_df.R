test_that("z_df throws errors", {
    expect_error(z_df(1:10), "data.frame")
    expect_error(
        z_df(data.frame(alb = 1, bili = 2), data.frame()),
        ".*age.* .* missing"
    )
    expect_error(
        z_df(data.frame(age = 1, alb = 1, bili = 2), data.frame()),
        ".*sex.* .* missing"
    )
    expect_error(
        z_df(data.frame(age = 1, sex = "f", alb = 1, bili = 2), 1),
        ".*data.frame.*"
    )
    expect_error(
        z_df(data.frame(age = 1, sex = "f", alb = 1, bili = 2), data.frame()),
        "with the following columns: "
    )

})

test_that("z_df/zlog_df", {
    l <- data.frame(
        param = c("alb", "bili", "hbg"),
        age = c(0, 0, 0),
        sex = c("both", "both", "both"),
        units = c("mg/l", "µmol/l", "mmol/l"),
        lower = c(35, 2, 8),
        upper = c(52, 21, 12)
    )
    x <- data.frame(
        age = 40:48,
        sex = rep(c("female", "male"), c(5, 4)),
        # from Hoffmann et al. 2017
        alb = c(42, 34, 38, 43, 50, 42, 27, 31, 24),
        bili = c(11, 9, 2, 5, 22, 42, 37, 200, 20)
    )
    r <- data.frame(
        age = 40:48,
        sex = rep(c("female", "male"), c(5, 4)),
        alb = zlog(x$alb, limits = c(35, 52)),
        bili = zlog(x$bili, limits = c(2, 21))
    )
    expect_equal(zlog_df(x, l), r)

    xw <- cbind(x, wbc = 8, alat = 0.5)
    rw <- cbind(r, wbc = NA_real_, alat = NA_real_)
    expect_warning(w <- zlog_df(xw, l), "wbc, alat")
    expect_equal(w, rw)

    # bug in zlog 0.0.13 (rev 25527b2), zlog_df ignores order of lookup_limits
    # results
    l2 <- l[c(2, 3, 1),]
    rownames(l2) <- NULL
    expect_equal(zlog_df(x, l2), r)
})
