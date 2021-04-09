test_that("impute_df throws errors", {
    expect_error(impute_df(1:10), "data.frame")
    expect_error(
        impute_df(data.frame(alb = 1, bili = 2), data.frame()),
        ".*age.* .* missing"
    )
    expect_error(
        impute_df(data.frame(age = 1, alb = 1, bili = 2), data.frame()),
        ".*sex.* .* missing"
    )
    expect_error(
        impute_df(data.frame(age = 1, sex = "f", alb = 1, bili = 2), 1),
        ".*data.frame.*"
    )
    expect_error(
        impute_df(data.frame(age = 1, sex = "f", alb = 1, bili = 2), data.frame()),
        "with the following columns: "
    )
})

test_that("impute_df", {
    l <- data.frame(
        param = c("alb", "alb", "bili", "bili", "hbg"),
        age = c(0, 18, 0, 0, 0),
        sex = c("both", "both", "f", "m", "both"),
        units = c("mg/l", "µmol/l", "mmol/l", "mmol/l", "mmol/l"),
        lower = c(30, 35, 2, 4, 8),
        upper = c(50, 52, 21, 24, 12)
    )
    x <- data.frame(
        age = rep(17:19, each = 2),
        sex = rep(c("f", "m"), c(4, 2)),
        alb = c(42, NA, 42, NA, 50, NA),
        bili = c(18, NA, 18, 19, 20, NA)
    )
    rmin <- rmax <- rmean <- rlmean <- x
    naa <- c(2, 4, 6)
    nab <- c(2, 6)
    rmin$alb[naa] <- c(30, 35, 35)
    rmin$bili[nab] <- c(2, 4)
    rmax$alb[naa] <- c(50, 52, 52)
    rmax$bili[nab] <- c(21, 24)
    rmean$alb[naa] <- (rmin$alb[naa] + rmax$alb[naa]) / 2
    rmean$bili[nab] <- (rmin$bili[nab] + rmax$bili[nab]) / 2
    rlmean$alb[naa] <- exp(log(rmin$alb[naa] * rmax$alb[naa]) / 2)
    rlmean$bili[nab] <- exp(log(rmin$bili[nab] * rmax$bili[nab]) / 2)

    expect_equal(impute_df(x, l), rlmean)
    expect_equal(impute_df(x, l, method = "min"), rmin)
    expect_equal(impute_df(x, l, method = "mean"), rmean)
    expect_equal(impute_df(x, l, method = "max"), rmax)
})
