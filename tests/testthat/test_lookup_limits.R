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

test_that(".lookup_limits_df throws errors", {
    expect_error(.lookup_limits_df(1:10), "data.frame")
    expect_error(
        .lookup_limits_df(data.frame(alb = 1, bili = 2), data.frame()),
        ".*age.* .* missing"
    )
    expect_error(
        .lookup_limits_df(data.frame(age = 1, alb = 1, bili = 2), data.frame()),
        ".*sex.* .* missing"
    )
    expect_error(
        .lookup_limits_df(data.frame(age = 1, sex = "f", alb = 1, bili = 2), 1),
        ".*data.frame.*"
    )
    expect_error(
        .lookup_limits_df(
            data.frame(age = 1, sex = "f", alb = 1, bili = 2),
            data.frame()
        ),
        "with the following columns: "
    )

})

test_that(".lookup_limits_df works", {
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
    r <- matrix(
        rep(c(35, 2, 52, 21), each = 9), nrow = 18,
        dimnames = list(rep(c("alb", "bili"), each = 9), c("lower", "upper"))
    )
    expect_equal(.lookup_limits_df(x, l), r)

    xw <- cbind(x, wbc = 8, alat = 0.5)
    rw <- r[]
    rw[] <- NA_real_
    rownames(rw) <- rep(c("wbc", "alat"), each = 9)
    rw <- rbind(r, rw)
    expect_warning(w <- .lookup_limits_df(xw, l), "wbc, alat")
    expect_equal(w, rw)

    # bug in zlog 0.0.13 (rev 25527b2), lookup_limits resorts param depending on
    # age
    l2 <- l[c(2, 3, 1),]
    rownames(l2) <- NULL
    expect_equal(.lookup_limits_df(x, l2), r[rev(seq_len(nrow(r))),])

    # x with uppercase Age/Sex columns
    xu <- x
    colnames(xu)[1:2] <- c("Age", "Sex")
    expect_equal(.lookup_limits_df(xu, l), r)

    # l with uppercase Age/Sex columns
    lu <- l
    colnames(lu)[2:3] <- c("Age", "Sex")
    expect_equal(.lookup_limits_df(x, lu), r)

    # bug caused by case-insensitivity if params are missing
    expect_equal(
        suppressWarnings(.lookup_limits_df(xu, lu[-1,])),
        rbind(
            r[10:18,],
            matrix(NA, nrow = 9, ncol = 2,
                   dimnames = list(rep.int("alb", 9), c("lower", "upper")))))

    # bug caused by case-insensitivity if params are upper case
    colnames(xu) <- toupper(colnames(xu))
    lu$param <- toupper(lu$param)
    ru <- r
    rownames(ru) <- toupper(rownames(ru))
    expect_equal(.lookup_limits_df(xu, lu), ru)
})
