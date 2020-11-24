test_that("zcol", {
    expect_error(zcol("foo"))
    expect_identical(zcol(c(0, 10)), c("#FEFCFA", "#F57800"))
    expect_identical(zcol(NA), c("#FEFCFA"))
})

test_that(".zlogF", {
    expect_identical(.zlogF(-2, 255), .zlogF(2, 255))
    expect_equal(.zlogF(0, 255), 4.59, tolerance = 0.01)
    expect_equal(.zlogF(11, 255), 255, tolerance = 0.01)
    expect_equal(.zlogF(-11, 120), 120, tolerance = 0.01)
})
