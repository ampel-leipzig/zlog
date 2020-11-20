test_that("reference_limits throws errors", {
    expect_error(reference_limits())
    expect_error(reference_limits("foo"))
    expect_error(reference_limits(1:3, probs = "foo"), "numeric")
    expect_error(reference_limits(1:3, probs = 1), "length 2")
    expect_error(reference_limits(1:3, probs = c(-1, 1)), "outside")
    expect_error(reference_limits(1:3, probs = c(0, 2)), "outside")
    expect_error(reference_limits(1:3, probs = c(2, 0)), "outside")
    expect_error(reference_limits(1:3, na.rm = "foo"), "interpretable")
})

test_that("reference_limits works", {
    rl <- setNames(quantile(1:3, probs = c(0.025, 0.975)), c("lower", "upper"))
    expect_identical(reference_limits(1:3), rl)
    expect_identical(reference_limits(c(NA, 1:3, NaN)), rl)
    expect_identical(
        reference_limits(1:3, probs = c(0.25, 0.75)),
        setNames(quantile(1:3, probs = c(0.25, 0.75)), c("lower", "upper"))
    )
})
