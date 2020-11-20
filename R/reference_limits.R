#' Calculate Reference Limits
#'
#' Calculates the lower and upper reference limit for given probabilities.
#'
#' @param x `numeric`, laboratory values
#' @param probs `numeric`, probabilities of the lower and upper reference limit,
#' default: `c(0.025, 0.975)` (spanning 95 %).
#' @param na.rm `logical`, if `TRUE` (default) `NA` values are removed before
#' the reference limits are calculated.
#'
#' @return `numeric` of length 2 with the lower and upper limit.
#'
#' @author Sebastian Gibb
#' @importFrom stats setNames quantile
#' @export
#' @examples
#' reference_limits(1:10)
reference_limits <- function(x, probs = c(0.025, 0.975), na.rm = TRUE) {
    if (!is.numeric(probs) || length(probs) != 2L)
        stop("'probs' has to be a numeric of length 2.")

    setNames(
        quantile(x, probs = range(probs), na.rm = na.rm, names = FALSE),
        c("lower", "upper")
    )
}
