#' Calculate z/zlog values
#'
#' Calculates the z or z(log) values for laboratory measurement standardisation
#' as proposed in Hoffmann 2017 et al.
#'
#' @param x `numeric`, laboratory values
#' @param limits `numeric`, lower and upper reference limits.
#' @param probs `numeric`, probabilities of the lower and upper reference limit,
#' default: `c(0.025, 0.975)` (spanning 95 %).
#'
#' @details
#' The z values is calculated as follows (assuming that the limits where 0.025
#' and 0.975 quantiles):
#' \eqn{z = (x - (limits_1 + limits_2 )/2) * 3.92/(limits_2 - limits_1)}.
#'
#' @return `numeric`, z or z(log) values.
#' @rdname zlog
#' @author Sebastian Gibb
#' @importFrom stats qnorm
#' @export
#' @examples
#' z(1:10, limits = c(2, 8))
#'
z <- function(x, limits, probs = c(0.025, 0.975)) {
    if (!is.numeric(limits) || length(limits) != 2L)
        stop("'limits' has to be a numeric of length 2.")

    if (!is.numeric(probs) || length(probs) != 2L)
        stop("'probs' has to be a numeric of length 2.")

    m <- (limits[1L] + limits[2L]) / 2L
    s <- abs(limits[2L] - limits[1L]) / sum(abs(qnorm(range(probs))))

    (x - m) / s
}

#' @rdname zlog
#' @export
#' @examples
#' # from Hoffmann et al. 2017
#' albumin <- c(42, 34, 38, 43, 50, 42, 27, 31, 24)
#' zlog(albumin, limits = c(35, 52))
zlog <- function(x, limits, probs = c(0.025, 0.975)) {
    if (!is.numeric(limits) || length(limits) != 2L)
        stop("'limits' has to be a numeric of length 2.")

    z(log(x), limits = log(limits), probs = probs)
}
