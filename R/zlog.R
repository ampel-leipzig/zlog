#' Calculate z/zlog Values
#'
#' Calculates the z or z(log) values for laboratory measurement standardisation
#' as proposed in Hoffmann 2017 et al.
#'
#' @param x `numeric`, laboratory values.
#' @param limits `numeric` or `matrix`, lower and upper reference limits. Has to
#' be of length 2 for `numeric` or a two-column `matrix` with as many rows as
#' elements in `x`.
#' @param probs `numeric`, probabilities of the lower and upper reference limit,
#' default: `c(0.025, 0.975)` (spanning 95 %). Has to be of length 2 for
#' `numeric` or a two-column `matrix` with as many rows as elements in `x`.
#' @param log `logical`, should z (`log = FALSE`, default) or
#' z(log) (`log = TRUE`) calculated?
#'
#' @details
#' The z value is calculated as follows (assuming that the limits where 0.025
#' and 0.975 quantiles):
#' \eqn{z = (x - (limits_1 + limits_2 )/2) * 3.92/(limits_2 - limits_1)}.
#'
#' The z(log) value is calculated as follows (assuming that the limits where 0.025
#' and 0.975 quantiles):
#' \eqn{z = (\log(x) - (\log(limits_1) + \log(limits_2))/2) * 3.92/(\log(limits_2) - \log(limits_1))}.
#'
#' `zlog` is an alias for `z(..., log = TRUE)`.
#'
#' @return `numeric`, z or z(log) values.
#' @rdname zlog
#' @author Sebastian Gibb
#' @references
#' Georg Hoffmann, Frank Klawonn, Ralf Lichtinghagen, and Matthias Orth.
#' 2017.
#' "The Zlog-Value as Basis for the Standardization of Laboratory Results."
#' LaboratoriumsMedizin 41 (1): 23–32.
#' \doi{10.1515/labmed-2016-0087}.
#' @seealso [`izlog()`]
#'
#' @importFrom stats qnorm
#' @export
#' @examples
#' z(1:10, limits = c(2, 8))
#'
z <- function(x, limits, probs = c(0.025, 0.975), log = FALSE) {
    if (!(is.numeric(limits) && length(limits) == 2L) &&
        !(is.matrix(limits) && mode(limits) == "numeric" &&
          nrow(limits) == length(x) && ncol(limits) == 2L))
        stop("'limits' has to be a numeric of length 2, or a ",
             "matrix with 2 columns (lower and upper limit) where ",
             "the number of rows equals the length of 'x'.")

    if (!is.matrix(limits))
        limits <- t(limits)

    if (!(is.numeric(probs) && length(probs) == 2L) &&
        !(is.matrix(probs) && mode(probs) == "numeric" &&
          nrow(probs) == length(x) && ncol(probs) == 2L))
        stop("'probs' has to be a numeric of length 2, or a ",
             "matrix with 2 columns (lower and upper limit) where ",
             "the number of rows equals the length of 'x'.")

    if (!is.matrix(probs))
        probs <- t(probs)

    if (log) {
        x <- log(x)
        limits <- log(limits)
    }

    m <- (limits[, 1L] + limits[, 2L]) / 2L
    s <- abs(limits[, 2L] - limits[, 1L]) / rowSums(abs(qnorm(probs)))

    (x - m) / s
}

#' @rdname zlog
#' @export
#' @examples
#' # from Hoffmann et al. 2017
#' albumin <- c(42, 34, 38, 43, 50, 42, 27, 31, 24)
#' zlog(albumin, limits = c(35, 52))
#'
#' bilirubin <- c(11, 9, 2, 5, 22, 42, 37, 200, 20)
#'
#' limits <- cbind(
#'     lower = rep(c(35, 2), c(length(albumin), length(bilirubin))),
#'     upper = rep(c(52, 21), c(length(albumin), length(bilirubin)))
#' )
#' zlog(c(albumin, bilirubin), limits = limits)
zlog <- function(x, limits, probs = c(0.025, 0.975)) {
    z(x, limits, probs, log = TRUE)
}
