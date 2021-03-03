#' Calculate Laboratory Measurements from z/zlog Values
#'
#' Inverse function to z or z(log) for laboratory measurement standardisation
#' as proposed in Hoffmann 2017 et al.
#'
#' @param x `numeric`, z/zlog values.
#' @param limits `numeric` or `matrix`, lower and upper reference limits. Has to
#' be of length 2 for `numeric` or a two-column `matrix` with as many rows as
#' elements in `x`.
#' @param probs `numeric`, probabilities of the lower and upper reference limit,
#' default: `c(0.025, 0.975)` (spanning 95 %). Has to be of length 2 for
#' `numeric` or a two-column `matrix` with as many rows as elements in `x`.
#'
#' @details
#' The inverse z value is calculated as follows (assuming that the limits where
#' 0.025 and 0.975 quantiles):
#' \eqn{x = z * (limits_2 - limits_1)/3.92 + (limits_1 + limits_2)/2}
#'
#' The inverse z(log) value is calculated as follows (assuming that the limits
#' where 0.025 and 0.975 quantiles):
#' \eqn{x = \exp(z * (\log(limits_2) - \log(limits_1))/3.92 + (\log(limits_1) + \log(limits_2))/2)}
#'
#' @return `numeric`, laboratory measurements.
#' @rdname izlog
#' @author Sebastian Gibb
#' @references
#' Georg Hoffmann, Frank Klawonn, Ralf Lichtinghagen, and Matthias Orth.
#' 2017.
#' "The Zlog-Value as Basis for the Standardization of Laboratory Results."
#' LaboratoriumsMedizin 41 (1): 23–32.
#' \doi{10.1515/labmed-2016-0087}.
#' @seealso [`zlog()`]
#'
#' @importFrom stats qnorm
#' @export
#' @examples
#' iz(z(1:10, limits = c(2, 8)), limits = c(2, 8))
#'
iz <- function(x, limits, probs = c(0.025, 0.975)) {
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

    m <- (limits[, 1L] + limits[, 2L]) / 2L
    s <- abs(limits[, 2L] - limits[, 1L]) / rowSums(abs(qnorm(probs)))
    x * s + m
}

#' @rdname izlog
#' @export
#' @examples
#' # from Hoffmann et al. 2017
#' albuminzlog <- c(-0.15, -2.25, -1.15, 0.08, 1.57, -0.15, -4.53, -3.16, -5.70)
#' izlog(albuminzlog, limits = c(35, 52))
#'
#' bilirubinzlog <- c(0.85, 0.57, -1.96, -0.43, 2.04, 3.12, 2.90, 5.72, 1.88)
#'
#' limits <- cbind(
#'     lower = rep(c(35, 2), c(length(albuminzlog), length(bilirubinzlog))),
#'     upper = rep(c(52, 21), c(length(albuminzlog), length(bilirubinzlog)))
#' )
#' izlog(c(albuminzlog, bilirubinzlog), limits = limits)
izlog <- function(x, limits, probs = c(0.025, 0.975)) {
    if (missing(limits))
        stop("argument \"limits\" is missing, with no default")
    exp(iz(x, log(limits), probs))
}
