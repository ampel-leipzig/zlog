#' Set Missing Limits in Reference Tables
#'
#' Sometimes reference limits are not specified. That is often the case for
#' biomarkers that are related to infection or cancer. Using zero as lower
#' boundary results in skewed distributions (Hoffmann et al. 2017; fig. 7).
#' Haeckel et al. 2015 suggested to set the lower reference limit to 0.15 of
#' the upper one.
#'
#' @param x `data.frame`, reference table, has to have the columns:
#' "lower" and "upper", `numeric` for the lower and upper reference
#' limits.  Additional columns are allowed (and ignored).
#' @param fraction `numeric(2)`, targeted fraction of the lower to the upper and
#' the upper to the lower limit. Haeckel et al. 2015 suggested to set the lower
#' limit to 0.15 of the upper one. We choose 20/3 (the reciprocal of 0.15) for
#' the upper to the lower one.
#'
#' @return `data.frame`, the same as `x` but the "lower" and "upper" columns are
#' modified if there were `NA` before.
#' @author Sebastian Gibb
#' @references
#' Georg Hoffmann, Frank Klawonn, Ralf Lichtinghagen, and Matthias Orth.
#' 2017.
#' "The Zlog-Value as Basis for the Standardization of Laboratory Results."
#' LaboratoriumsMedizin 41 (1): 23–32.
#' \doi{10.1515/labmed-2016-0087}.
#'
#' Rainer Haeckel, Werner Wosniok, Ebrhard Gurr and Burkhard Peil.
#' 2015.
#' "Permissible limits for uncertainty of measurement in laboratory medicine"
#' Clinical Chemistry and Laboratory Medicine 53 (8): 1161-1171.
#' \doi{10.1515/cclm-2014-0874}.
#' @export
#' @examples
#' reference <- data.frame(
#'     param = c("albumin", rep("bilirubin", 4)),
#'     age = c(0, 1, 2, 3, 7),             # ignored
#'     sex = "both",                       # ignored
#'     units = c("g/l", rep("µmol/l", 4)), # ignored
#'     lower = c(35, rep(NA, 4)),  # no real reference values
#'     upper = c(52, 5, 8, 13, 18) # no real reference values
#' )
#' set_missing_limits(reference)
#' set_missing_limits(reference, fraction = c(0.2, 5))
set_missing_limits <- function(x, fraction = c(0.15, 20/3)) {
    if (!is.data.frame(x))
        stop("'x' has to be a data.frame")

    if (!all(c("lower", "upper") %in% colnames(x)))
        stop("'x' has to have the columns: \"upper\", \"lower\".")

    if (!is.numeric(fraction) || length(fraction) != 2L)
        stop("'fraction' has to be a numeric of length 2")

    na <- is.na(x[c("lower", "upper")])
    x$lower[na[, "lower"]] <- x$upper[na[, "lower"]] * fraction[1L]
    x$upper[na[, "upper"]] <- x$lower[na[, "upper"]] * fraction[2L]
    x
}
