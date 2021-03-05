#' Calculate z/zlog Values for a data.frame
#'
#' Calculates the z or z(log) values for laboratory measurement standardisation
#' as proposed in Hoffmann 2017 et al. for a complete `data.frame`.
#'
#' @param x `data.frame`, with the columns: "age", `numeric`, "sex", `factor`
#' and more user defined `numeric` columns that should be z/z(log) transformed.
#' @param limits `data.frame`, reference table, has to have the columns:
#' "age", `numeric` (same units as in `age`, e.g. days or years, age of `0`
#' matches all ages),
#' "sex", `factor` (same levels for male and female as `sex` and a special level
#' `"both"`), "param", `character` with the laboratory parameter name that have
#' to match the column name in `x`, "lower" and "upper", `numeric` for the
#' lower and upper reference limits.
#' @param probs `numeric`, probabilities of the lower and upper reference limit,
#' default: `c(0.025, 0.975)` (spanning 95 %). Has to be of length 2 for
#' `numeric` or a two-column `matrix` with as many rows as elements in `x`.
#' @param log `logical`, should z (`log = FALSE`, default) or
#' z(log) (`log = TRUE`) calculated?
#'
#' @details
#' This is a wrapper function for [`z()`] and [`lookup_limits()`]. Please find
#' the details for the z/z(log) calculation at [`z()`].
#'
#' `zlog_df` is an alias for `z_df(..., log = TRUE)`.
#'
#' @return `data.frame`, with the columns: "age", "sex" and all `numeric`
#' columns z/zlog transformed. If a column name is missing in `limits$param`
#' a warning is thrown and the column is set to `NA`.
#' @rdname zlog_df
#' @author Sebastian Gibb
#' @references
#' Georg Hoffmann, Frank Klawonn, Ralf Lichtinghagen, and Matthias Orth.
#' 2017.
#' "The Zlog-Value as Basis for the Standardization of Laboratory Results."
#' LaboratoriumsMedizin 41 (1): 23–32.
#' \doi{10.1515/labmed-2016-0087}.
#' @seealso [`zlog()`]
#'
#' @export
#' @examples
#' l <- data.frame(
#'     param = c("alb", "bili"),
#'     age = c(0, 0),
#'     sex = c("both", "both"),
#'     units = c("mg/l", "µmol/l"),
#'     lower = c(35, 2),
#'     upper = c(52, 21)
#' )
#' x <- data.frame(
#'     age = 40:48,
#'     sex = rep(c("female", "male"), c(5, 4)),
#'     # from Hoffmann et al. 2017
#'     alb = c(42, 34, 38, 43, 50, 42, 27, 31, 24),
#'     bili = c(11, 9, 2, 5, 22, 42, 37, 200, 20)
#' )
#' z_df(x, l)
#'
z_df <- function(x, limits, probs = c(0.025, 0.975), log = FALSE) {
    limits <- .lookup_limits_df(x, limits)
    prms <- unique(rownames(limits))

    x[prms] <- z(as.matrix(x[prms]), limits, probs = probs, log = log)
    x
}

#' @rdname zlog_df
#' @export
#' @examples
#' zlog_df(x, l)
zlog_df <- function(x, limits, probs = c(0.025, 0.975)) {
    z_df(x, limits, probs, log = TRUE)
}
