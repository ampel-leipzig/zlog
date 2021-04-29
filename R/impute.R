#' Imputation
#'
#' Impute `NA` values with the logmean, mean, minimal or maximum reference value.
#'
#' @param x `data.frame`, with the columns: "age", `numeric`, "sex", `factor`
#' and more user defined `numeric` columns that should be imputed.
#' @param limits `data.frame`, reference table, has to have the columns:
#' "age", `numeric` (same units as in `age`, e.g. days or years, age of `0`
#' matches all ages),
#' "sex", `factor` (same levels for male and female as `sex` and a special level
#' `"both"`), "param", `character` with the laboratory parameter name that have
#' to match the column name in `x`, "lower" and "upper", `numeric` for the
#' lower and upper reference limits.
#' @param method `character`, imputation method. `method = "logmean"` (default)
#' replaces all `NA` with its corresponding logged mean values for the reference
#' table `limits` (for subsequent use of the *zlog* score,
#' use `method = "mean" for *z* score calculation).
#' For `method = "min"` or `method = "max"` the lower or the upper limits are
#' used.
#'
#' @note
#' Imputation should be done prior to [`z()`]/[`zlog()`] transformation.
#' Afterwards the `NA` could replaced by zero (for mean-imputation) via
#' `d[is.na(d)] <- 0`.
#' @return `data.frame`, the same as `x` but missing values are replaced by
#' the corresponding logmean, mean, minimal or maximal reference values
#' depending on the chosen `method`.
#'
#' @author Sebastian Gibb
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
#'     alb = c(42, NA, 38, NA, 50, 42, 27, 31, 24),
#'     bili = c(11, 9, NA, NA, 22, 42, NA, 200, 20)
#' )
#' impute_df(x, l)
#' impute_df(x, l, method = "min")
#' zlog_df(impute_df(x, l), l)
impute_df <- function(x, limits, method = c("logmean", "mean", "min", "max")) {
    limits <- .lookup_limits_df(x, limits)
    prms <- unique(rownames(limits))

    v <- switch(
        match.arg(method),
        "min" = limits[, "lower"],
        "max" = limits[, "upper"],
        "mean" = (limits[, "lower"] + limits[, "upper"]) / 2L,
        "logmean" = exp(log(limits[, "lower"] * limits[, "upper"]) / 2L)
    )
    na <- is.na(x[prms])
    x[prms][na] <- v[na]
    x
}
