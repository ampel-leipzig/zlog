#' Lookup Limits in Reference Tables
#'
#' Reference limits are specific for age and sex. Each laboratory institute has
#' its own reference limits. This function is useful to query a dataset against
#' this database.
#'
#' @param age `numeric`, patient age.
#' @param sex `character`/`factor`, patient sex, has to be the same length as
#' `age`.
#' @param table `data.frame`, reference table, has to have the columns:
#' "age", `numeric` (same units as in `age`, e.g. days or years, age of `0`
#' matches all ages),
#' "sex", `factor` (same levels for male and female as `sex` and a special level
#' `"both"`), "lower" and "upper", `numeric` for the lower and upper reference
#' limits. If an "param" column is given the "lower" and "upper" limits for all
#' different values in "param" is returned. Additional columns are allowed (and
#' ignored).
#'
#' @return `matrix`, with 2 columns ("lower", "upper") and as many rows as
#' `length(age)`.
#' @author Sebastian Gibb
#' @importFrom stats setNames quantile
#' @export
#' @examples
#' reference <- data.frame(
#'     param = c("albumin", rep("bilirubin", 4)),
#'     age = c(0, 1, 2, 3, 7),     # days
#'     sex = "both",
#'     units = c("g/l", rep("µmol/l", 4)), # ignored
#'     lower = c(35, rep(NA, 4)),  # no real reference values
#'     upper = c(52, 5, 8, 13, 18) # no real reference values
#' )
#'
#' # lookup albumin reference values for 18 year old woman
#' lookup_limits(
#'     age = 18 * 365.25,
#'     sex = "female",
#'     table = reference[reference$param == "albumin",]
#' )
#'
#' # lookup albumin and bilirubin values for 18 year old woman
#' lookup_limits(
#'     age = 18 * 365.25,
#'     sex = "female",
#'     table = reference
#' )
#'
#' # lookup bilirubin referenc values for infants
#' lookup_limits(
#'     age = 0:8,
#'     sex = rep(c("female", "male"), 5:4),
#'     table = reference[reference$param == "bilirubin",]
#' )
lookup_limits <- function(age, sex, table) {
    if (!is.numeric(age))
        stop("'age' has to be a numeric value.")
    if (length(age) != length(sex))
        stop("'age' and 'sex' have to be of the same length.")

    sex <- as.factor(sex)
    if (nlevels(sex) > 2)
        stop("'sex' has to be a factor of at most 2 levels ",
             "(male, female).")

    cn <- colnames(table) <- tolower(colnames(table))
    if (!all(c("age", "sex", "lower", "upper") %in% cn))
        stop("'table' has to have the columns: ",
             "\"age\", \"sex\", \"upper\", \"lower\".")

    table$sex <- as.factor(table$sex)
    if (nlevels(table$sex) > 3)
        stop("'table$sex' has to be a factor of at most 3 levels ",
             "(male, female, both).")

    if (!"param" %in% cn)
        table$param <- "param"

    params <- unique(table$param)
    nparam <- length(params)
    nage <- length(age)

    table <- table[order(table$age, decreasing = TRUE),]

    limits <- matrix(
        NA_real_, nrow = nage * nparam, ncol = 2L,
        dimnames = list(
                if (nparam > 1L || params != "param")
                    rep(params, each = nage)
                else
                    NULL,
                c("lower", "upper")
        )
    )

    for (i in seq(along = params)) {
        for (j in seq(along = age)) {
            k <- which(
                (sex[j] == table$sex | table$sex == "both") &
                age[j] >= table$age & params[i] == table$param
            )[1L]
            if (length(k))
                limits[(i - 1) * nage + j, ] <-
                    c(table$lower[k], table$upper[k])
        }
    }

    limits
}

#' Lookup Limits in Reference Tables for a Whole data.frame
#'
#' internal function
#'
#' @param x `data.frame`
#' @param table `data.frame`, reference table, format see above
#' @return see above
#'
#' @noRd
.lookup_limits_df <- function(x, limits) {
    if (!is.data.frame(x)) {
        stop("'x' has to be a data.frame.")
    }

    cnx <- colnames(x)
    cnxl <- tolower(cnx)

    if (!"age" %in% cnxl)
        stop("Column \"age\" is missing in 'x'.")
    if (!"sex" %in% cnxl)
        stop("Column \"sex\" is missing in 'x'.")


    if (!is.data.frame(limits))
        stop("'limits' has to be a data.frame.")

    cnl <- colnames(limits) <- tolower(colnames(limits))

    if (!all(c("age", "sex", "param", "lower", "upper") %in% cnl))
        stop("'limits' has to be a data.frame with the following columns: ",
             "\"age\", \"sex\", \"param\", \"upper\", \"lower\".\n",
             "See '?lookup_limits' for details.")

    num <- vapply(x, is.numeric, FALSE, USE.NAMES = FALSE) &
        !cnxl %in% c("age", "sex")

    if (any(!cnx[num] %in% limits$param)) {
        na <- cnx[num & (!cnx %in% limits$param)]
        warning(
            "No reference for column(s): ", paste0(na, collapse = ", ")
        )
        ina <- nrow(limits) + seq_len(length(na))
        limits[ina, c("param", "age", "sex")] <-
            cbind.data.frame(param = na, age = 0L, sex = "both")
    }

    lookup_limits(
        age = x[[which(cnxl == "age")]], sex = x[[which(cnxl == "sex")]],
        table = limits[limits$param %in% cnx[num],]
    )
}
