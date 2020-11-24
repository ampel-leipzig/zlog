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
#' limits. Additional columns are allowed.
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

    if (!all(c("age", "sex", "lower", "upper") %in% colnames(table)))
        stop("'table' has to have the columns: ",
             "\"age\", \"sex\", \"upper\", \"lower\".")

    table$sex <- as.factor(table$sex)
    if (nlevels(table$sex) > 3)
        stop("'table$sex' has to be a factor of at most 3 levels ",
             "(male, female, both).")

    table <- table[order(table$age, decreasing = TRUE),]

    limits <- matrix(
        NA_real_, nrow = length(age), ncol = 2L,
        dimnames = list(NULL, c("lower", "upper"))
    )

    for (i in seq(along = age)) {
        j <- which(
            (sex[i] == table$sex | table$sex == "both") &
                age[i] >= table$age
        )[1L]
        if (length(j))
            limits[i, ] <- c(table$lower[j], table$upper[j])
    }
    limits
}
