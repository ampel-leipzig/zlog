Dear Julia Haider, dear CRAN-maintainers,

thanks for reviewing my package.

In the following I like to address your review comments:

Quoting Julia Haider (2021-04-29 13:33:14)
> Please only capitalize sentence beginnings and names in the description
> text. e.g. Laboratory Measurements into z ...  --> laboratory
> measurements into z ...

Done.

> Please add \value to .Rd files regarding exported methods and explain
> the functions results in the documentation. Please write about the
> structure of the output (class) and also what the output means. (If a
> function does not return a value, please document that too, e.g.
> \value{No return value, called for side effects} or similar)
> Missing Rd-tags:
>       impute_df.Rd: \value

Done.

> Please always make sure to reset to user's options(), working directory
> or par() after you changed it in examples and vignettes and demos.
> e.g.: inst/doc/zlog.R
> oldpar <- par(mar = c(0, 0, 0, 0))
> ...
> par(oldpar)

Done.

Best wishes,

Sebastian
