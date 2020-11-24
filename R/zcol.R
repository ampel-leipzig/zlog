#' Z(log) Depending Color
#'
#' This function provides a color gradient depending on the zlog value as
#' described in Hoffmann et al. 2017. The colours are only roughly equal to the
#' one found in the article.
#'
#' @param x `numeric`, z(log) value.
#' @return `character`, of `length(x)` with the corresponding color hex code.
#'
#' @author Sebastian Gibb
#' @references
#' Hoffmann, Georg, Frank Klawonn, Ralf Lichtinghagen, and Matthias Orth.
#' 2017.
#' "The Zlog-Value as Basis for the Standardization of Laboratory Results."
#' LaboratoriumsMedizin 41 (1): 23–32. \doi{10.1515/labmed-2016-0087}.
#' @importFrom graphics image text
#' @importFrom grDevices rgb
#' @export
#' @examples
#' z <- -10:10
#' image(matrix(z, ncol = 1), col = zcol(z), xaxt = "n", yaxt = "n")
#' text(seq(0, 1, length.out=length(z)), 0, label = z)
zcol <- function(x) {
    x[is.na(x)] <- 0

    ## the colour values are picked from the PDF version of Hoffmann et al. 2017
    ## and may be incorrect cause of jpeg artefacts
    rgb(
        red = 255 - .zlogF(x, ifelse(x < 0, 185, 10)),
        green = 255 - .zlogF(x, 135),
        blue = 255 - .zlogF(x, ifelse(x < 0, 70, 255)),
        maxColorValue = 255
    )
}

#' Map zlog Value to 0:255
#'
#' Similar to the `F` described in Hoffmann et al. 2017.
#'
#' @param x `numeric`, zlog value.
#' @param d `numeric`, scale value.
#'
#' @return `numeric`, of `length(x)` in the range `0:255`.
#' @noRd
.zlogF <- function(x, d) {
    # the proposed function in Hoffmann et al. 2017 doesn't increase enough,
    # so we add 4 here
    # the color range is white:color for 0:10, 0:1 should be white, 2 should
    # have a clear visible color, d / 1 + exp(4 - abs(x)) is in the range from
    # 4.6:254.4 for x = 0:10 and d = 255
    d / (1 + exp(4 - abs(x)))
}
