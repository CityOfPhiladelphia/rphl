
# Internal list of Philadelphia color standards

phl_colors <- c(
  dark_ben_franklin  = "#0f4d90",
  ben_franklin_blue  = "#2176d2",
  light_ben_franklin = "#96c9ff",
  electric_blue      = "#25cef7",
  bell_yellow        = "#f3c613",
  flyers_orange      = "#f99300",
  kelly_drive_green  = "#58c04d",
  light_bell         = "#ffefa2",
  light_red          = "#fed0d0",
  light_kelly_drive  = "#b9f2b1",
  light_blue         = "#DAEDFE",
  phanatic_green     = "#3a833c",
  love_park_red      = "#cc3000",
  pride_purple       = "#9400c6",
  black              = "#000000",
  dark_gray          = "#444444",
  medium_gray        = "#a1a1a1",
  sidewalk           = "#cfcfcf"
)

#' Get names and hex codes from City of Philadelphia color palette
#' @param ... List of colors to return. If \code{NULL}, returns the entire set of options.
#' @param palette Named character vector of hex codes; defaults to \code{phl_standards}.
#' @examples
#' get_colors()
#' get_colors("ben_franklin_blue")
#' get_colors("bell_yellow", "ben_franklin_blue")
#' @export

get_colors <- function(..., palette = phl_colors) {
  colors <- c(...)

  if (is.null(colors))
    return (palette)
  palette[colors]
}

# Palettes using City of Philadelphia colors

phl_pal_list <- list(
  main = get_colors("ben_franklin_blue",
                    "light_ben_franklin",
                    "bell_yellow",
                    "kelly_drive_green",
                    "flyers_orange",
                    "medium_gray",
                    "pride_purple",
                    "love_park_red"),
  dark = get_colors("dark_ben_franklin", "phanatic_green", "love_park_red",
                    "pride_purple", "dark_gray"),
  light = get_colors("light_blue", "light_bell", "light_red", "light_kelly_drive"),
  blues = get_colors("dark_ben_franklin", "ben_franklin_blue", "light_ben_franklin"),
  grays = get_colors("black", "dark_gray", "medium_gray", "sidewalk")
)

#' City of Philadelphia color scales
#'
#' These functions provide \code{ggplot2}-compatible color and fill scales using
#' City of Philadelphia colors, available at
#' \url{https://standards.phila.gov/guidelines/design-development/brand-elements/color-palette/}.
#' The underlying code is adapted from \code{@drsimonj}'s tutorial at
#' \url{https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2}.
#'
#' @param ... options passed to \code{scale_color_manual} or
#'   \code{scale_color_gradientn}
#' @param palette character, which palette to use. Options are \code{"main"},
#'   \code{"dark"}, \code{"light"}, \code{"blues"} and \code{"grays"}.
#' @param discrete logical, whether palette is discrete or continuous.
#'   Continuous will interpolate between colors; discrete has a maximum number
#'   of values.
#' @param reverse logical, whether to reverse the order of the palette.
#' @param pal_list Named list of palettes, used by palette argument. Each
#'   element is a vector of hex codes.
#' @param named logical, whether the colors in the palette have names
#'   corresponding to the data. This is usually not true.

#'
#' @describeIn scale_color_phl Color scale
#' @examples
#' library(ggplot2)
#' p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point(size = 3)
#' p + scale_color_phl()
#'
#' p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Petal.Length)) +
#'   geom_point(size = 3)
#' p + scale_color_phl("blues", discrete = F)
#' p + scale_color_phl("blues", discrete = F, reverse = T)
#'
#' @export

scale_color_phl <- function(...,
                            palette = "main",
                            discrete = T,
                            reverse = F,
                            pal_list = phl_pal_list,
                            named = F) {

  pal <- phl_pal_list[[palette]]

  if(!named)
    names(pal) <- NULL

  if (reverse) pal <- rev(pal)
  if (discrete) {
    scale_color_manual(..., values = pal)
  } else {
    scale_color_gradientn(..., colors = pal)
  }
}

#' @describeIn scale_color_phl Fill scale
#' @examples
#' p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Species)) +
#'   geom_point(aes(size = Petal.Width), shape = 21)
#'
#' p + scale_fill_phl()
#'
#' p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Petal.Length)) +
#'   geom_point(aes(size = Petal.Width), shape = 21)
#' p + scale_fill_phl("blues", discrete = F)
#' p + scale_fill_phl("blues", discrete = F, reverse = T)
#'
#' @export

scale_fill_phl <- function(...,
                           palette = "main",
                           discrete = T,
                           reverse = F,
                           pal_list = phl_pal_list,
                           named = F) {

  pal <- phl_pal_list[[palette]]

  if(!named)
    names(pal) <- NULL

  if (reverse) pal <- rev(pal)
  if (discrete) {
    scale_fill_manual(..., values = pal)
  } else {
    scale_fill_gradientn(..., colors = pal)
  }
}
