#' Theme for City of Philadelphia graphs
#'
#' @export

theme_phl <- function(base_size = 9) {
  half_line <- base_size / 2
  theme_bw(
    base_size = base_size,
    base_family = "Open Sans") %+replace%
  theme(
    plot.title = element_text(
      size = rel(1.2), family = "Montserrat",
      margin = margin(b = half_line)),
    strip.background = element_blank(),
    strip.text = element_text(
      margin = margin(
        0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line),
      face = "bold"),
    axis.text = element_text(
      color = "black",
      size = rel(0.8)),
    axis.title = element_text(face = "bold"),
    complete = T)
}
