
geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
    ggplot2::layer(
      geom = GeomTimeline,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
}

#' GeomTimeline
#'
#' See \code{\link{geom_timeline}} for description.
#'
#' @format NULL
#' @usage NULL
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
  required_aes = c('x'),

  default_aes = ggplot2::aes(
    y = 0,
    size = 1,
    color = 'grey50',
    alpha = 0.5,
    shape = 19,
    stroke = 0.5,
    fill = NA
  ),

  draw_key = ggplot2::draw_key_point,

  draw_panel = function(data, panel_scales, coord) {

    coords <- coord$transform(data, panel_scales)

    coords$size <-
      coords$size / max(coords$size) * 1.5

    grid::pointsGrob(
      x = coords$x,
      y = coords$y,
      pch = coords$shape,
      gp = grid::gpar(
        col = coords$colour,
        alpha = coords$alpha,
        cex = coords$size
      )
    )
  }
)
