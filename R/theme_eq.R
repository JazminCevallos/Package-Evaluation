#
theme_eq <- function(base_size = 11,
                     base_family = 'sans') {
  eq <- (
    ggplot2::theme_minimal(base_size = base_size,
                           base_family = base_family) +
      theme(
        legend.position = 'bottom',
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(),
        axis.line.x = element_line()
      )
  )

  eq
}
