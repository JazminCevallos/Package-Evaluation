
eq_timeline <- function(df,
                        countries = 'USA',
                        date_min = '2000-01-01',
                        date_max = '2018-01-01',
                        label_n = 0) {

  countries <- toupper(countries)

  df <- df %>%
    dplyr::filter_(~DATE >= date_min,
                   ~DATE <= date_max,
                   ~COUNTRY %in% countries)

  if (label_n == 0) {
    p <- df %>%
      ggplot2::ggplot() +
      geom_timeline(aes_(x = ~DATE,
                         y = ~COUNTRY,
                         color = ~TOTAL_DEATHS,
                         size = ~EQ_PRIMARY)) +
      ggplot2::scale_size_continuous(name = 'Richter scale value') +
      ggplot2::scale_color_continuous(name = '# of Deaths') +
      theme_eq()
  } else {
    p <- df %>%
      ggplot2::ggplot() +
      geom_timeline(aes_(x = ~DATE,
                        y = ~COUNTRY,
                        color = ~TOTAL_DEATHS,
                        size = ~EQ_PRIMARY)) +
      geom_timeline_label(aes_(x = ~DATE,
                               y = ~COUNTRY,
                               magnitude = ~EQ_PRIMARY,
                               label = ~LOCATION_NAME,
                               n_max = label_n
      )) +
      scale_size_continuous(name = 'Richter scale value') +
      scale_color_continuous(name = '# of Deaths') +
      theme_eq()
  }

  p
}
