
eq_clean_data <- function(data) {

  required_vars <- c('LONGITUDE', 'LATITUDE', 'MONTH', 'DAY', 'YEAR',
                     'DEATHS', 'TOTAL_DEATHS', 'EQ_PRIMARY')
  purrr::map(required_vars, function(rv) {
    if(!(rv %in% names(data))) {
      stop('Missing required variable: ', rv)
    }
  })

  data <- data %>%
    dplyr::mutate_(
      LONGITUDE = ~as.numeric(LONGITUDE),
      LATITUDE = ~as.numeric(LATITUDE),
      MONTH = ~ifelse(is.na(MONTH), 1, MONTH),
      DAY = ~ifelse(is.na(DAY), 1, DAY),
      DEATHS = ~as.numeric(DEATHS),
      TOTAL_DEATHS = ~as.numeric(TOTAL_DEATHS),
      EQ_PRIMARY = ~as.numeric(EQ_PRIMARY))

  data <- data %>%
    dplyr::mutate_(DATE = ~purrr::pmap(list(YEAR, MONTH, DAY),
                                      function(y, m, d) {
      if (y < 0) {
        first_date <-
          as.numeric(as.Date('0 1 1', '%Y %m %d', origin = '1970-01-01'))
        mirror_date <-
          as.Date(paste(y * -1 - 1, 1, 1, sep = '-'), '%Y-%m-%d',
                  origin = '1970-01-01')
        dt <- as.numeric(mirror_date) - first_date

        end_of_year_dt <-
          as.numeric(as.Date(paste(y * -1 + 2, 12, 31, sep = '-'), '%Y-%m-%d',
                             origin = '1970-01-01')) -
          as.numeric(as.Date(paste(y * -1 + 2, m, d, sep = '-'), '%Y-%m-%d',
                             origin = '1970-01-01'))
        date <-
          as.Date(first_date - dt - end_of_year_dt + 1, origin = '1970-01-01')
      } else {
        date <- as.Date(paste(y, m, d, sep = '-'), '%Y-%m-%d')
      }
      date
    })) %>%
    dplyr::mutate_(DATE = ~unlist(DATE),
                  DATE = ~as.Date(DATE, origin = '1970-01-01'))

  data
}

eq_location_clean <- function(data) {

  required_vars <- c('COUNTRY', 'LOCATION_NAME')
  purrr::map(required_vars, function(rv) {
    if(!(rv %in% names(data))) {
      stop('Missing required variable: ', rv)
    }
  })

  data <- data %>%
    dplyr::mutate_(
      LOCATION_NAME =
        ~purrr::map2_chr(COUNTRY, LOCATION_NAME,
                        function(COUNTRY, LOCATION_NAME) {
                          gsub(paste0(COUNTRY, ":"), '', LOCATION_NAME)
                        }),
      LOCATION_NAME = ~stringr::str_trim(LOCATION_NAME),
      LOCATION_NAME = ~stringr::str_to_title(LOCATION_NAME)
    )

  data
}

eq_load_clean_data <- function() {
  eq <- get('quakes')
  df <- eq %>%
    eq_clean_data() %>%
    eq_location_clean()
  df
}
