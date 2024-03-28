library(dplyr)
library(lubridate)
library(fastDummies)
library(stringr)

dates <- read.csv("C:\\Users\\ALec\\Documents\\joelle\\weather_data\\richmond 2020-01-01 to 2021-12-31.csv") %>%
  dplyr::select(datetime) %>%
  dplyr::mutate(day = lubridate::wday(datetime, label = T, week_start = 1)) %>%
  fastDummies::dummy_cols(select_columns = "day") %>%
  dplyr::select(-day) %>%
  dplyr::rename_with(~ stringr::str_remove(., "day_"), everything())


dates2 <- read.csv("C:\\Users\\ALec\\Documents\\joelle\\weather_data\\richmond 2020-01-01 to 2021-12-31.csv") %>%
  dplyr::select(datetime) %>%
  dplyr::mutate(day = lubridate::wday(datetime, label = T, week_start = 1))
same <- dates2 %>%
  pivot_longer(cols = where(is.character),
               names_to = "dummy_names",
               values_to = "dummy_levels") %>%
  mutate(dummy_value = 1) %>%
  pivot_wider(names_from = c(dummy_names, day),
              values_from = dummy_value,
              values_fill = 0) %>%
  dplyr::rename_with(~ stringr::str_remove(., "datetime_"), everything()) %>%
  dplyr::select(dummy_levels, order(colnames(.)))
