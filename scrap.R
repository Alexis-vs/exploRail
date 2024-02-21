# exploRail project : exec file scrap

library(dplyr)
library(rvest) # with rvest >= 1.0.4 for 'read_html_live()'
library(stringr)
library(tidyr)
library(purrr)


# expand grid
cities_from_paris <-  c("Rennes", "Lyon", "Strasbourg", "Marseille")
dates <- seq.Date(from = as.Date("2024-02-23"),
                  to = as.Date("2024-02-24"),
                  by = "day")

grid_parameters_go <- expand.grid(origin = "Paris",
                                  destination = cities_from_paris,
                                  date = as.character(dates),
                                  stringsAsFactors = FALSE)

grid_parameters_back <- grid_parameters_go %>%
  rename(origin = destination,
         destination = origin) %>%
  relocate(origin, .before = destination)


grid_parameters <- rbind(grid_parameters_go,
                         grid_parameters_back) %>%
  arrange(date, origin, destination)

grid_parameters

results <- grid_parameters %>%
  pmap(.f = get_trains, .progress = TRUE) %>%
  do.call(what = "rbind")

results
