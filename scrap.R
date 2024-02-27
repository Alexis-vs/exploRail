# exploRail project : exec file scrap

library(dplyr)
library(rvest) # with rvest >= 1.0.4 for 'read_html_live()'
library(stringr)
library(stringi)
library(tidyr)
library(purrr)
library(arrow)

source(file = "main.R",
       encoding = "UTF-8") # UTF-8 encoding for euro symbol in regex 


# expand grid
cities_from_paris <-  c("Rennes", "Lyon") # "Strasbourg", "Marseille"
dates <- seq.Date(from = Sys.Date() + 1,
                  to = as.Date("2024-05-22"),
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


# get results according to grid parameters
results <- grid_parameters %>%
  pmap(.f = possibly(get_trains), .progress = TRUE) %>%
  do.call(what = "rbind") %>%
  mutate(day = as.Date(time_scrap),
         hour = ifelse(as.numeric(format(time_scrap, "%H")) < 15, "10h", "22h"))

# save in parquet format
results %>%
  group_by(day, hour) %>%
  write_dataset(path = "data/")
