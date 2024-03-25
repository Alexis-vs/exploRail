# exploRail project : exec file scrap

library(dplyr)
library(rvest) # with rvest >= 1.0.4 for 'read_html_live()'
library(stringr)
library(stringi)
library(tidyr)
library(purrr)
library(arrow)

source(file = "src/init.R")
source(file = file.path(varglobales$dir$R, "main.R"),
       encoding = "UTF-8") # UTF-8 encoding for euro symbol in regex 


# Parameters for `get_trains()`
cities_from_paris <-  c("Lyon", "Rennes", "Strasbourg", "Marseille")

grid_parameters <- expand.grid(origin = "Paris",
                               destination = cities_from_paris,
                               stringsAsFactors = FALSE)

dates <- seq.Date(from = as.Date("2024-04-17"),
                  to = as.Date("2024-04-23"),
                  by = "day")


# get results according to grid parameters
results <- get_trains(origin = grid_parameters$origin,
                      destination = grid_parameters$destination,
                      date = dates,
                      local_save = FALSE)

# save in parquet format in `/data` directory
results_df <- results %>%
  unlist(recursive = FALSE) %>%
  do.call(what = "rbind") %>%
  mutate(scrap_day = as.Date(time_scrap),
         scrap_hour = ifelse(as.numeric(format(time_scrap, "%H")) < 15, "10h", "22h")) %>%
  mutate(days_before_trip = as.Date(departure) - scrap_day,
         .before = "scrap_day")

results_df %>%
  group_by(scrap_day, scrap_hour) %>%
  write_dataset(path = file.path(varglobales$dir$data))
