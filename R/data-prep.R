library(tidyverse)

# https://www.kaggle.com/datasets/rounakbanik/the-movies-dataset?select=keywords.csv
movie_metadata <- read_csv("~/Downloads/archive/movies_metadata.csv")
ratings <- read_csv("~/Downloads/archive/ratings.csv")

json2string <- function(x) {
  x |>
    str_extract_all("\\{.*?\\}") |>
    map(str_remove, ".*': '") |>
    map(str_remove, "'\\}") |>
    map(setdiff, "Horror") |>
    map_chr(paste, collapse = ", ")
}

horror_movies <- movie_metadata |>
  filter(str_detect(genres, "'id': 27, 'name': 'Horror'")) |>
  filter(original_language == "en") |>
  filter(status == "Released") |>
  mutate(genres = json2string(genres)) |>
  mutate(production_countries = json2string(production_countries)) |>
  mutate(spoken_languages = json2string(spoken_languages)) |>
  select(
    -belongs_to_collection,
    -original_language,
    -homepage,
    -poster_path,
    -revenue,
    -production_companies,
    -adult,
    -overview,
    -imdb_id,
    -popularity,
    -status,
    -tagline,
    -video,
    -vote_average,
    -original_title,
    -vote_count
  )

last_rating <- lubridate::as_datetime(max(ratings$timestamp))

horror_times <- horror_movies |>
  select(id, release_date) |>
  left_join(ratings, by = c("id" = "movieId")) |>
  mutate(release_date = lubridate::as_datetime(release_date)) |>
  mutate(timestamp = lubridate::as_datetime(timestamp)) |>
  summarise(
    .by = "id",
    target = n() >= 100,
    time = if_else(
      target,
      difftime(sort(timestamp)[100], sort(timestamp)[1], units = "weeks"),
      difftime(
        last_rating,
        if_else(
          is.na(sort(timestamp)[1]),
          release_date[1],
          sort(timestamp)[1]
        ),
        units = "weeks"
      )
    )
  ) |>
  mutate(time = as.numeric(time)) |>
  mutate(target = target)

horror_movies <- horror_movies |>
  left_join(horror_times, by = join_by(id)) |>
  filter(time > 0) |>
  mutate(budget = if_else(budget == 0, NA, budget))

write_rds(horror_movies, "horror_movies.rds")
