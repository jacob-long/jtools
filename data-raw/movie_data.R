library(readr)
# Read in raw data
chars <- read_csv("character_list5.csv")
movies <- read_csv("meta_data7.csv")

# # Use IMDB API pkg
# library(imdbapi)
# # Here's my API key from omdbapi.com
# omdb_api_key("b1dd4889")
# # Grab the first movie to set up the data frame
# first_movie <- find_by_id(movies[1, "imdb_id", drop = TRUE])[1,]
# # Add a bunch of empty rows
# imdb_dat <- first_movie[1:nrow(movies),]

# # Now I loop through each film and get its IMDB data
# for (i in 1:nrow(movies)) {
#   # Get the data
#   results <- find_by_id(movies[i, "imdb_id", drop = TRUE])
#   # Add the first row to the existing data
#   imdb_dat[i, ] <- results[1, ]
# }

imdb_dat <- readRDS("imdb_dat.rds")

movies["men_lines"] <- 
  sapply(
    lapply(strsplit(movies[, "lines_data", drop = T], ""), as.numeric),
    mean
  )/7

library(dplyr)
mc <- inner_join(imdb_dat, movies, by = c("imdbID" = "imdb_id"))

library(fivethirtyeight)
mc <- inner_join(mc, bechdel, by = c("imdbID" = "imdb"))

# comedy, drama, romance, action, [horror, thriller]
mc$genre <- stringr::str_extract_all(mc$Genre,
 "Comedy|Drama|Action|Horror|Thriller")
mc$genre[sapply(mc$genre, length) > 0] <- 
  sapply(mc$genre[sapply(mc$genre, length) > 0], `[[`, i = 1)
mc$genre[sapply(mc$genre, length) == 0] <- "Other"
mc$genre <- unlist(mc$genre)
mc$genre[mc$genre %in% c("Horror", "Thriller")] <- "Horror/Thriller"
mc$genre <- factor(mc$genre,
 levels = c("Action", "Comedy", "Drama", "Horror/Thriller", "Other"))

mc2 <- select(mc, title = Title, year = Year, release_date = Released, 
             runtime = Runtime, genre5 = genre, genre_detailed = Genre, 
             rated = Rated,
             director = Director, writer = Writer, actors = Actors, 
             language = Language, country = Country, metascore = Metascore,
             imdb_rating = imdbRating, imdb_votes = imdbVotes, imdb_id = imdbID,
             studio = Production, bechdel_binary = binary,
             bechdel_ordinal = clean_test, us_gross = domgross_2013,
             int_gross = intgross_2013, budget = budget_2013,
             men_lines = men_lines, lines_data = lines_data) %>%
  filter(rated != "NOT RATED")

mcf <- mutate(mc2,
    runtime = as.numeric(stringr::str_extract(runtime, "[0-9]*"))/60,
    metascore = as.numeric(metascore),
    year = as.numeric(year),
    rated = factor(rated, levels = c("G", "PG", "PG-13", "R", "NC-17"), 
                   ordered = TRUE),
  ) %>%
  rowwise() %>%
  mutate(
    bechdel_binary = switch(
      bechdel_binary,
      "PASS" = TRUE,
      "FAIL" = FALSE,
      NA
    ),
  ) %>%
  ungroup()

movies <- mcf
