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
mc <- left_join(mc, bechdel, by = c("imdbID" = "imdb"))

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

mc2 <- mc
mc2 <- filter(mc2, Rated %in% c("G", "PG", "PG-13", "R"))
mc2$Rated[mc2$Rated %in% c("G", "PG")] <- "G/PG"

fitn <- lm( log(domgross) - log(budget) ~ men_lines + genre + year.y,
 data = filter(mc2, year.y >= 1980))
summ(fit <- lm( log(domgross) - log(budget) ~ binary * year.y + genre,
 data = filter(mc2, year.y >= 1980)))
anova(fitn, fit)

summ(fitn <- lm(as.numeric(Metascore) ~ men_lines + year + genre,
 data = mc2))
summ(fit <- lm(as.numeric(Metascore) ~ men_lines + genre + Rated + genre + Rated + genre,
 data = mc2))
interact_plot(fit, pred = year, modx = men_lines)
anova(fitn, fit)

summ(fitn <- lm(imdbRating ~ men_lines + genre + imdbVotes,
 data = filter(mc2, year >= 1990)))
summ(fit <- lm(imdbRating ~ binary * log(budget) + genre ,
 data = filter(mc2, year.y >= 1990)))
interact_plot(fit, pred = year, modx = men_lines)
anova(fitn, fit)