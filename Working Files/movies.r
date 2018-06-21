library(dplyr)
movies <- read.table("movielens.txt", header = FALSE, sep = "|", quote = "\"")
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "unknown", "Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

movies <- movies %>% select(-ID, -ReleaseDate, -VideoReleaseDate, -IMDB)
str(movies)
movies <- unique(movies)
str(movies)
