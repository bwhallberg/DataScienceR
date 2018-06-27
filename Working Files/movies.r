library(dplyr)
movies <- read.table("movielens.txt", header = FALSE, sep = "|", quote = "\"")
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "unknown", "Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

movies <- movies %>% select(-ID, -ReleaseDate, -VideoReleaseDate, -IMDB)
str(movies)
movies <- unique(movies)
str(movies)

distances = dist(movies[2:20], method = "euclidean")
clusterMovies = hclust(distances, method="ward")
plot(clusterMovies)
rect.hclust(clusterMovies, k=10, border="red")
clusterGroups = cutree(clusterMovies, k = 10)
tapply(movies$Action, clusterGroups, mean)

subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]
cluster2 = subset(movies, clusterGroups == 2)
cluster2$Title[1:10]


k = 5
set.seed(1)
KMC = kmeans(movies, centers = k, iter.max=1000)