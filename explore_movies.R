
# change working directory

source("R_getwd.r")

# read the data

movies <- read.csv("movies.csv", header = T)
ratings<- read.csv("ratings.csv", header = T)

# basic information of the data

dim(movies) # 27278     3
dim(ratings) # 20000263        4

colnames(movies)
colnames(ratings)

# check the headers

head(movies)
head(ratings)

## descriptive statistics

# load dplyr library

library(dplyr)

# unique values of ratings

ratings %>%
  distinct(rating) %>%

# unique values of ratings and the number of movies or records
# same movie could be rated by different users with the same rating
  
ratings %>%
  group_by(rating) %>%
  summarise(
    umovie = n_distinct(movieId),
    ucase = n()
  )

  


# number of movies with ratings > 3.5

ratings %>%
  filter(rating > 3.5) %>%
  summarise(
    avg = mean(rating),
    med = median(rating),
    min = min(rating),
    max = max(rating),
    m3.5 = n_distinct(movieId))



  