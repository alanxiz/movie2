
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

## select a random sample of movies

id = ratings %>%
        distinct(movieId)

# randomly select 100 movies

idx <- sample_n(id, 100)

# merge randomly selected movies to ratings data to get their ratings

ratings2 <- inner_join(idx, ratings, by ="movieId")

# by movies, avg rating and number of reviews

s1<- ratings2 %>%
      group_by(movieId) %>%
      summarise(
        avg = mean(rating),
        nsize = n())

# rank grouped data by review count descending

s1<- as.data.frame(s1)

s1<- s1 %>%
       arrange(desc(nsize))

# check the genre and title of the top two reviewed movies

movies %>%
  filter(movieId %in% c(2324, 594))

################### check the relationship between length of movie and number of reviews

# substring movie title to get year of movie

library(stringr)

movies<- movies %>%
  mutate(year = str_sub(title, -5, -2))

# merge s1 with movies to get the year of the movie

s2 <- inner_join(s1, movies, by = "movieId") %>%
        select(movieId, avg, nsize, year)

s2$year <- as.numeric(s2$year)

# remove NA in year due to conversion

s2<- s2 %>%
       filter(!is.na(year))

# check correlation between year and review count

cor(s2$nsize, s2$year)  # -0.09

# use regression

model <- lm(nsize ~ year, data = s2)
summary(model)  # year is not significant 
































  