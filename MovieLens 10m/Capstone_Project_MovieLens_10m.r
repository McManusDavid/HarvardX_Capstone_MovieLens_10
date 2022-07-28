
# Harvardx PH125.9x Capstone Project: Movielens 10m
# Author: David Mc Manus

# Load packages
# Install if required
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org" )
if(!require(ds4psy)) install.packages("ds4psy", repos = "http://cran.us.r-project.org" )
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org" )
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org" )
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org" )
if(!require(ggforce))install.packages("ggforce", repos = "http://cran.us.r-project.org" )
if(!require(rmarkdown))install.packages("rmarkdown", repos = "http://cran.us.r-project.org"  )
if(!require(knitr))install.packages("knitr", repos = "http://cran.us.r-project.org" )
if(!require(kableExtra))install.packages("kableExtra", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)
library(tidyr)
library(ds4psy)
library(dplyr)
library(lubridate)
library(recosystem)
library(ggforce)
library(rmarkdown)
library(knitr)
library(kableExtra)


# Change memory allocation 
# 56000 equals 7gb
memory.limit(size=56000)


##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


# Analysing Dataset
# Structural 
# Head
head(edx) %>%
  kbl(caption = "edX Data Set") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

# No of Users and Movies
edx %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId)) %>%
  kbl(caption = "Number of User and Movies") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

# Analysing Rating Paterns
# most rated movies 
edx %>% group_by(title) %>%
  summarise(count = n()) %>%
  slice_max(count,n=10, with_ties = FALSE)%>% 
  ggplot(aes(x= reorder(title, count),y = count)) + 
  geom_bar(stat='identity', fill="blue") + coord_flip(y=c(0, 40000))+
  ggtitle("10 Most Rated Movies")

# least rated movies
edx %>% group_by(title) %>%
  summarise(count = n()) %>%
  slice_min(count, n=10, with_ties = FALSE)%>% 
  ggplot(aes(x= reorder(title, count),y = count)) + 
  geom_bar(stat='identity', fill="blue") + coord_flip(y=c(0, 40000))+
  scale_y_continuous()+
  ggtitle("10 Least Rated Movies")

# AVerage no of Rating for Most and Least Rated
edx %>% group_by(movieId) %>%
  summarise(count = n()) %>%
  slice_max(count, n=10, with_ties = FALSE) %>%
  summarise("Average" = mean(count)) %>%
  kbl(caption = "Average Number of Ratings for the 10 Most Rated Movies") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

edx %>% group_by(movieId) %>%
  summarise(count = n()) %>%
  slice_min(count, n=10, with_ties = FALSE) %>%
  summarise("Average" = mean(count)) %>%
  kbl(caption = "Average Number of Ratings for the 10 Least Rated Movies") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

# Top 10 Highest Rated Movies and No of Ratings
edx %>% group_by(title) %>%
  summarise(mean = mean(rating),  count = n()) %>%
  slice_max(mean, n=10, with_ties = FALSE) %>%
  kbl(caption = "Top 10 Highest Rated Movies")%>%
  kable_styling(latex_options = c("striped", "hold_position"))


# Ratings by Genre
# Most Rated Genres
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(genres, -count), y = count)) +
  geom_bar(stat = "identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  xlab("Genres")+
  ylab("Rating")+
  ggtitle("Number of Rating by Genre")

# Users by ratings
edx %>% group_by(userId) %>%
  summarise(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(bins = 60)+
  scale_x_log10()+
  xlab("Ratings")+
  ylab("Users")+
  ggtitle("Distrabution of Users by Activity")

# Top Ratings by Number of Ratings
edx %>% group_by(rating) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>% 
  ggplot(aes(x = rating, y = count, fill = is_wholenumber(rating))) +
  geom_bar(stat='identity') +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  labs(x="Stars", y="Number of Ratings") +
  scale_fill_discrete(name = "Whole Star")+
  ggtitle("Distribution of Ratings")

# Analysing Means
# mean rating by genre, with error
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(count = n(), avg = mean(rating), se = sd(rating)/sqrt(count)) %>%
  mutate(genres = reorder(genres, avg)) %>%
  filter(genres != "(no genres listed)")%>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) +
  geom_point() +
  geom_errorbar()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  ggtitle("Mean Rating by Genre")

#year of release
edx %>% mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}")))) %>%
  group_by(releaseyear) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(releaseyear, rating)) +
  geom_point() +
  geom_smooth()+
  ggtitle("Yearly Mean Rating")

#year of release by genre
#use ggforce to split into graphs into multiple pages
#first process the data
release_year_genre <- edx %>% mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}")))) %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(releaseyear,genres)%>%
  summarize(rating = mean(rating)) 

#find how many pages are needed
release_year_genre%>%
  ggplot(aes(releaseyear, rating)) +
  geom_point() +
  geom_smooth()+
  facet_wrap_paginate(~genres, nrow = 2, ncol = 2)+
  ggtitle("Yearly Mean Rating by Genre") ->p

required_n_pages <- n_pages(p)

#loop to print each page
for(i in 1:required_n_pages){
  release_year_genre%>%
    ggplot(aes(releaseyear, rating)) +
    geom_point() +
    geom_smooth()+
    facet_wrap_paginate(~genres, nrow = 2, ncol = 2, page = i)+
    ggtitle("Yearly Mean Rating by Genre") ->p
  
  print(p)
}

#date of rating v release
edx %>% mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))))%>%
  mutate(date = as.numeric(what_year(round_date(as_datetime(timestamp),"year")))) %>%
  mutate(Release_Review = releaseyear - date) %>%
  group_by(Release_Review) %>%
  summarise(rating = mean(rating))%>%
  ggplot(aes(Release_Review, rating)) +
  geom_point() +
  geom_smooth()+
  xlab("Years After Release")+
  ylab("Mean Rating")+
  ggtitle("Effect of Delayed Rating")

#date of rating v release by genre
#use ggforce to break up into multiple pages
rating_release_genre <- edx %>% mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))))%>%
  mutate(date = as.numeric(what_year(round_date(as_datetime(timestamp),"year")))) %>%
  mutate(Release_Review = releaseyear - date) %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(Release_Review, genres) %>%
  summarise(rating = mean(rating))

#find how many pages needed
rating_release_genre %>%
  ggplot(aes(Release_Review, rating)) +
  geom_point() +
  geom_smooth()+
  facet_wrap_paginate(~genres, nrow = 2, ncol = 2)+
  xlab("Years After Release")+
  ylab("Mean Rating")+
  ggtitle("Effect of Delayed Rating by Genre") -> p

required_n_pages <- n_pages(p)

#run loop to print each page
for (i in 1:required_n_pages){
  rating_release_genre %>%
    ggplot(aes(Release_Review, rating)) +
    geom_point() +
    geom_smooth()+
    facet_wrap_paginate(~genres, nrow = 2, ncol = 2, page = i)+
    xlab("Years After Release")+
    ylab("Mean Rating")+
    ggtitle("Effect of Delayed Rating by Genre") -> p
  
  print(p)
}


# Recommendation Systems
# define rmse
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#creating training and testing sets from edx
#test set will be 10% of edx
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

#insuring test movie and users are in training set
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Model 1 - Simple Mean Rating

mu_hat <- mean(train_set$rating)

rmse_native <- RMSE(test_set$rating, mu_hat)

#save rmse results
rmse_results <- tibble(method = "Just the average", RMSE = rmse_native) 

rmse_results %>%
  kbl(caption = "RMSE Results") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

#Model 2 - adding movie bias

mu <- mean(train_set$rating)

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

rmse_movie_bias <-RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results, tibble(method ="With Movie Bias", RMSE = rmse_movie_bias ))

rmse_results %>%
  kbl(caption = "RMSE Results") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

#Model 3 - adding user bias
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

rmse_user_bias <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results, tibble(method = "With Movie Bias and User Bias", RMSE = rmse_user_bias ))

rmse_results %>%
  kbl(caption = "RMSE Results") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

#Model 4 - adding Release Year
#adding Release Date to data sets
test_set <- test_set %>% mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))))
train_set <- train_set %>% mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))))

release_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(releaseyear) %>%
  summarize(b_r = mean(rating - mu - b_i - b_u))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by= 'movieId') %>%
  left_join(user_avgs, by='userId') %>% 
  left_join(release_avgs, by='releaseyear') %>% 
  mutate(pred = mu + b_i + b_u + b_r) %>% 
  pull(pred)

rmse_release_bias <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results, tibble(method = "With Movie Bias and User Bias and Release Year Bias", RMSE = rmse_release_bias ))

rmse_results %>%
  kbl(caption = "RMSE Results") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

#Model 5 - Adding Genre Bias
#seperating genres in data sets
test_set <- test_set  %>% separate_rows(genres, sep = "\\|")
train_set <- train_set %>% separate_rows(genres, sep = "\\|")

genres_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% 
  left_join(release_avgs, by='releaseyear') %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu - b_i - b_u - b_r))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by= 'movieId') %>%
  left_join(user_avgs, by='userId') %>% 
  left_join(release_avgs, by='releaseyear') %>%
  left_join(genres_avgs, by= "genres") %>%
  mutate(pred = mu + b_i + b_u + b_r + b_g) %>% 
  pull(pred)

rmse_genre_bias <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, tibble(method = "With Movie Bias, User Bias, Release Year Bias, and Genre Bias ", RMSE = rmse_genre_bias ))

rmse_results %>%
  kbl(caption = "RMSE Results") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

#Model 6 - substituting release year and genre for release year by genre

release_by_genre_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% 
  group_by(releaseyear, genres) %>% 
  summarize(b_r_g = mean(rating - mu - b_i - b_u))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by= 'movieId') %>%
  left_join(user_avgs, by='userId') %>% 
  left_join(release_by_genre_avgs, by= c("releaseyear", "genres")) %>%
  mutate(pred = mu + b_i + b_u + b_r_g) %>% 
  pull(pred)

rmse_release_genre_bias <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, tibble(method = "With Movie Bias, User Bias, and Release Year by Genre Bias ", RMSE = rmse_release_genre_bias ))

rmse_results %>%
  kbl(caption = "RMSE Results") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

#Model 7 - subsituting release year by genre for release year - review year
#getting the year of review
test_set <- test_set %>% mutate(date = as.numeric(what_year(round_date(as_datetime(timestamp),"year")))) %>%
  mutate(release_review = releaseyear - date)
train_set <- train_set %>% mutate(date = as.numeric(what_year(round_date(as_datetime(timestamp),"year")))) %>%
  mutate(release_review = releaseyear - date)

release_review_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>%
  group_by(release_review) %>% 
  summarize(b_r_r = mean(rating - mu - b_i - b_u))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by= 'movieId') %>%
  left_join(user_avgs, by='userId') %>% 
  left_join(release_review_avgs, by='release_review') %>%
  mutate(pred = mu + b_i + b_u + b_r_r) %>% 
  pull(pred)

rmse_release_review_bias <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, tibble(method = "With Movie Bias, User Bias, and Release/Review Bias ", RMSE = rmse_release_review_bias ))

rmse_results %>%
  kbl(caption = "RMSE Results") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

#Model 8 - adding genre to release-review
release_review_plus_genre_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>%
  left_join(release_review_avgs , by='release_review') %>%
  group_by(genres) %>% 
  summarize(b_r_r_g = mean(rating - mu - b_i - b_u - b_r_r))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by= 'movieId') %>%
  left_join(user_avgs, by='userId') %>% 
  left_join(release_review_avgs, by='release_review') %>%
  left_join(release_review_plus_genre_avgs, by='genres')%>%
  mutate(pred = mu + b_i + b_u + b_r_r + b_r_r_g) %>% 
  pull(pred)

rmse_release_review_plus_genre_bias <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, tibble(method = "With Movie Bias, User Bias, Release/Review Bias, and genre ", RMSE = rmse_release_review_plus_genre_bias ))

rmse_results %>%
  kbl(caption = "RMSE Results") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

#Model 9 - substituting release year - review year for release year - review year by genre
#adding release-review
# NA's being caused in movieId 3136, Documentary, r_r c(-48, -49, -52)
test_set <- test_set %>% mutate(date = as.numeric(what_year(round_date(as_datetime(timestamp),"year"))))%>%
  mutate(release_review = releaseyear - date)
train_set <- train_set %>% mutate(date = as.numeric(what_year(round_date(as_datetime(timestamp),"year")))) %>%
  mutate(release_review = releaseyear - date)


release_review_genre_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>%
  group_by(release_review, genres) %>% 
  summarize(b_r_r_g2 = mean(rating - mu - b_i - b_u))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by= 'movieId') %>%
  left_join(user_avgs, by='userId') %>% 
  left_join(release_review_genre_avgs, by= c("release_review", "genres")) %>%
  mutate(pred = mu + b_i + b_u + b_r_r_g2) %>%
  pull(pred)


rmse_release_review_genre_bias <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, tibble(method = "With Movie Bias, User Bias, and Release/Review Bias by Genre ", RMSE = rmse_release_review_genre_bias ))

rmse_results %>%
  kbl(caption = "RMSE Results") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

#checking where the na is

test_set %>%
  left_join(movie_avgs, by= 'movieId') %>%
  left_join(user_avgs, by='userId') %>% 
  left_join(release_review_genre_avgs, by= c("release_review", "genres")) %>%
  mutate(pred = mu + b_i + b_u + b_r_r_g2) %>%
  filter(is.na(pred)) %>%
  kbl(caption = "Entries with NA's") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

#Model 10 - Regularized Movies - PLS
#Finding Lambda
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    mutate(pred = mu + b_i ) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  
lambdas[which.min(rmses)] %>%
  kbl(caption = "Lambda with Lowest RMSE") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

#getting a rmse for lambda 2.5
lambda <- 2.5
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i ) %>%
  pull(pred)

rmse_reg_movie <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results, tibble(method = "With Regularised Movie ", RMSE = rmse_reg_movie ))

rmse_results %>%
  kbl(caption = "RMSE Results") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

#Model 11 - Reg Movies and User bias
lambda <-  2.5
mu <- mean(train_set$rating)

movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

user_avgs_new <- train_set %>%
  left_join(movie_reg_avgs, by= "movieId") %>% 
  group_by(userId)%>%
  summarise(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_avgs_new, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

rmse_reg_movie_and_user <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, tibble(method = "With Regularised Movie and User Bias ", RMSE = rmse_reg_movie_and_user ))

rmse_results %>%
  kbl(caption = "RMSE Results") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

#Model 12 - reg movie plus user and release year by genre bias
#using previously min lambda
lambda <- 2.5
mu <- mean(train_set$rating)

movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

user_avgs_new <- train_set %>%
  left_join(movie_reg_avgs, by= "movieId") %>% 
  group_by(userId)%>%
  summarise(b_u = mean(rating - mu - b_i))


release_year_by_genre_avgs <- train_set %>% 
  left_join(movie_reg_avgs, by = 'movieId') %>%
  left_join(user_avgs_new, by = 'userId') %>%
  group_by(releaseyear, genres)  %>%
  summarise(b_r_g = mean(rating - mu - b_i - b_u))

predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(user_avgs_new, by = 'userId')%>%
  left_join(release_year_by_genre_avgs, by =c('releaseyear', 'genres')) %>% 
  mutate(pred = mu + b_i + b_u + b_r_g) %>%
  pull(pred)

rmse_reg_movie_user_plus_release_genre <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results, tibble(method = "With Regularised Movie. User Bias, and Release Year by Genre Bias", RMSE = rmse_reg_movie_user_plus_release_genre ))


rmse_results %>%
  kbl(caption = "RMSE Results") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

#Matrix Factorization
#Model 13 - recosystem
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)
train_reco <- with(train_set, data_memory(user_index = userId, item_index = movieId, rating = rating))
test_reco <- with(test_set, data_memory(user_index = userId, item_index = movieId, rating = rating))

r <- Reco()
tuned_reco <- r$tune(train_reco,opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                            costp_l1 = 0, costq_l1 = 0,
                                            nthread = 1, niter = 10))

r$train(train_reco, opts = c(tuned_reco$min, nthread = 1, niter = 20))
reco_results <- r$predict(test_reco, out_memory())

rmse_reco <- RMSE(reco_results, test_set$rating)
rmse_results <- bind_rows(rmse_results, tibble(method = "Recosystem ", RMSE = rmse_reco ))

rmse_results %>%
  kbl(caption = "RMSE Results") %>%
  kable_styling(latex_options = c("striped", "hold_position"))


#Final Validation 
# recosystem
train_reco <- with(edx, data_memory(user_index = userId, item_index = movieId, rating = rating))
test_reco <- with(validation , data_memory(user_index = userId, item_index = movieId, rating = rating))

r <- Reco()

tuned_reco <- r$tune(train_reco,opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                            costp_l1 = 0, costq_l1 = 0,
                                            nthread = 1, niter = 10))

r$train(train_reco, opts = c(tuned_reco$min, nthread = 1, niter = 20))
reco_results <- r$predict(test_reco, out_memory())

final_rmse_reco <- RMSE(reco_results, validation$rating)
rmse_results <- bind_rows(rmse_results, tibble(method = "Final Validation: Recosystem ", RMSE = final_rmse_reco ))

rmse_results %>%
  kbl(caption = "RMSE Results") %>%
  kable_styling(latex_options = c("striped", "hold_position"))





