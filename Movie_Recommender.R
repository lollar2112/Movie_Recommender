
install.packages('aws.s3')
install.packages('tidyverse')
install.packages('qdapTools')
install.packages('recommenderlab')
library(recommenderlab)


```{r setup, message=FALSE, results='hide'}

library('aws.s3')
library('tidyverse')
library('qdapTools')
library('recommenderlab')

# Matrix needed for recommender model (only numeric values)
# Cleaned and fully joined table with all data for final output

# Split genres to one genre per column per movie, only keep numeric value

movies_clean <- movies %>% 
  cbind(mtabulate(str_split(movies$genres, "\\|")))  %>% 
  select(-title, -genres, -'(no genres listed)') 


#Calculating the average rating of each film
avg_rating <- aggregate(ratings$rating, list(ratings$movieId), FUN=mean)
avg_movie_rating <- avg_rating

ratings_data <- setNames(aggregate(ratings$rating, list(ratings$movieId), FUN=mean),
                         c("movieId", "Avg_Rating"))


# Ratings and movies, filtering out movies without rating
movies_rated <- movies_clean  %>% 
  inner_join(ratings_data, by="movieId")


# Prepare dataset for recommender engine as matrix
movies_matrix <- movies_rated %>% 
  select(-Avg_Rating) %>% 
  column_to_rownames(var = "movieId") %>%
  as.matrix() %>% 
  as("binaryRatingMatrix") 

# Retrieve full list of genres as a vector
genres <- movies_matrix %>% 
  colnames() %>% 
  as_tibble()

# Retrieve top 15 of movie tags to filter out rarely used tags
tags_sel <- tags %>% 
  filter(!(tag %in% c("sci-fi", "action", "comedy", "BD-R", "funny", "horror", "romance"))) %>% 
  group_by(tag) %>% 
  tally() %>% 
  slice_max(n, n = 20)

# Top movie genres
genre_sel <- movies %>% 
  group_by(genres) %>% 
  tally() %>% 
  slice_max(n, n = 20)

# Clean up tags, one per movie and seperated by ",", only top 20 tags
tags_valid <- tags %>% 
  select(-userId, -timestamp) %>% 
  filter(tag %in% tags_sel$tag) %>% 
  group_by(movieId) %>% 
  mutate(tag = paste0(unique(tag), collapse = ",")) %>% 
  unique()
  
# Add tags to a column
movies_full <- movies %>% 
  inner_join(ratings_data, by = "movieId") %>% 
  left_join(tags_valid, by = "movieId")

=====

# Building the model
# Setting up recommender engine and perform item based collaborative filtering

recom <- Recommender(movies_matrix, method = "IBCF", param = list(k = 5))
 
# Create user genre choices and preprocess as matrix

# Personal genre choices
genre_choice <- c("Action", "Adventure", "Mystery")

genre_choice_matrix = genres %>% 
  mutate(genre = as.numeric(value %in% genre_choice)) %>%
  pivot_wider(names_from = value, values_from = genre) %>%
  as.matrix() %>%
  as("binaryRatingMatrix")



# Making predictions and retrieve highest matching genre

pred <- predict(recom, newdata = genre_choice_matrix, n=1)
fav_genre <- getList(pred) %>%
  as.character()
fav_rating <- getRatings(pred) %>%
  as.numeric()


# Create user tag choices

tag_choice <- c("based on a book")

top5 <- movies_full %>% 
  filter(str_detect(movies_full$genres, fav_genre) == TRUE, str_detect(movies_full$tag, tag_choice) == TRUE) %>%
  mutate(match = fav_rating * avg_rating) %>%
  arrange(desc(match)) %>% 
  select(title, match)
head(top5)








































