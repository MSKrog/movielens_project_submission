

## Downloading and cleaning the data

# Creating the test and validation sets

###################################
# Create edx set and validation set
###################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1) 
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




## Exploring the data set


#Count of the number of rows & columns

nrow(edx)
ncol(edx)



#Number of 0 & 3 ratings

length(which(edx$rating == 0))
length(which(edx$rating == 3))



#Number of different movies

diff_movies <- unique(edx$movieId)
length(diff_movies)



#Number of different users

n_distinct(edx$userId)



#Number of movie ratings in each genre

genres <- edx %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(number = n()) %>%
  arrange(desc(number))

ggplot(genres, aes(x=as.factor(genres), y=as.factor(number), fill = as.factor(genres))) +
  geom_bar(stat = "identity") + 
  theme_classic() +
  scale_fill_hue(c =40) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() 



#Which movie has most ratings:

edx %>% group_by(title) %>% summarise(number = n()) %>%
  arrange(desc(number))



#What are the five most given ratings in order from most to least?

edx %>% group_by(rating) %>%
  summarise(number = n()) %>%
  arrange(desc(number))




#.5 star ratings more or less common than whole?
#(Taken directly from the course work)

edx %>% group_by(rating) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  theme_dark() +
  geom_line(colour = "Green")





## Creating the calculation for RMSE

  
### Creating the RMSE function
  
#Create RMSE function

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}





## Step 1: Building the simplest possible recommendation system

### Running the RMSE function against the average rating for all movies

#Avg rating of all movies across all users:

mu_hat <- mean(edx$rating)
mu_hat



#How well does this prediction do on the test set?

naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse




### Results table - 'Just the average'
  
#To show continued improvements in the RMSE a table will be created as below:

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

rmse_results







## Movie Effect Model

  
### Showing the movie bias
  

mu <- mean(edx$rating) #Create avg rating across all movies.
movie_avgs <- edx %>% #Create avg rating per movie from the edx data set.
  group_by(movieId) %>% #Group by movie ID.
  summarise(b_i = mean(rating - mu)) #b_i is the average rating for any given movie minus the overall avg rating.

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"), fill = "Avg Rating Per Movie - Avg Rating For All Movies")

#movies with a b_1 1.5 = 5 star avg movie ratings!




### Running the 'Movie effect model' and showing the results

predicted_ratings <- mu + validation %>% #Create the column of predicted ratings.
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, validation$rating) #Test the predicted ratings against the validation set ratings.
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie effect model",
                                     RMSE = model_1_rmse )) #Table the RMSE results.
rmse_results %>% knitr::kable() #Show in table.


  
  
  
## Movie + user effect model
  
  
### Showing avg ratings per user
  

#Avg rating per user for users who have rated over 100 movies

edx %>% 
  group_by(userId) %>%  #Group by user ID
  summarize(b_u = mean(rating)) %>% #Create avg ratings per user
  filter(n()>=100) %>% #Filter the top 100 results
  ggplot(aes(b_u)) + #Plot avg ratings per user
  geom_histogram(bins = 30, color = "black", aes(fill = ..count..))



### Including user bias in the prediction and showing the results


user_avgs <- edx %>% #Create user_avgs from the edx data set.
  left_join(movie_avgs, by='movieId') %>% #Join with movie_avgs by user ID.
  group_by(userId) %>% #Group by user ID.
  summarize(b_u = mean(rating - mu - b_i)) #Avg unique user rating = avg of the avg rating minus the overall avg rating minus the movie bias.

predicted_ratings2 <- validation %>% #Create the predicted ratings from the validation set.
  left_join(movie_avgs, by='movieId') %>% #Join movie_avgs by movie ID.
  left_join(user_avgs, by='userId') %>% #Join user_avgs by user ID.
  mutate(pred = mu + b_i + b_u) %>% #Create predictions combining avg rating + movie bias + user bias.
  .$pred

model_2_rmse <- RMSE(predicted_ratings2, validation$rating) #Run the RMSE prediction model.
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + user effects model",  #Create the rmse_results data frame.
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable() #Plot results into table.




## Regularised movie + user effect model

### The biggest mistakes made in the 'Movie effect model'

#Looking at 10 largets errors of movie effects only:

edx %>% #Using the edx data set.
     left_join(movie_avgs, by='movieId') %>% #Joining movie_avgs by movie ID.
     mutate(residual = rating - (mu + b_i)) %>% #Creating 'redidual column of rating minus the sum of average rating and movie bias.
arrange(desc(abs(residual))) %>% #Arranging residual in descending order.
  select(title,  residual) %>% slice(1:10) %>% knitr::kable() #Show first ten results of 'title' and 'residual'




### Top 10 best movies based on original 'Movie effect method'

#10 best movies based on estimates:

movie_titles <- edx %>% #Create 'movie_titles' using edx dataset.
     select(movieId, title) %>% #Select movie ID and title columns.
     distinct() #Keep unique rows
movie_avgs %>% left_join(movie_titles, by="movieId") %>% #Join 'movie_titles' to 'movie_avgs by movie ID.
arrange(desc(b_i)) %>% #Arrange in descending order according to movie bias
  select(title, b_i) %>% #Select the title and movie bias columns
  slice(1:10) %>% #Choose the first ten entries
  knitr::kable() #Place into table



### Top 10 worst movies based on original 'Movie effect method'

#10 worst movies based on estimates:

movie_avgs %>% left_join(movie_titles, by="movieId") %>% #Join 'movie_titles' to 'movie_avgs by movie ID.
  arrange(b_i) %>% #Arrange by movie bias
  select(title, b_i) %>% #Select the title and movie bias columns
  slice(1:10) %>% #Choose the first ten entries
  knitr::kable() #Place into table



### 10 worst movies including the count of ratings for each movie:
  
#Creating the same table but now including the number of ratings received.

#10 worst movies:

edx %>% dplyr::count(movieId) %>% #Count ratings per movie ID.
  left_join(movie_avgs) %>% #Join to 'movie_avgs'.
  left_join(movie_titles, by="movieId") %>% #Join 'movie_titles' by movie ID.
  arrange(desc(b_i)) %>% #Arrange in descending order by movie bias.
  select(title, b_i, n) %>% #Select the title, movie bias, and number of ratings columns.
  slice(1:10) %>% #Show first ten entries.
  knitr::kable() #Show in table.



### 10 best movies with the count of ratings for each movie:

#10 best movies:
edx %>% dplyr::count(movieId) %>% #Count ratings by movie ID.
  left_join(movie_avgs) %>% #Join to 'movie_avgs'.
  left_join(movie_titles, by="movieId") %>% #Join 'movie_titles' by movie ID.
  arrange(b_i) %>% #Arrange by movie bias.
  select(title, b_i, n) %>% #Select the title, movie bias, and number of ratings columns.
  slice(1:10) %>% #Show first ten entries.
  knitr::kable() #Show in table.


### Introducing regularisation

#Regularization using lambda = 5.25

lambda <- 5.25 #Create 'lambda'.
mu <- mean(edx$rating) #Create overall avg rating from 'edx'.
movie_reg_avgs <- edx %>%  #Create 'movie_reg_avgs' (movie regularisation averages).
  group_by(movieId) %>% #Group by movie ID.
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) #Summarise movie bias (including lambda penalisation).


#Plot to visualise the differences between original and regularized estimates:

data_frame(original = movie_avgs$b_i, #Create a data frame using the original movie bias and the regularised movie bias and the number of regularised avgs.
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + #Plot to show biases that will be penalised (larger circles) where it can be seen that when 'n' is small the values shrink more towards zero.
  geom_point(shape=5, alpha=0.75, colour = "Orange") +
  theme_dark()


### Top 10 movies including regularisation effect:

edx %>%
     dplyr::count(movieId) %>% #Count ratings by movie ID.
     left_join(movie_reg_avgs) %>% #Join 'movie_reg_avgs'.
     left_join(movie_titles, by="movieId") %>% #Join 'movie_title' by movie ID.
     arrange(desc(b_i)) %>% #Arrange in descending order by movie bias.
     select(title, b_i, n) %>% #Select the title, movie bias and count columns.
     slice(1:10) %>% #Select the first 10 entries.
     knitr::kable() #Show in a table.


### Worst 10 movies including regularisation effect:

edx %>%
     dplyr::count(movieId) %>% #Count ratings by movie ID.
     left_join(movie_reg_avgs) %>% #Join 'movie_reg_avgs'.
     left_join(movie_titles, by="movieId") %>% #Join 'movie_title' by movie ID.
     arrange(b_i) %>% #Arrange by movie bias.
     select(title, b_i, n) %>% #Select the title, movie bias and count columns.
     slice(1:10) %>% #Select the first 10 entries.
     knitr::kable() #Show in a table.



### Prediction results of 'Regularisation + movie effect model'

predicted_ratings3 <- validation %>% #Create 'predicted_ratings3 from the validation set.
left_join(movie_reg_avgs, by='movieId') %>% #Join 'movie_reg_avgs by movie ID.
  mutate(pred = mu + b_i) %>% #Create 'pred' from the overall avg and movie bias.
  .$pred

model_3_rmse <- RMSE(predicted_ratings3, validation$rating) #Run the prediction model against validation set.
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model", #Create a data frame of the results.
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable() #Show in a table.



### Introducing tuning through 'lambda'

lambdas <- seq(0, 10, 0.25) #Setting tuning parameters for finding best lambda.
rmses <- sapply(lambdas, function(l){ #Apply lambdas to the function below.
  mu <- mean(edx$rating) #Creating the overall avg rating
  b_i <- edx %>% #Creating the movie bias
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l)) #Create movie bias.
  b_u <- edx %>% # Creating the user bias as previously shown
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l)) #Create user bias. 
  predicted_ratings <- #Creating the prediction from the validation set.
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>% #Create pred from overall avg, movie bias and user bias (lambda factored in each)
    .$pred
  return(RMSE(predicted_ratings, validation$rating)) #Run the RMSE function.
})



### Plot to show best lambda to produce lowest RMSE

qplot(lambdas, rmses) #Plot lambdas vs RMSE to show best lambda for RMSE


# Lambda with the lowest RMSE:

lambda <- lambdas[which.min(rmses)] #Select the lambda that gives the lowest RMSE.
lambda



### Run predictions and table results

rmse_results <- bind_rows(rmse_results, #Create data frame to show RMSE.
                          data_frame(method="Regularized movie + user effect model",  
                                     RMSE = min(rmses))) #Select the min RMSE from the data frame.
rmse_results %>% knitr::kable() #Show results in table.





















