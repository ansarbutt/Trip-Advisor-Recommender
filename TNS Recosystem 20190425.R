# Sources: 
# https://www.softserveinc.com/de-de/blogs/recommender-comparison-algorithm-de/
# https://cran.r-project.org/web/packages/recosystem/vignettes/introduction.html
# https://rstudio-pubs-static.s3.amazonaws.com/288275_c115bc2ebc794d89a71b5654905beddc.html

install.packages('recosystem')
library('recosystem')
library('dplyr')
library('tidyverse')

set.seed(0)

# Read file into dataframe
Attractions_Reviews_final <- read_csv("G:/Ansar/TripAdvisor/Attractions_Reviews_final.csv")

# Create ID for each user and attraction
Attractions_Reviews_final$user_id <- c(as.factor(Attractions_Reviews_final$user_name))
Attractions_Reviews_final$attraction_id <- c(as.factor(Attractions_Reviews_final$Attr_ID))

# Create indicator to specify rows for training and testing
Attractions_Reviews_final$train_ind <- rep(TRUE, nrow(Attractions_Reviews_final))
Attractions_Reviews_final$train_ind[sample(1:nrow(Attractions_Reviews_final),  size = 0.2*nrow(Attractions_Reviews_final))] <- FALSE

# Create training and testing dataframes while selecting relevant rows
train_ratings <- Attractions_Reviews_final %>%
  filter(train_ind == TRUE) %>%
  select(user_id, attraction_id, Ratings)

test_ratings <- Attractions_Reviews_final %>%
  filter(train_ind == FALSE) %>%
  select(user_id, attraction_id, Ratings)

# Write to table (required data format for recosystem)
write.table(train_ratings, file = "trainset.txt", sep = " ", row.names = FALSE, col.names = FALSE)
write.table(test_ratings, file = "testset.txt", sep = " ", row.names = FALSE, col.names = FALSE)

# Initialize recommender
r = Reco()
# Perform tuning on training set
opts <- r$tune("trainset.txt", opts = list(dim = c(1:20), lrate = c(0.05),
                                           nthread = 4, costp_l1 = 0, costq_l1 = 0, niter = 200, nfold = 10, verbose = FALSE))
# Train model
r$train("trainset.txt", opts = c(opts$min, nthread = 4, niter = 500, verbose = FALSE))
# Save model
saveRDS(r, "./final_model.rds")

# Make predictions on test set and calculate error (RMSE)
outfile = tempfile()
r$predict("testset.txt", outfile)
scores_real <- read.table("testset.txt", header = FALSE, sep = " ")$V3
scores_pred <- scan(outfile)
rmse_mf <- sqrt(mean((scores_real-scores_pred) ^ 2))
rmse_mf

# Get user IDs from training set (for users who have more than 2 ratings)
users_3plus_ratings <- train_ratings %>% 
  group_by(user_id) %>%
  tally() %>%
  filter(n > 2)  %>%
  distinct(user_id)
# Store user IDs as a numeric vector (required for grid expansion)
users_3plus_ratings <- as.numeric(unlist(users_3plus_ratings))

# Get attraction IDs from training set 
attr_ids <- train_ratings %>%
  distinct(attraction_id)
# Store attraction IDs as a numeric vector (required for grid expansion)
attr_ids <- as.numeric(unlist(attr_ids))

# Expand the user and attraction ID list (so each user will have a row with every attraction) 
pred <- expand.grid(user = users_3plus_ratings, attraction = attr_ids)
# Store the data into memory for predictions
pred_set <- data_memory(pred$user, pred$attraction, index1 = TRUE)
# Predict the user rating of each user/attraction combination
pred$rating <- r$predict(pred_set, out_memory())

# Load the file with the attraction ID to name mapping
Attractions_High_Level <- read_csv("G:/Ansar/TripAdvisor/Attractions_HighLevel.csv")

# Create a dataframe with the attraction IDs and their respective attraction names
attr_name_map <- distinct(Attractions_Reviews_final, Attr_ID, attraction_id) %>%
  inner_join(Attractions_High_Level, by = c("Attr_ID" = "Attraction_Id")) %>%
  select(Attr_ID, attraction_id, Attraction_Nm)

# Getting the current ratings for a specific user
user_curr_ratings <- train_ratings %>% 
  group_by(user_id) %>%
  filter(user_id == 18) %>%
  inner_join(attr_name_map, by = c("attraction_id" = "attraction_id")) %>%
  select(user_id, Attraction_Nm, Ratings)

# Get the recommendations for a specific user (and filter ratings for attractions that have been already rated)
user_reco <- pred %>% 
  group_by(user) %>%
  filter(user == 18) %>%
  filter(!attraction %in% user_curr_ratings$attraction_id) %>%
  inner_join(attr_name_map, by = c("attraction" = "attraction_id")) %>%
  select(user, Attraction_Nm, rating) %>%
  arrange(desc(rating)) 

 



