# load data into R programming
house_price_data <- read.csv("/Users/pannatadsoisup/Desktop/R programming/House Price India.csv")

# overview of the data distribution
hist(house_price_data$Price,
     prob = TRUE,
     main = "Histogram with density line",
     xlab = "Price",
     ylab = "Frequency",
     col = "lightblue",
     border = "red",
     breaks =  50)
  ## this data is right-skewed 
# check column = "Price" <= 0
house_price_data[house_price_data <= 0]
nrow(house_price_data[house_price_data <= 0])
  ## Price has not values <= 0
                 
# adjust distribution to more normal distribution
house_price_data$Price_log <- log(house_price_data$Price)
hist(house_price_data$Price_log,
     main = "Histogram more normal distribution",
     xlab = "Price",
     ylab = "Frequency",
     col = "gold",
     border = "green",
     breaks = 50)

# Subset column we focus
full_df <- house_price_data %>%
  select(Price_log, lot.area)

# Check NA
full_df %>%
  complete.cases() %>%
  mean()
 ## 1.split data (80% train, 20% test)
split_data <- function(full_df){
  set.seed(42)
  n <- nrow(full_df)
  train_id <- sample(1:n, size = n*0.8)
  train_df <- full_df[train_id, ]
  test_df <- full_df[-train_id, ]
  return(list(training = train_df,
              testing = test_df))
}

prep_data <- split_data(full_df)
train_df <- prep_data$training
test_df <- prep_data$testing

  ## 2. train model
set.seed(42)
lm_model <- train(Price_log ~.,
                  data = train_df,
                  method = "lm")
  ## 3.score model
prediction <- predict(lm_model, newdata = test_df)
  ## 4.evaluate
test_df$Price_log # Actual values
  ## 4.1 mae (mean absolute error)
mae <- mean(abs(prediction - test_df$Price_log))
  ## 4.2 mse (mean squared error)
rmse <- sqrt(mean((prediction - test_df$Price_log) ^2))

lm_model
mae
rmse
