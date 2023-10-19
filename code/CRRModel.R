################################################################################
# Libraries
################################################################################

# Reading Libraries
library(ggplot2)
library(fs)
library(reshape2)
library(tidyr)
library(quantmod)
library(dplyr)
library(stringr)


################################################################################
# TODO Comments Functions
################################################################################

# Generate binomial tree
generate_binomial_tree <- function(StartingValue, treeHeight){
  
  # Make binomial tree
  binomial_tree <- matrix(NA, nrow = treeHeight+1, ncol = treeHeight+1)
  binomial_tree[1,1] <- StartingValue
  
  # Fill the tree
  for (n in 0:treeHeight) {
    for (k in 0:n) {
      S <- round(StartingValue*(u^k)*(d^(n-k)), 2)
      binomial_tree[n+1, k+1] <- S
    }
  }
  return(binomial_tree)
}

# Compute call option value
call_value <- function(StartingValue, treeHeight, Strike) {
  
  # Generate call binomial tree
  binomial_tree <- generate_binomial_tree(StartingValue, treeHeight)
  call_tree <- matrix(NA, nrow = treeHeight+1, ncol = treeHeight+1)
  
  # Compute values in the tree
  for(i in 1:(treeHeight+1)){
    # Call option: max{(Sn − K), 0}
    call_tree[treeHeight+1, i] <- max(binomial_tree[treeHeight+1, i] - Strike, 0)
  }
  
  # Backwards induction
  for (i in treeHeight:1) {
    for (j in 1:i) {
      call_tree[i, j] <- round(exp(-r*delta_t)*(p_tilde*call_tree[i+1, j+1] + q_tilde*call_tree[i+1, j]), 2)
    }
  }
  
  return(call_tree[1,1])
}

# Compute put option value
put_value <- function(StartingValue, treeHeight, Strike) {
  
  # Generate call binomial tree
  binomial_tree <- generate_binomial_tree(StartingValue, treeHeight)
  put_tree <- matrix(NA, nrow = treeHeight+1, ncol = treeHeight+1)
  
  # Compute values in the tree
  for(i in 1:(treeHeight+1)){
    # Put option: max{(K − Sn), 0}
    put_tree[treeHeight+1, i] <- max(Strike - binomial_tree[treeHeight+1, i], 0)
  }
  
  # Backwards induction
  for (i in treeHeight:1) {
    for (j in 1:i) {
      put_tree[i, j] <- round(exp(-r*delta_t)*(p_tilde*put_tree[i+1, j+1] + q_tilde*put_tree[i+1, j]), 2)
    }
  }
  
  return(put_tree[1,1])
}


################################################################################
# Script commands
################################################################################

# Cox Ross Rubinstein Model (Multi-Period Multiplicative Binomial Model)

# At the end of every one of the negotiation periods, we can have one of two outcomes: 
#   - u: positive (prob p)
#   - d: negative (prob q = 1-p)

# Every result is independent from every other
# => P(w) = p^k q^(n-k)


################################################################################
# 1. Model Parameters
################################################################################

# Read stock data until 2023-08-28
stock_data <- read.csv("data/csv/MSFT_data.csv")
stock_data <- data.frame(Date = as.Date(stock_data$Date),
                         Adjusted = as.vector(stock_data$Adjusted))

# Get stock_data from 2023-08-28 to 2023-09-15
getSymbols("MSFT", from = "2023-08-28", to = "2023-09-16")
real_prices <- Ad(get("MSFT"))
real_prices <- data.frame(Adjusted = as.vector(real_prices))

# Read Risk-Free rate
r <- read.csv("data/txt/10_year_risk_free_rate.txt")
r <- as.numeric(r$x)

# Read stock volatility
sigma <- read.csv("data/txt/volatility.txt")
sigma <- as.numeric(sigma$x)

# Set some constant
S0 <- real_prices$Adjusted[1] # Set S0 to first adjusted real price
N <- 15                       # Trading days from 2023-08-28 to 2023-09-15
delta_t <- 1                  # One day in a trading year

# Set the two possible outcomes
u <- exp(sigma*sqrt(delta_t))
d <- 1/u

# Compute Risk-Neutral probability
p_tilde <- (1+r-d)/(u-d)
q_tilde <- 1-p_tilde

# Print computed model parameters
cat("\tStarting Stock Price:\t", S0, "\n")
cat("\tVolatility:\t\t", sigma, "\n")
cat("\tRisk-free rate:\t\t", r, "\n")
cat("\tUp:\t\t\t", u, "\n")
cat("\tDown:\t\t\t", d, "\n")
cat("\tP tilde:\t\t", p_tilde, "\n")
cat("\tQ tilde:\t\t", q_tilde, "\n")


################################################################################
# 2. Get and print options data
################################################################################

# Set file paths
calls_file_name <- "MSFT_calls_options"
puts_file_name <- "MSFT_puts_options"
calls_parent_folder <- paste0(getwd(),"/data/csv/options/calls/")
puts_parent_folder <- paste0(getwd(),"/data/csv/options/puts/")

# Read last Call and Put files (2023-09-15)
last_call_observation <- read.csv(paste0(calls_parent_folder, calls_file_name, "_2023-09-15.csv"))
last_put_observation <- read.csv(paste0(puts_parent_folder, puts_file_name, "_2023-09-15.csv"))

# Generate dataframe from previously read files
last_call_data <- data.frame(Strike = as.vector(last_call_observation$Strike),
                             Last = as.vector(last_call_observation$Last))
last_put_data <- data.frame(Strike = as.vector(last_put_observation$Strike),
                            Last = as.vector(last_put_observation$Last))

# Generate dataframe with Strike values as columns
call_dataframe <- data.frame(t(last_call_data$Last))
colnames(call_dataframe) <- last_call_data$Strike

put_dataframe <- data.frame(t(last_put_data$Last))
colnames(put_dataframe) <- last_put_data$Strike

# Set the date in a new column
call_dataframe$Date <- as.Date("2023-09-15")
call_column_names <- colnames(call_dataframe)

put_dataframe$Date <- as.Date("2023-09-15")
put_column_names <- colnames(put_dataframe)

# Read Call option files
calls_file = list.files(calls_parent_folder)
for (file in calls_file[-length(puts_file)]) {
  
  # Read Call file and generate a dataframe
  call_observation_data <- read.csv(paste0(calls_parent_folder, file))
  call_observation_dataframe <- data.frame(Strike = as.vector(call_observation_data$Strike),
                                           Last = as.vector(call_observation_data$Last))
  
  # Add a new row with NA values to the original dataframe
  new_row <- data.frame(matrix(NA, ncol = length(call_column_names)))
  colnames(new_row) <- call_column_names
  call_dataframe <- rbind(call_dataframe, new_row)
  
  # Update Date column
  call_dataframe[nrow(call_dataframe), "Date"] <- as.Date(str_sub(file, -14, -5))
  
  # Get the data from the current file in the correct format
  curr_call_row <- data.frame(t(call_observation_dataframe$Last))
  colnames(curr_call_row) <- call_observation_dataframe$Strike
  
  # Update last row of dataframe with current file
  call_dataframe[nrow(call_dataframe), names(curr_call_row)] <- unlist(curr_call_row)
}
  
# Read Put option files
puts_file = list.files(puts_parent_folder)
for (file in puts_file[-length(puts_file)]) {

  # Read Put file and generate a dataframe
  put_observation_data <- read.csv(paste0(puts_parent_folder, file))
  put_observation_dataframe <- data.frame(Strike = as.vector(put_observation_data$Strike),
                                          Last = as.vector(put_observation_data$Last))
  
  # Add a new row with NA values to the original dataframe
  new_row <- data.frame(matrix(NA, ncol = length(put_column_names)))
  colnames(new_row) <- put_column_names
  put_dataframe <- rbind(put_dataframe, new_row)
  
  # Update Date column
  put_dataframe[nrow(put_dataframe), "Date"] <- as.Date(str_sub(file, -14, -5))
  
  # Get the data from the current file in the correct format
  curr_put_row <- data.frame(t(put_observation_dataframe$Last))
  colnames(curr_put_row) <- put_observation_dataframe$Strike
  
  # Update last row of dataframe with current file
  put_dataframe[nrow(put_dataframe), names(curr_put_row)] <- unlist(curr_put_row)
}

# Save call option dataframe in csv file
call_dataframe <- call_dataframe[order(call_dataframe$Date), ]
write.csv(call_dataframe, file = "data/csv/call_opt_price.csv", row.names = FALSE)
melted_call_dataframe <- melt(call_dataframe, id.vars = 'Date', variable.name = 'strike')

# Print and save call option price plot
call_price_graph <- ggplot(melted_call_dataframe, aes(Date,value)) +
  geom_line(aes(colour = strike)) +
  labs(title = "Call Options Price History", x = "Date", y = "Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(call_price_graph)
ggsave("data/png/call_price_graph.png", plot = call_price_graph, width = 10, height = 6)

# Save put option dataframe in csv file
put_dataframe <- put_dataframe[order(put_dataframe$Date), ]
write.csv(put_dataframe, file = "data/csv/put_opt_price.csv", row.names = FALSE)
melted_put_dataframe <- melt(put_dataframe, id.vars = 'Date', variable.name = 'strike')

# Print and save put option price plot
put_price_graph <- ggplot(melted_put_dataframe, aes(Date,value)) + 
  geom_line(aes(colour = strike)) + 
  labs(title = "Put Options Price History", x = "Date", y = "Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(put_price_graph)
ggsave("data/png/put_price_graph.png", plot = put_price_graph, width = 10, height = 6)


################################################################################
# 3. Generate price binomial tree
################################################################################

# Generate price binomial tree using S0
price_tree <- generate_binomial_tree(S0, N)

# Convert the matrix to a dataframe
price_tree <- as.data.frame(price_tree)
price_tree$index <- 1:nrow(price_tree)
price_tree <- gather(price_tree, key = "Column", value = "Value", -index)

# Print and save price tree
tree_graph <- ggplot(data = real_prices, aes(x = seq_along(Adjusted), y = Adjusted)) +
  geom_line() +
  geom_point(data = price_tree, aes(x = index, y = Value)) +
  labs(title = "MSFT Adjusted Prices (Aug 28, 2023 - Sep 15, 2023)",
       x = "Number of Rows",
       y = "Adjusted Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(tree_graph)
ggsave("data/png/tree_graph.png", plot = tree_graph, width = 10, height = 6)

# Refactor price binomial tree
price_tree <- na.omit(price_tree)
price_tree <- price_tree[order(price_tree$index), ]
price_tree <- subset(price_tree, select = -Column)

# Compute the difference between the real value and the closest predicted value
difference <- list()
for(i in 1:nrow(real_prices)){
  curr_predicted_values <- subset(price_tree, index == i)
  curr_predicted_values$Value <- abs(curr_predicted_values$Value - real_prices$Adjusted[i])
  min <- min(curr_predicted_values$Value)
  difference <- c(difference, min)
}
difference <- data.frame(Value = unlist(difference))

# Print and save difference plot
difference_graph <- ggplot(difference, aes(x = seq_along(Value), y = Value)) +
  geom_line() +
  labs(title = "Difference between actual stock value and closest predicted value",
       x = "",
       y = "Values") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(difference_graph)
ggsave("data/png/difference_graph.png", plot = difference_graph, width = 10, height = 6)

# Compute min, max and mean difference
min_difference <- min(difference$Value[2:length(difference$Value)])
max_difference <- max(difference$Value[2:length(difference$Value)])
avg_difference <- mean(difference$Value[2:length(difference$Value)])

# Print differences
cat("\n\tDifference between predicted and actual stock value:\n")
cat("\tMin:\t\t", min_difference, "\n")
cat("\tMax:\t\t", max_difference, "\n")
cat("\tAverage:\t", avg_difference, "\n")


################################################################################
# 4. Compute Call and Put options values with current CRR model
################################################################################

# TODO (A lui è 21 a me 15 vedere se si hanno problemi con ultimo giorno)
# TODO (Perche tra 125 e 135?) DO THE FOLLOWING FOR ALL VALUES BETWEEN 125 AND 135

# Get dataframe based on selected columns
columns_to_keep <- names(call_dataframe)[names(call_dataframe) >= '125' & names(call_dataframe) <= '135']
call_dataframe <- call_dataframe[, columns_to_keep]
put_dataframe <- put_dataframe[, columns_to_keep]

# Repeat for each column
for(s in columns_to_keep){
  
  # Dataframe to store the values predicted by the CRR model
  predicted_call <- data.frame(CallValue = numeric(0))
  predicted_put <- data.frame(PutValue = numeric(0))
  
  # Fill the dataframes
  for(i in (N):1){
    StartingValue <- real_prices$Adjusted[N+2-i]
    treeHeight <- i
    
    predicted_call <- predicted_call %>% add_row(CallValue = call_value(StartingValue, treeHeight, as.numeric(s)))
    predicted_put <- predicted_put %>% add_row(PutValue = put_value(StartingValue, treeHeight, as.numeric(s)))
  }
  
  # Print and save call prediction
  call_prediction <- ggplot(data = predicted_call, aes(x = seq_along(CallValue))) +
    geom_line(aes(y = CallValue, color = "Estimate")) +
    geom_line(aes(y = call_dataframe[[s]], color = "Real")) +
    labs(title = paste("Call Value for Strike ", s), x = "Date", y = "Price", color = "Metric") +
    scale_color_manual(values = c("Estimate" = "blue", "Real" = "orange")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(call_prediction)
  ggsave(paste("data/png/call_prediction_strike_", s, ".png"), plot = call_prediction, width = 10, height = 4)
  
  # Print and save put prediction
  put_prediction <- ggplot(data = predicted_put, aes(x = seq_along(PutValue))) +
    geom_line(aes(y = PutValue, color = "Estimate")) +
    geom_line(aes(y = put_dataframe[[s]], color = "Real")) +
    labs(title = paste("Put Value for Strike ", s), x = "Date", y = "Price", color = "Metric") +
    scale_color_manual(values = c("Estimate" = "blue", "Real" = "orange")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(put_prediction)
  ggsave(paste("data/png/put_prediction_strike_", s, ".png"), plot = put_prediction, width = 10, height = 4)
}
