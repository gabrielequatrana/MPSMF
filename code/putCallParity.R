################################################################################
# Libraries
################################################################################

# Reading Libraries
library(quantmod)
library(ggplot2)
library(stringr)


################################################################################
# Script commands
################################################################################

# Put Call Parity
# Call - Put = StockPrice - Strike/(1 + riskFreeRate)
# Let's consider the American option data as if they were European options.

# Get stock price at 2023-08-28
getSymbols("MSFT", from = "2023-08-28", to = "2023-08-29")
price <- Ad(get("MSFT"))
price <- data.frame(Adjusted = as.vector(price))
stock_price <- price$Adjusted

# Read Risk-Free rate
risk_free_rate <- read.csv("data/txt/risk_free_rate.txt")
risk_free_rate <- as.numeric(risk_free_rate$x)

# Get options price
call_price <- read.csv("data/csv/call_opt_price.csv", header = TRUE)
put_price <- read.csv("data/csv/put_opt_price.csv", header = TRUE)

# TODO Bisonga selezionare un range perchè altimenti sono troppi (PROJ 2)
columns_to_keep <- names(call_price)[names(call_price) >= 'x116' & names(call_price) <= 'x136'] # The last value is not included
call_price <- t(call_price[1, columns_to_keep])
put_price <- t(put_price[1, columns_to_keep])

# Get strike dataframe
strike_prices <- data.frame(Strike_Price = as.numeric(columns_to_keep))

# Calculate the RHS and LHS of Put-Call parity equation
RHS <- stock_price - strike_prices / (1 + risk_free_rate)
LHS <- data.frame(Left_Side = as.vector(call_price - put_price))

# Print and save Put-Call Parity plot
put_call_parity <- ggplot(strike_prices, aes(x = Strike_Price)) +
  geom_line(aes(y = LHS$Left_Side, color = "C - P")) +
  geom_line(aes(y = RHS$Strike_Price, color = "S - K/(1 + r)")) +
  labs(x = "Strike Price", y = "Option Price", title = "Put-Call Parity approximation for American Options") +
  scale_color_manual(values = c("S - K/(1 + r)" = "blue", "C - P" = "red")) +
  theme_minimal()

print(put_call_parity)
ggsave("data/png/put_call_parity.png", plot = put_call_parity, width = 10, height = 6)

# Compute upper and lower bounds for the plot
upper_bound <- data.frame(Upper_Value = as.vector(call_price - stock_price + strike_prices))
lower_bound <- data.frame(Lower_Value = as.vector(call_price - stock_price + strike_prices/(1 + risk_free_rate)^20))

# Get put price dataframe
put <- data.frame(Put_Value = as.vector(put_price))

# Print and save Put-Call Inequality plot
put_call_inequality <- ggplot(strike_prices, aes(x = Strike_Price)) +
  geom_line(aes(y = upper_bound$Strike_Price, color = "C - S + K")) +
  geom_line(aes(y = put$Put_Value, color = "P")) +
  geom_line(aes(y = lower_bound$Strike_Price, color = "C - S + K/(1+r)^N")) +
  labs(x = "Strike Price", y = "Option Price", title = "Put-Call Inequality for American Options") +
  scale_color_manual(values = c("C - S + K" = "blue", "P" = "green", "C - S + K/(1+r)^N" = "red")) +
  theme_minimal()

print(put_call_inequality)
ggsave("data/png/put_call_inequality.png", plot = put_call_inequality, width = 10, height = 6)
