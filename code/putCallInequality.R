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

# Put Call Inequality
# Call - StockPrice + Strike >= Put >= Call - StockPrice + Strike/(1 + riskFreeRate)^ObservationPeriod

# Set variables
N <- 14

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

# Get previously computed strikes to analyze
strike_to_keep <- read.csv("data/txt/strike_to_keep.txt")$x
strike_to_keep <- paste0("X", strike_to_keep)

# Get call and put prices based on strikes to analyze
call_price <- t(call_price[1, strike_to_keep])
put_price <- t(put_price[1, strike_to_keep])

# Get strike dataframe
strike_prices <- data.frame(Strike_Price = as.numeric(str_sub(strike_to_keep, 2)))

# Compute upper and lower bounds for the plot
upper_bound <- data.frame(Upper_Value = as.vector(call_price - stock_price + strike_prices))
lower_bound <- data.frame(Lower_Value = as.vector(call_price - stock_price + strike_prices/(1 + risk_free_rate)^N))

# Get put price dataframe
put <- data.frame(Put_Value = as.vector(put_price))

# Print and save Put-Call Inequality plot
put_call_inequality <- ggplot(strike_prices, aes(x = Strike_Price)) +
  geom_line(aes(y = upper_bound$Strike_Price, color = "C - S + K"), linewidth = 1) +
  geom_line(aes(y = put$Put_Value, color = "P"), linewidth = 1) +
  geom_line(aes(y = lower_bound$Strike_Price, color = "C - S + K/(1+r)^N"), linewidth = 1) +
  labs(x = "Strike Price", y = "Option Price", title = "MSFT Options Put-Call Inequality") +
  scale_color_manual(name = "", values = c("C - S + K" = "blue", "P" = "green", "C - S + K/(1+r)^N" = "red")) +
  scale_x_continuous(breaks = strike_prices$Strike_Price) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(put_call_inequality)
ggsave("data/png/put_call_inequality.png", plot = put_call_inequality, width = 10, height = 6)
