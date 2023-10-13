library(quantmod)
library(ggplot2)

# Put Call Parity
# Call - Put = StockPrice - Strike/(1 + riskFreeRate)
# Let's consider the American option data as if they were European options.

# Define the parameters
stock_price <- 130.21 # price on 2023-08-10
risk_free_rate <- 0.002420839

call_price <- read.csv("data/csv/call.csv", header = TRUE)
put_price <- read.csv("data/csv/put.csv", header = TRUE)

columns_to_keep <- names(call_price)[names(call_price) >= 'x116' & names(call_price) <= 'x136'] # the last value is not included
call_price <- t(call_price[1, columns_to_keep])
put_price <- t(put_price[1, columns_to_keep])

strike_prices <- data.frame(Strike_Price = as.vector(seq(116, 135, by = 1)))

# Calculate the right-hand side (RHS) of put-call parity
RHS <- stock_price - strike_prices / (1 + risk_free_rate)
LHS <- data.frame(Left_Side = as.vector(call_price - put_price))

put_call_parity <- ggplot(strike_prices, aes(x = Strike_Price)) +
                   geom_line(aes(y = LHS$Left_Side, color = "C - P")) +
                   geom_line(aes(y = RHS$Strike_Price, color = "S - K/(1 + r)")) +
                   labs(x = "Strike Price", y = "Option Price", title = "Put-Call Parity approximation for American Options") +
                   scale_color_manual(values = c("S - K/(1 + r)" = "blue", "C - P" = "red")) +
                   theme_minimal()

print(put_call_parity)
ggsave("data/png/put_call_parity.png", plot = put_call_parity, width = 10, height = 6)

upper_bound <- data.frame(Upper_Value = as.vector(call_price - stock_price + strike_prices))
put <- data.frame(Put_Value = as.vector(put_price))
lower_bound <- data.frame(Lower_Value = as.vector(call_price - stock_price + strike_prices/(1 + risk_free_rate)^20))

put_call_inequality <- ggplot(strike_prices, aes(x = Strike_Price)) +
                       geom_line(aes(y = upper_bound$Strike_Price, color = "C - S + K")) +
                       geom_line(aes(y = put$Put_Value, color = "P")) +
                       geom_line(aes(y = lower_bound$Strike_Price, color = "C - S + K/(1+r)^N")) +
                       labs(x = "Strike Price", y = "Option Price", title = "Put-Call Inequality for American Options") +
                       scale_color_manual(values = c("C - S + K" = "blue", "P" = "green", "C - S + K/(1+r)^N" = "red")) +
                       theme_minimal()

print(put_call_inequality)
ggsave("data/png/put_call_inequality.png", plot = put_call_inequality, width = 10, height = 6)