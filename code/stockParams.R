################################################################################
# Libraries
################################################################################

# Reading Libraries
library(ggplot2)
library(readr)


################################################################################
# Script commands
################################################################################

# Read stock data file
stock_data <- read.csv("data/csv/MSFT_data.csv")

# Create stock dataframe
stock_data <- data.frame(Date = as.Date(stock_data$Date),
                         Adjusted = as.vector(stock_data$Adjusted),
                         Opening = as.vector(stock_data$Opening),
                         High = as.vector(stock_data$High),
                         Low = as.vector(stock_data$Low),
                         Close = as.vector(stock_data$Close),
                         Volume = as.vector(stock_data$Volume))

# Print and save Stock Price plot
stock_graph <- ggplot(data = stock_data, aes(x = Date)) +
  geom_line(aes(y = Adjusted, color = "Adjusted")) +
  labs(title = "Stock Price", x = "Date", y = "Price", color = "Metric") +
  scale_color_manual(values = c("Adjusted" = "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(stock_graph)
ggsave("data/png/stock_price.png", plot = stock_graph, width = 10, height = 4)

# Crate stock Returns dataframe
dates <- stock_data$Date[-1] # remove first date
log_returns_data <- data.frame(Date = as.Date(dates),
                           Adjusted = as.vector(diff(log(stock_data$Adjusted))),
                           Opening = as.vector(diff(log(stock_data$Opening))),
                           High = as.vector(diff(log(stock_data$High))),
                           Low = as.vector(diff(log(stock_data$Low))),
                           Close = as.vector(diff(log(stock_data$Close))))

# Print and save Stock Logarithmic Returns 
# TODO (log(S(n+1)/S(n)))
log_returns_graph <- ggplot(data = log_returns_data, aes(x = Date)) +
  geom_line(aes(y = Adjusted, color = "Adjusted")) +
  labs(title = "Logarithmic returns", x = "Date", y = "Returns", color = "Metric") +
  scale_color_manual(values = c("Adjusted" = "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(log_returns_graph)
ggsave("data/png/stock_log_returns.png", plot = log_returns_graph, width = 10, height = 4)

# TODO Compute Volatility (Standard Deviation of returns)
volatility <- sd(returns_data$Adjusted, na.rm = TRUE)
cat("\tVolatility:\t\t", volatility, "\n")
write.csv(volatility, file = "data/txt/volatility.txt", row.names = FALSE)

# TODO 252 Compute Annualized Volatility
annualized_volatility <- sqrt(252) * volatility
cat("\tAnnualized Volatility:\t", annualized_volatility, "\n")
write.csv(annualized_volatility, file = "data/txt/annualized_volatility.txt", row.names = FALSE)
