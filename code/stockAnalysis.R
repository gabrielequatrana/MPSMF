################################################################################
# Libraries
################################################################################

# Reading Libraries
library(ggplot2)
library(readr)
library(scales)


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
  labs(title = "MSFT Adjusted Stock Price (Aug 28 2021 - Aug 28 2023)",
       x = "Date", y = "Price", color = "Metric") +
  scale_color_manual(values = c("Adjusted" = "darkcyan")) +
  theme_minimal() +
  scale_x_date(date_labels = "%d/%m/%Y", date_breaks = "2 months",
               limits = c(min(stock_data$Date), max(stock_data$Date)), expand = c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

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

# Print and save Stock Logarithmic Returns plot
log_returns_graph <- ggplot(data = log_returns_data, aes(x = Date)) +
  geom_line(aes(y = Adjusted, color = "Adjusted")) +
  labs(title = "MSFT Logarithmic Returns (Aug 28 2021 - Aug 28 2023)",
       x = "Date", y = "Returns", color = "Metric") +
  scale_color_manual(values = c("Adjusted" = "darkcyan")) +
  theme_minimal() +
  scale_x_date(date_labels = "%d/%m/%Y", date_breaks = "2 months",
               limits = c(min(log_returns_data$Date), max(log_returns_data$Date)),
               expand = c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(log_returns_graph)
ggsave("data/png/stock_log_returns.png", plot = log_returns_graph, width = 10, height = 4)

# Compute Volatility (Standard Deviation of returns)
volatility <- sd(log_returns_data$Adjusted, na.rm = TRUE)
cat("\tVolatility:\t\t", volatility, "\n")
write.csv(volatility, file = "data/txt/volatility.txt", row.names = FALSE)
