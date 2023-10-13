library(ggplot2)
library(readr)

stock_data <- read.csv("output/MSFT_data.csv")

# Create dataframe
stock_data <- data.frame(Date = as.Date(stock_data$Date),
                         Adjusted = as.vector(stock_data$Adjusted),
                         Opening = as.vector(stock_data$Opening),
                         High = as.vector(stock_data$High),
                         Low = as.vector(stock_data$Low),
                         Close = as.vector(stock_data$Close),
                         Volume = as.vector(stock_data$Volume))

# Stock Price
stock_graph <- ggplot(data = stock_data, aes(x = Date)) + 
               geom_line(aes(y = Adjusted, color = "Adjusted")) + 
               labs(title = "Stock Price", x = "Date", y = "Price", color = "Metric") +
               scale_color_manual(values = c("Adjusted" = "blue")) +
               theme_minimal()

print(stock_graph)
ggsave("output/stock.png", plot = stock_graph, width = 10, height = 4)

# Returns dataframe
dates <- stock_data$Date[-1] # remove first date
returns_data <- data.frame(Date = as.Date(dates),
                           Adjusted = as.vector(diff(log(stock_data$Adjusted))),
                           Opening = as.vector(diff(log(stock_data$Opening))),
                           High = as.vector(diff(log(stock_data$High))),
                           Low = as.vector(diff(log(stock_data$Low))),
                           Close = as.vector(diff(log(stock_data$Close))))

# Logarithmic Returns log(S(n+1)/S(n))
returns_graph <- ggplot(data = returns_data, aes(x = Date)) +
                 geom_line(aes(y = Adjusted, color = "Adjusted")) +
                 labs(title = "Logarithmic returns", x = "Date", y = "Returns", color = "Metric") +
                 scale_color_manual(values = c("Adjusted" = "blue",
                                               "Open" = "red",
                                               "High" = "green",
                                               "Low" = "purple",
                                               "Close" = "orange")) +
                 theme_minimal()

print(returns_graph)
ggsave("output/returns.png", plot = returns_graph, width = 10, height = 4)

# Volatility (Standard Deviation of returns)
volatility <- sd(returns_data$Adjusted, na.rm = TRUE)
cat("\tVolatility:\t\t", volatility, "\n")
write.csv(volatility, file = "output/volatility.txt", row.names = FALSE)

annualized_volatility <- sqrt(252) * volatility
cat("\tAnnualized Volatility:\t", annualized_volatility, "\n")
write.csv(annualized_volatility, file = "output/annualized_volatility.txt", row.names = FALSE)
