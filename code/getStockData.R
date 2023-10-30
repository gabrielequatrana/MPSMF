################################################################################
# Libraries
################################################################################

# Reading Libraries
library(quantmod)


################################################################################
# Script commands
################################################################################

# Set stock parameter
stock_name <- "MSFT"
start_date <- as.Date("2021-08-28")
end_date <- as.Date("2023-08-28")

# Download stock data
getSymbols(stock_name, src = "yahoo", from = start_date, to = end_date)

# Get stock information
adjusted_prices <- Ad(get(stock_name))
opening_prices <- Op(get(stock_name))
high_prices <- Hi(get(stock_name))
low_prices <- Lo(get(stock_name))
closing_prices <- Cl(get(stock_name))
volume <- Vo(get(stock_name))

# Create dataframe
stock_data <- data.frame(Date = index(closing_prices),
                         Adjusted = as.vector(adjusted_prices),
                         Opening = as.vector(opening_prices),
                         High = as.vector(high_prices),
                         Low = as.vector(low_prices),
                         Close = as.vector(closing_prices),
                         Volume = as.vector(volume))

# Save the downloaded file as csv
write.csv(stock_data, file = "data/csv/MSFT_data.csv", row.names = FALSE)
