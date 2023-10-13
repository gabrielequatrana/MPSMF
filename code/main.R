# Get the path of the currently running script
script_path <- sys.frame(1)$ofile

# Set the working directory to the directory containing the script
setwd(dirname(script_path))

# Get MSFT data from Yahoo Finance
cat("Executing getData.R\n")
source("getData.R")

# Compute Stock Prices, Volatility and Returns on previous data
cat("Executing getParams.R\n")
source("getParams.R")

# Compute an approximation of risk free rate both inflation adjusted and not
cat("Executing riskFreeRate.R\n")
source("riskFreeRate.R")

# Use the CRR model to estimate the value of Put and Call options, compare price data to the CRR binomial tree 
cat("Executing CRRModel.R\n")
source("CRRModel.R")

# Compute the put call parity function over the options data retrieved
cat("Executing putCallParity.R\n")
source("putCallParity.R")