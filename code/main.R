# Get the path of the currently running script
script_path <- sys.frame(1)$ofile

# Set the working directory to the root directory
setwd(dirname(dirname(script_path)))

# Get MSFT data from Yahoo Finance
cat("Executing getData.R\n")
source("code/getData.R")

# Compute Stock Prices, Volatility and Returns on previous data
cat("Executing getParams.R\n")
source("code/getParams.R")

# Compute an approximation of risk free rate both inflation adjusted and not
cat("Executing riskFreeRate.R\n")
source("code/riskFreeRate.R")

# Use the CRR model to estimate the value of Put and Call options, compare price data to the CRR binomial tree 
cat("Executing CRRModel.R\n")
source("code/CRRModel.R")

# Compute the put call parity function over the options data retrieved
cat("Executing putCallParity.R\n")
source("code/putCallParity.R")