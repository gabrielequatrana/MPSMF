################################################################################
# Environment Setting
################################################################################

# Removing all items in Global Environment
rm(list=ls())

# Clearing all Plots
try(dev.off(),silent=TRUE)

# Clearing the Console
cat(rep("\n",100))

# Get the path of the currently running script
proj_dir <- dirname(normalizePath(rstudioapi::getSourceEditorContext()$path))

# Set the working directory to the root directory
setwd(dirname(proj_dir))


################################################################################
# Libraries
################################################################################

# Reading Libraries
library(quantmod)
library(ggplot2)
library(readr)
library(fs)
library(reshape2)
library(tidyr)
library(dplyr)
library(stringr)
library(latticeExtra)


################################################################################
# Run program
################################################################################

# Run getStockData.R
cat("Get MSFT stock data from Yahoo Finance\n")
source("code/getStockData.R")

# Run stockParams.R
cat("\nCompute Stock Prices and Volatility on previous data\n")
source("code/stockAnalysis.R")

# Run riskFreeRate.r
cat("\nCompute an approximation of Risk Free Return Rate\n")
source("code/riskFreeRate.R")

# Run CRRModel.R
cat("\nEstimate the value of Put and Call options with the CRR model and compare price data to the CRR binomial tree\n")
source("code/CRRModel.R")

# Run putCallParity.R
cat("\nCompute the put call parity function over the options data retrieved\n")
source("code/putCallParity.R")
