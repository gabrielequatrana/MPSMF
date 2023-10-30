################################################################################
# Libraries
################################################################################

# Reading Libraries
library(quantmod)


################################################################################
# Script commands
################################################################################

# Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity
treasury_bond <- "DGS10"
getSymbols(treasury_bond, src = "FRED", from = "2020-08-15", to = "2023-08-15")

# Compute and save Risk-Free Rate
bond_yield <- get(treasury_bond)
bond_yield <- diff(bond_yield) / bond_yield[-length(bond_yield)]
bond_yield <- na.omit(bond_yield)
bond_yield <- as.numeric(mean(bond_yield))
cat("\tRisk-free rate based on 10-Year Treasury Bill value:\t", bond_yield, "\n")
write.csv(bond_yield, file = "data/txt/risk_free_rate.txt", row.names = FALSE)
