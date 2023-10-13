library(quantmod)

# TODO Market Yield on U.S. Treasury Securities at 1-Month Constant Maturity
treasury_ticker <- "DGS1MO"
getSymbols(treasury_ticker, src = "FRED", from = "2023-08-08", to = "2023-08-08")

bond_yield <- get(treasury_ticker) # percentage
bond_yield <- as.numeric(bond_yield)/100 # decimal

cat("\tRisk-free rate:\t\t\t\t\t", bond_yield, "\n")
write.csv(bond_yield, file = "data/txt/risk_free_rate.txt", row.names = FALSE)

annualized_yield <- (1 + as.numeric(bond_yield))^12 - 1

cat("\tAnnualized risk-free rate:\t\t\t", annualized_yield, "\n")
write.csv(annualized_yield, file = "data/txt/annualized_risk_free_rate.txt", row.names = FALSE)

# TODO Value of 10 Years Treasury Constant Maturity Rate
treasury_ticker <- "DGS10"
getSymbols(treasury_ticker, src = "FRED", from = "2020-08-08", to = "2023-08-08")
bond_yield <- get(treasury_ticker)
bond_yield <- diff(bond_yield) / bond_yield[-length(bond_yield)]
bond_yield <- na.omit(bond_yield)
bond_yield <- as.numeric(mean(bond_yield))

cat("\tRisk-free rate based on 10-Year T Bill value:\t", bond_yield, "\n")
write.csv(bond_yield, file = "data/txt/10_year_risk_free_rate.txt", row.names = FALSE)
