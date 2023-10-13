# Cox Ross Rubinstein Model (Multi-Period Multiplicative Binomial Model)

# at the end of every one of the negotiation periods, we can have one of two outcomes: 
#   - positive (prob p)
#   - negative (prob q = 1-p)
# every result is independent from every other
# => P(w) = p^k q^(n-k)

library(ggplot2)
library(fs)
library(reshape2)
library(tidyr)
library(quantmod)
library(dplyr)

#### 0. RETRIEVE DATA AND INITIALIZE CONSTANTS ####