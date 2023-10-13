##############################################################################################################################
##### Reading libraries
library(quantmod)

##############################################################################################################################
##### Environment Setting

# Remove all items in Global Environment
rm(list=ls())

# Clear all Plots
try(dev.off(),silent=TRUE)

# Clear the Console
cat(rep("\n",100))

# Set the working directory
WD <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(WD)


##############################################################################################################################
##### Functions
na.rm <- function(x){x <- as.vector(x[!is.na(as.vector(x))])}


##############################################################################################################################
##### Get history data

# Set data directory
CSV_folder <- paste(WD, "/csv/", sep = "")
RData_folder <- paste(WD, "/rdata/", sep = "")

# Set data extension
CSV_ext <- ".csv"
RData_ext <- ".RData"

# Set data period
From <- as.Date("2021-08-28")
To <- as.Date("2023-08-28")

# Set data path and names
MSFT_csv <- paste(CSV_folder, "MSFT_", From, "__", To, CSV_ext, sep = "")
MSFT_rdata <- paste(RData_folder, "MSFT_", From, "__", To, RData_ext, sep = "")

# Download data
MSFT_df <- getSymbols.yahoo("MSFT", from = From, to = To, perodicity = "daily",
                            base.currency="USD", return.class = "data.frame",
                            env = .GlobalEnv, verbose = FALSE, warning = TRUE,
                            auto.assign = FALSE)

# Remove the rows with NA from the data frame *MSFT_df*
MSFT_df <- na.omit(MSFT_df)

# Add "Date" column to the data frame *MSFT_df*
MSFT_df <- add_column(.data=MSFT_df, Date=as.Date(rownames(MSFT_df),
                          format="%Y-%m-%d"), .before="MSFT.Open")

# Set the row names of the data frame *MSFT_df* to the default values
rownames(MSFT_df) <- NULL

# Add an "Index" column to the data frame *MSFT_df*
MSFT_df <- add_column(.data=MSFT_df, Index=1:nrow(MSFT_df), .before="Date")

# Save the downloaded file as ".RData" and as ".csv"
save(MSFT_df, file = MSFT_rdata)
write.csv(MSFT_df, MSFT_csv, row.names = FALSE)
