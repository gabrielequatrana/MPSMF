##############################################################################################################################
##### Reading libraries
library(quantmod)


##############################################################################################################################
##### Environment Setting

# Remove all items in Global Environment
#rm(list=ls())

# Clear all Plots
#try(dev.off(),silent=TRUE)

# Clear the Console
#cat(rep("\n",100))

# Set the working directory
#WD <- dirname(rstudioapi::getSourceEditorContext()$path)
#setwd(WD)
WD <- getwd()

##############################################################################################################################
##### Get option chain

# Set data directory
CSV_folder <- paste(WD, "/csv/", sep = "")
RData_folder <- paste(WD, "/rdata/", sep = "")

# Set data extension
CSV_ext <- ".csv"
RData_ext <- ".RData"

# Get current day
Day <- Sys.Date()

# Set data path and names for ".csv" files
Calls_csv <- paste(CSV_folder, "MSFT_calls_options_", Day, CSV_ext, sep = "")
Puts_csv <- paste(CSV_folder, "MSFT_puts_options_", Day, CSV_ext, sep = "")
Straddle_csv <- paste(CSV_folder, "MSFT_straddle_options_", Day, CSV_ext, sep = "")

# Set data path and names for ".RData" files
Calls_rdata <- paste(RData_folder, "MSFT_calls_options_", Day, RData_ext, sep = "")
Puts_rdata <- paste(RData_folder, "MSFT_puts_options_", Day, RData_ext, sep = "")
Straddle_rdata <- paste(RData_folder, "MSFT_straddle_options_", Day, RData_ext, sep = "")

# Get Option Chain
MSFT_Option_Chain <- getOptionChain("MSFT", Exp = "2023-09-15", src = "yahoo")

# Get calls and puts data frame
Calls_df <- MSFT_Option_Chain[["calls"]]
Puts_df <- MSFT_Option_Chain[["puts"]]

# Set straddle data frame
Strike <- sort(union(Calls_df$Strike, Puts_df$Strike))
Call_Indx <- sapply(Strike, function(x) which(Calls_df$Strike==x)[1])
Put_Indx <- sapply(Strike, function(x) which(Puts_df$Strike==x)[1])
Straddle_df <- data.frame(Indx=1:length(Strike),
                          Call_ContractID=Calls_df$ContractID[Call_Indx], 
                          Call_Bid=Calls_df$Bid[Call_Indx],
                          Call_Ask=Calls_df$Ask[Call_Indx],
                          Call_Vol=Calls_df$Vol[Call_Indx],
                          Call_OI=Calls_df$OI[Call_Indx],
                          Call_PrChg=Calls_df$Chg[Call_Indx],
                          Call_PrChgPct=Calls_df$ChgPct[Call_Indx],
                          Call_LastTrTime=Calls_df$LastTradeTime[Call_Indx],
                          Call_LastPr=Calls_df$Last[Call_Indx],
                          Call_ImplVol=Calls_df$IV[Call_Indx],
                          Call_ITM=Calls_df$ITM[Call_Indx],
                          Strike=Strike,
                          Put_ITM=Puts_df$ITM[Put_Indx],
                          Put_ImplVol=Puts_df$IV[Put_Indx],
                          Put_LastPr=Puts_df$Last[Put_Indx],
                          Put_LastTrTime=Puts_df$LastTradeTime[Put_Indx],
                          Put_PrChgPct=Puts_df$ChgPct[Put_Indx],
                          Put_PrChg=Puts_df$Chg[Put_Indx],
                          Put_OI=Puts_df$OI[Put_Indx],
                          Put_Vol=Puts_df$Vol[Put_Indx],
                          Put_Ask=Puts_df$Ask[Put_Indx],
                          Put_Bid=Puts_df$Bid[Put_Indx],
                          Put_ContractID=Puts_df$ContractID[Put_Indx])

# Remove Option Chain list
rm(MSFT_Option_Chain)

# Save the downloaded file as ".RData"
save(Calls_df, file = Calls_rdata)
save(Puts_df, file = Puts_rdata)
save(Straddle_df, file = Straddle_rdata)

# Save the downloaded file as ".csv"
write.csv(Calls_df, Calls_csv, row.names = FALSE)
write.csv(Puts_df, Puts_csv, row.names = FALSE)
write.csv(Straddle_df, Straddle_csv, row.names = FALSE)
