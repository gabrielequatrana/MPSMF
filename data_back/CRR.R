################################################################################
# System setting
################################################################################

# Removing all items in Global Environment
rm(list=ls())

# Clearing all Plots
try(dev.off(),silent=TRUE)

# Clearing the Console
cat(rep("\n",100))

# Loading the system directory
Sys.getenv('PATH')

# Setting the working directory
WD <- dirname(rstudioapi::getSourceEditorContext()$path)
show(WD)
setwd(WD)
dir()

################################################################################
# Libraries
################################################################################

# Reading Libraries
library(timeDate)
library(timeSeries)
library(fBasics)
library(fOptions)
library(tibble)
library(data.table)
library(dplyr)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(urca)
library(moments)
library(lmtest)


# TODO

# Set data period
init_date  <- which(US_DTR_2020_2023_df$Date=="2021-08-28")
final_date <- which(US_DTR_2020_2023_df$Date=="2023-08-28")


################################################################################
# TODO Capire come trovare parametri
# Model Setting
################################################################################
r <- 0.05
u <- 1 + 0.15
d <- 1 - 0.05
p <- (1+r-d)/(u-d)
q <- (u-(1+r))/(u-d)
S_0 <- 100
K <- 100
N <- 5

################################################################################
# TODO Capire come fare con opzioni americane invece di europee
# American Options on Microsoft Corporation (Yahoo Finance - MSFT)
################################################################################

MSFT_Opt_date_df <- read.csv("test.csv")
class(MSFT_Opt_date_df)
head(MSFT_Opt_date_df, 10)
tail(MSFT_Opt_date_df, 10)

Call_LastTrDate_df <- data.frame(Call_LastTrDate=as.Date(MSFT_Opt_date_df$Call_LastTrTime, format="%Y-%m-%d"))
class(Call_LastTrDate_df)
head(Call_LastTrDate_df, 20)
nrow(Call_LastTrDate_df)

Call_LastTrDate_tb <- table(Call_LastTrDate_df)
class(Call_LastTrDate_tb)
show(Call_LastTrDate_tb)

Put_LastTrDate_df <- data.frame(Put_LastTrDate=as.Date(MSFT_Opt_date_df$Put_LastTrTime, format="%Y-%m-%d"))
class(Put_LastTrDate_df)
head(Put_LastTrDate_df, 20)
nrow(Put_LastTrDate_df)

Put_LastTrDate_tb <- table(Put_LastTrDate_df)
class(Put_LastTrDate_tb)
show(Put_LastTrDate_tb)

Call_LastTrDate_TODO_Indx <- MSFT_Opt_date_df$Indx[which(Call_LastTrDate_df$Call_LastTrDate=="2023-04-11")]
show(Call_LastTrDate_TODO_Indx)
length(Call_LastTrDate_TODO_Indx)
Put_LastTrDate_TODO_Indx <- MSFT_Opt_date_df$Indx[which(Put_LastTrDate_df$Put_LastTrDate=="2023-04-11")]
show(Put_LastTrDate_TODO_Indx)
length(Put_LastTrDate_TODO_Indx)
Call_Put_TODO_Indx <- intersect(Call_LastTrDate_TODO_Indx, Put_LastTrDate_TODO_Indx)
show(Call_Put_TODO_Indx)
length(Call_Put_TODO_Indx)

# TODO parity americane
# Put-Call parity
# P_0 = C_0 - S_0 + K/(1+r_f)
# C_0 - P_0 = S_0 - K/(1+r_f)
#

x <- SPX_Opt_2023_04_11_06_16_df$Strike[Call_Put_2023_04_11_Indx]
show(x)
length(x)
y <- SPX_Opt_2023_04_11_06_16_df$Call_LastPr[Call_Put_2023_04_11_Indx]-SPX_Opt_2023_04_11_06_16_df$Put_LastPr[Call_Put_2023_04_11_Indx]
show(y)
length(y)

Data_df <- data.frame(x,y)
nrow(Data_df)
Data_df <- na.omit(Data_df)
nrow(Data_df)
head(Data_df,10)
tail(Data_df,10)
rownames(Data_df) <- 1:nrow(Data_df)
nrow(Data_df)
head(Data_df,10)
tail(Data_df,10)
n <- nrow(Data_df)

title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023", 
                             paste("Scatter Plot of the Call-Put Difference Against the Strike Price")))
subtitle_content <- bquote(paste("Data set size",~~.(n),~~"sample points;    Evaluation Date 2023-04-11;   Maturity Date 2023-06-16"))
caption_content <- "Author: Roberto Monte"
x_breaks_num <- 8
x_breaks_low <- min(Data_df$x)
x_breaks_up <- max(Data_df$x)
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- format(x_breaks, scientific=FALSE)
J <- 0.2
x_lims <- c(x_breaks_low-J*x_binwidth,x_breaks_up+J*x_binwidth)
x_name <- bquote("strike")
y_breaks_num <- 10
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- y_min
y_breaks_up <- y_max
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth)
if((y_breaks_up-max(y_breaks))>y_binwidth/2){y_breaks <- c(y_breaks,y_breaks_up)}
y_labs <- format(y_breaks, scientific=FALSE)
y_name <- bquote("call-put difference")
K <- 0.2
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
col_1 <- bquote("data set sample points")
col_2 <- bquote("regression line")
col_3 <- bquote("LOESS curve")
leg_labs <- c(col_1, col_2, col_3)
leg_cols <- c("col_1"="blue", "col_2"="green", "col_3"="red")
leg_ord <- c("col_1", "col_2", "col_3")
Call_Put_Strike_Pr_2023_04_11_06_16_sp <- ggplot(Data_df, aes(x=x, y=y)) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="dashed", aes(color="col_3"),
              method="loess", formula=y ~ x, se=FALSE, fullrange = FALSE) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="solid", aes(color="col_2"),
              method="lm" , formula=y ~ x, se=FALSE, fullrange=FALSE) +
  geom_point(alpha=1, size=1.0, shape=19, aes(color="col_1")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_ord,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x=element_text(angle=0, vjust=1),
        legend.key.width=unit(1.0,"cm"), legend.position="bottom")
plot(Call_Put_Strike_Pr_2023_04_11_06_16_sp)

################################################################################
# TODO
################################################################################

# Consider the pay off of an American put option we have
AP_PO <- matrix(NA, nrow=N+1, ncol = N+1)
AP_PO[1,1] <- 0
for(n in 1:N){
  for(k in 0:N){AP_PO[n+1,k+1] <- round(max(100-S[n+1,k+1],0),3)}
}
show(AP_PO)

AP_PO_df <- as.data.frame(AP_PO)
AP_PO_tb <- setDT(AP_PO_df)
class(AP_PO_tb)
head(AP_PO_tb)

AP_PO_rsh_df <- melt(AP_PO_tb, na.rm=FALSE)
show(AP_PO_rsh_df[1:20,])
AP_PO_mod_rsh_df <- subset(AP_PO_rsh_df, select = -variable)
AP_PO_mod_rsh_df <- rename(AP_PO_mod_rsh_df, AP_PO_value=value)
head(AP_PO_mod_rsh_df,15)
AP_PO_mod_rsh_df <- add_column(AP_PO_mod_rsh_df, Index=rep(0:(nrow(S_df)-1), times=ncol(AP_PO_df)), .before="AP_PO_value")
head(AP_PO_mod_rsh_df,15)
AP_PO_mod_rsh_df <- add_column(AP_PO_mod_rsh_df, S_value=S_mod_rsh_df$S_value, .before="AP_PO_value")
head(AP_PO_mod_rsh_df,15)