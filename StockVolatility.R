#Install Packages first time
#install.packages("writexl")

# Clear workspace. 
rm(list=ls())

#Turn Off Scientific Notation
options(scipen=999)

#Load Packages
library(quantmod)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(writexl)

#Get a base stock ticket, for this example, using United Airlines (ticker - UAL)
UAL_data <- getSymbols("UAL", auto.assign = FALSE)
combined_all_data <- UAL_data[, "UAL.Adjusted"]

symbols <- c("AAPL","ADP","CWH","COST","DIS","HON","IBM","SONO","TWTR")
for (symbol in symbols) {
  symbol_data <- getSymbols(symbol, auto.assign = FALSE)
  close_column <- paste(symbol, ".Adjusted", sep = "")
  symbol_all_data <- symbol_data[, close_column]
  combined_all_data <- merge(combined_all_data, symbol_all_data, join = "left")
}

combined_all_df <- data.frame(combined_all_data)
combined_all_df <- tibble::rownames_to_column(combined_all_df, "Date")
combined_all_df$Date <- as.Date(combined_all_df$Date, format = "%Y-%m-%d")
combined_all_df<-combined_all_df[combined_all_df$Date >= "2018-01-01" & combined_all_df$Date <= "2020-12-31", ]

#Populate daily to daily percentage change
for (col in names(combined_all_df)[2:11]) {
  symbol <- str_sub(col, 1, -6)
  new_col_name <- paste(symbol, "% Change")
  col_values <- combined_all_df[[col]]
  combined_all_df[[new_col_name]] <- log(col_values/lag(col_values))
}
#sort from most recent to oldest
combined_all_df <- combined_all_df[rev(order(combined_all_df$Date)),]

#calculate Average Daily Volatility
for (col in names(combined_all_df)[12:21]) {
  symbol <- str_sub(col, 1, -14)
  new_col_name <- paste(symbol, "Avg Daily Volatility")
  col_values <- combined_all_df[[col]]
  combined_all_df[[new_col_name]] <- sd(col_values, na.rm=TRUE)
}

#calculate Annualized Volatility
for (col in names(combined_all_df)[22:31]) {
  symbol <- str_sub(col, 1, -22)
  new_col_name <- paste(symbol, "Annualized Volatility")
  col_values <- combined_all_df[[col]]
  combined_all_df[[new_col_name]] <- col_values*sqrt(252)
}

#make into dataframe
Output <- as.data.frame(t(combined_all_df))

#Reduce to single column
Output<- Output[-c(2:ncol(Output))]

#Remove irrevant to summary data
Output$HeaderName <- row.names(Output)
rownames(Output) <- 1:nrow(Output)
Output<-Output[-1,]
Output<-Output[,c(2,1)]
colnames(Output)[2] <- "Value"
Output<-Output[!grepl("Adjusted",Output$HeaderName),]
Output<-Output[!grepl("Adj %",Output$HeaderName),]
Output<-Output[order(Output$HeaderName),]

#write to excel
write_xlsx(Output, "VolatilityOutput.xlsx")
