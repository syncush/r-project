# Install Pacakages
# install.packages("tidyverse")

# Import packages
library(tidyverse)

# Variables for Pavel

# Credit card data
credit_card_date_format = "%d/%m/%Y"
june_csv_path = "Export_8_06_2022.csv"
july_csv_path = "Export_8_07_2022.csv"
august_csv_path = "Export_8_08_2022.csv"
shouldImportSeptember = TRUE
september_csv_path = "Export_9_2022.csv"

# Chequing account data
checquing_account_date_format = "%d.%m.%Y"
checquing_account_csv_path = "chequing_account.csv"
categoryCreditCardChequingFilterName = "credit card"

#output csv
output_path_normalized_csv = "normalized_data.csv"

# Analysis data range
min_analysis_date = as.Date("01/06/2022", credit_card_date_format)
max_analysis_date = as.Date("31/08/2022", credit_card_date_format)

# Analysis
analysis_categories = c()
analysis_plots = c()


# Import CSVs to R
june_csv = read.csv(june_csv_path)
july_csv = read.csv(july_csv_path)
august_csv = read.csv(august_csv_path)

checquing_account_csv = read.csv(checquing_account_csv_path)

# Change the Hebrew name of the columns to English
english_col_names_credit = c( "date", "transaction_sum", "transaction_type", "business_name", "category")
english_col_names_chequing_account = c("date", "debit", "credit", "description", "category")

# set values of CPI from CBS
cpi_values_diff_between_months = c(0.4 , 1.1 , -0.3)

prepareCreditCardDataset <- function(dataset) {
  # Change the Hebrew name of the columns to English
  colnames(dataset) = english_col_names_credit
  # Add a column named “account” with all values being either “checquing” or “credit card”
  dataset = cbind(dataset, account="credit card")
  # convert date chars to date objects
  dataset$date = as.Date(dataset$date, credit_card_date_format)
  # convert numeric characters to numeric values
  dataset$transaction_sum = as.numeric(dataset$transaction_sum)
  # prep making sure no na values in numeric columns
  dataset["transaction_sum"][is.na(dataset["transaction_sum"])] = 0
  dataset
}

prepareChequingDataset <- function(dataset) {
  
  colnames(dataset) = english_col_names_chequing_account
  dataset = cbind(dataset, account="chequing")
  dataset$date = as.Date(dataset$date, checquing_account_date_format)
  dataset$debit = as.numeric(dataset$debit)
  dataset$credit = as.numeric(dataset$credit)
  # prep making sure no na values in numeric columns
  dataset["debit"][is.na(dataset["debit"])] = 0
  dataset["credit"][is.na(dataset["credit"])] = 0
  dataset
}

june_csv <- prepareCreditCardDataset(june_csv)
july_csv <- prepareCreditCardDataset(july_csv)
august_csv <- prepareCreditCardDataset(august_csv)
checquing_account_csv <- prepareChequingDataset(checquing_account_csv)

if (shouldImportSeptember == TRUE) {
  september_csv = prepareCreditCardDataset(read.csv(september_csv_path))
  cpi_values_diff_between_months = append(cpi_values_diff_between_months, 0.2)
} else {
  september_csv = data.frame(matrix(nrow = 0, ncol = length(colnames(august_csv))))
  colnames(september_csv) = colnames(august_csv)
}

# Combine all credit card csv to one unified dataframe
credit_card_data = rbind(june_csv, july_csv, august_csv, september_csv)

# remove credit card transactions
checquing_account_data = subset(checquing_account_csv, category!=categoryCreditCardChequingFilterName)

# prep to merge objects
colnames(credit_card_data) = c("date", "debit", "transaction_type", "description", "category", "account")
credit_card_data = cbind(credit_card_data, credit=0)

checquing_account_data = cbind(checquing_account_data, transaction_type=NA)

combined_data = rbind(checquing_account_data, credit_card_data)

# get time span of checquing account
checquing_account_min_date = min(checquing_account_data$date)
checquing_account_max_date = max(checquing_account_data$date)

combined_data = subset(combined_data, date>=checquing_account_min_date & date<= checquing_account_max_date)

write.csv(combined_data, file=output_path_normalized_csv, fileEncoding = "UTF-8", row.names = FALSE)

# function that returns the sum of expenditures between two dates
sumExpenditures = function(df, startDate, endDate, categoryFilter=NA) {
  if (is.na(categoryFilter)) {
    expenses = (subset(df, date>= startDate &  date <= endDate))$debit  
  } else {
    expenses = (subset(df, date>= startDate &  date <= endDate & category == categoryFilter))$debit
  }
  
  sum(expenses, na.rm=TRUE)
}

# function that returns the mean daily expenditure between two dates
meanDailyExpenditure <- function(df, startDate, endDate, categoryFilter=NA) {
  daysDiff = as.numeric(difftime(endDate, startDate, units))
  if (is.na(categoryFilter)) {
    transactionBetweenDates <- subset(df, date >= startDate & date <= endDate)
  } else {
    transactionBetweenDates <- subset(df, date >= startDate & date <= endDate & category == categoryFilter)  
  }
  sum(transactionBetweenDates$debit, na.rm = TRUE) / (daysDiff + 1)
}

# function that returns the sum of expenditures between two dates
sumExpendituresPerDay = function(df, startDate, endDate, categoryFilter=NA) {
  if (is.na(categoryFilter)) {
    expenses = (subset(df, date>= startDate &  date <= endDate))  
  } else {
    expenses = (subset(df, date>= startDate &  date <= endDate & category == categoryFilter))
  }
  as.data.frame(expenses %>% group_by(date) %>% summarise(sum=sum(debit, na.rm = TRUE)) %>% complete(date=seq.Date(min(date), max(date), by="day"), fill = list(sum=0)))
}

# function that returns the mean daily expenditure between two dates
meanExpenditurePerDay <- function(df, startDate, endDate, categoryFilter=NA) {
  if (is.na(categoryFilter)) {
    transactionBetweenDates <- subset(df, date >= startDate & date <= endDate)
  } else {
    transactionBetweenDates <- subset(df, date >= startDate & date <= endDate & category == categoryFilter)  
  }
  as.data.frame(transactionBetweenDates %>% group_by(date) %>% summarise(sum=mean(debit, na.rm = TRUE)) %>% complete(date=seq.Date(min(date), max(date), by="day"), fill = list(sum=0)))
}
