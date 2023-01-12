# Variables for Pavel
# Input
credit_card_date_format = "%d/%m/%Y"
june_csv_path = "Export_8_06_2022.csv"
july_csv_path = "Export_8_07_2022.csv"
august_csv_path = "Export_8_08_2022.csv"

checquing_account_date_format = "%d.%m.%Y"
checquing_account_csv_path = "chequing_account.csv"

output_path_normalized_csv = "normalized_data.csv"

categoryCreditCardChequingFilterName = "credit card"

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

# Combine all credit card csv to one unified dataframe
credit_card_data = rbind(june_csv, july_csv, august_csv)

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

# function to compare the change in sum and mean expenditures between the 3 months
compareExpenditureToCPI <- function(df, category=NA) {
  summarized_data = data.frame(month=c("June", "July", "August"),
                               sumExpendituresPerMonth=c(
                                 sumExpenditures(df, as.Date("2022-06-01"), as.Date("2022-06-30"), category),
                                 sumExpenditures(df, as.Date("2022-07-01"), as.Date("2022-07-31"), category),
                                 sumExpenditures(df, as.Date("2022-08-01"), as.Date("2022-08-31"), category)
                               ),
                               meanDailyExpenditurePerMonth=c(
                                 meanDailyExpenditure(df, as.Date("2022-06-01"), as.Date("2022-06-30"), category),
                                 meanDailyExpenditure(df, as.Date("2022-07-01"), as.Date("2022-07-31"), category),
                                 meanDailyExpenditure(df, as.Date("2022-08-01"), as.Date("2022-08-31"), category)
                               ),
                               stringsAsFactors = FALSE
  )
  
  comparison_table <- ((summarized_data[c("sumExpendituresPerMonth", "meanDailyExpenditurePerMonth")][2:3,] / 
                          summarized_data[c("sumExpendituresPerMonth", "meanDailyExpenditurePerMonth")][1:2,]) - 1) * 100
  
  data.frame(Months=c("July-June", "August-July"), CPI_INDEX=cpi_values_diff_between_months[2:3], sum_expenditure_index=comparison_table[,1], mean_daily_expenditure_index=comparison_table[,2])
}

compareExpenditureByCategory <- function(category) {
  general_comparison_table <- compareExpenditureToCPI(combined_data)
  category_comparison_table <- compareExpenditureToCPI(combined_data, category)
  colnames(category_comparison_table) <- c("Months", "CPI_INDEX", paste(category, colnames(category_comparison_table)[3], sep="_"), paste(category, colnames(category_comparison_table)[4], sep="_"))
  cbind(general_comparison_table, category_comparison_table[,3:4])
}

cat("************** Results for Task 2 **************")
compareExpenditureToCPI(combined_data)

cat("************** Results for Task 3 **************")
# Input for task 3
category_for_task3 = "food"
compareExpenditureByCategory(category_for_task3)


