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
max_analysis_date = as.Date("10/08/2022", credit_card_date_format)

# Analysis
analysis_categories = c("all")
# Options: 
#    ExpenditureOverMonthPlot (Task 1)
#    MeanExpenditurePerDayPlot (Task 2)
#    MeanExpenditureOverMonth (Task 3)
#    SumExpenditureHeatmapPerCategory (Task 4)
analysis_plots = c("ExpenditureOverMonthPlot",
                   "MeanExpenditurePerDayPlot",
                   "MeanExpenditureOverMonth",
                   "SumExpenditureHeatmapPerCategory")


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

prepareCombinedData <- function(chequingDataset, categoryCreditCardChequingFilterName, creditCardDataset, categories) {
  # get time span of checquing account
  checquing_account_min_date <- min(chequingDataset$date)
  checquing_account_max_date <- max(chequingDataset$date)

  # remove credit card transactions
  chequingDataset = subset(chequingDataset, category!=categoryCreditCardChequingFilterName)
  chequingDataset = cbind(chequingDataset, transaction_type=NA)
  
  # prep to merge objects
  colnames(creditCardDataset) <- c("date", "debit", "transaction_type", "description", "category", "account")
  creditCardDataset = cbind(creditCardDataset, credit=0)
  
  combined_data <- rbind(chequingDataset, creditCardDataset)
  
  combined_data <- as_tibble(combined_data)
  
  combined_data %>%
    filter(category %in% categories | match("all", categories)) %>%
    filter(date >= checquing_account_min_date & date <= checquing_account_max_date)
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

combined_data <- prepareCombinedData(checquing_account_csv,
                                     categoryCreditCardChequingFilterName,
                                     credit_card_data,
                                     analysis_categories
                                     )

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

# function that returns the sum of expenditures between two dates
sumExpendituresPerDay = function(df, startDate, endDate) {
  df %>% 
    filter(date >= startDate & date <= endDate) %>%
    group_by(date) %>%
    summarise(sum=sum(debit, na.rm = TRUE)) %>%
    complete(date=seq.Date(min(date), max(date), by="day"), fill = list(sum=0))
}

# function that returns the mean daily expenditure between two dates
meanExpenditurePerDay <- function(df, startDate, endDate) {
  df %>%
    filter(date >= startDate & date <= endDate & debit > 0) %>%
    group_by(date) %>%
    summarise(mean=mean(debit, na.rm = TRUE)) %>%
    complete(date=seq.Date(startDate, endDate, by="day"), fill = list(mean=0))
}

drawExpenditureOverMonthPlot <- function(tbl, minDate=min_analysis_date, maxDate=max_analysis_date) {
  sumEPM <- tbl %>%
    group_by(date=lubridate::floor_date(date, "month")) %>%
    summarize(sumExpenditure = sum(debit, na.rm = TRUE))
  ggplot() +
    geom_line(data=sumExpendituresPerDay(tbl, minDate, maxDate), aes(date, sum,), colour="blue") + 
    geom_step(data=sumEPM, aes(date, sumExpenditure), colour="red", size=1.2) +
    labs(y = "Sum of Expenditures (Shekels)", x = "Time (days)")
}


drawMeanExpenditurePerDayPlot <- function(tbl, minDate=min_analysis_date, maxDate=max_analysis_date) {
  boxplotData <- tbl %>%
                  filter(date >= minDate & date <= maxDate & debit > 0)
  ggplot() +
    geom_line(data=meanExpenditurePerDay(tbl, minDate, maxDate), aes(date, mean,)) +
    geom_boxplot(data=boxplotData, aes(date, debit, group=date)) +
    labs(title = "Expenditures Per Day", x = "Time (days)", y = "Mean Expenditure Per Day (Shekels)")
}

drawSumExpenditureHeatmapPerCategory <- function(dataet, minDate=min_analysis_date, maxDate=max_analysis_date) {
  dataet %>%
    filter(date >= minDate & date <= maxDate) %>%
    group_by(month=months(date), category) %>%
    summarise(sum=sum(debit)) %>%
    ggplot(aes(x=month, y=category, fill=sum, alpha)) +
    geom_tile() +
    scale_fill_gradient(low="green", high="red") +
    labs(title = "Expenditure Heatmap By Month and Category", x = "Months", y = "Category Name")
}

drawMeanExpenditureOverMonth <- function(tbl, minDate=min_analysis_date, maxDate=max_analysis_date) {
  stepData <- tbl %>%
    filter(date >= minDate & date <= maxDate & debit > 0) %>%
    group_by(date=months(date)) %>%
    summarize(expenditureMean=mean(debit, na.rm = TRUE))
  t <- ggplot() +
    geom_step(data=stepData, aes(x=date, y=expenditureMean, color="red"))
  
  boxplotData <- tbl %>%
    filter(date >= minDate & date <= maxDate & debit > 0) %>%
    group_by(date=months(date))
  
  t + geom_boxplot(data=boxplotData, aes(x=date, y=debit, group=date)) +
      labs(title="Monthly Median Expenditures", x="Months", y="Expenditures (Shekels)")
}

# Options: 
#    ExpenditureOverMonthPlot (Task 1)
#    MeanExpenditurePerDayPlot (Task 2)
#    MeanExpenditureOverMonth (Task 3)
#    SumExpenditureHeatmapPerCategory (Task 4)
if (match("ExpenditureOverMonthPlot", analysis_plots)) {
  drawExpenditureOverMonthPlot(combined_data)
}
if (match("MeanExpenditurePerDayPlot", analysis_plots)) {
  drawMeanExpenditurePerDayPlot(combined_data)
}
if (match("MeanExpenditureOverMonth", analysis_plots)) {
  drawMeanExpenditureOverMonth(combined_data)
}
if (match("SumExpenditureHeatmapPerCategory", analysis_plots)) {
  drawSumExpenditureHeatmapPerCategory(combined_data)
}

          