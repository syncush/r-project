# Install Pacakages
#install.packages("tidyverse")
#install.packages("ggpubr")

# Import packages
library(tidyverse)
library(ggpubr)

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


# Import CSVs to R
june_csv = read.csv(june_csv_path)
july_csv = read.csv(july_csv_path)
august_csv = read.csv(august_csv_path)

checquing_account_csv = read.csv(checquing_account_csv_path)

# Change the Hebrew name of the columns to English
english_col_names_credit = c( "date", "transaction_sum", "transaction_type", "business_name", "category")
english_col_names_chequing_account = c("date", "debit", "credit", "description", "category")

# set values of CPI from CBS
create_month_tibble <- function() {
  month_start <- seq(
    from = as.Date("2022-01-01", format="%Y-%m-%d"),
    to = as.Date("2022-12-01", format="%Y-%m-%d"),
    by = "months"
  )
  month_end <- month_start %>% lubridate::ceiling_date(unit="month") - 1
  tibble(month_name = month.name,
         start = month_start,
         end = month_end,
         days_in_month=month_start %>% lubridate::days_in_month())
}
cpi_values_diff_between_months = as_tibble(read.csv("cpi_csv.csv")) %>%
                                  complete(month_name=month.name, fill=list(cpi=0))

general_month_info <- inner_join(cpi_values_diff_between_months %>% complete(month_name=month.name, fill=list(cpi=0)),
                                 create_month_tibble(),
                                 by=c("month_name"))

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

##
## No handling of date range
##
stdDevPerMonthForDailySumOfExpend <- function(tbl, min_date_range, max_date_range) {
  sumPerDay <- tbl %>%
                group_by(date) %>%
                summarise(sum=sum(debit, na.rm = TRUE)) %>% 
                complete(date=seq.Date(min(date), max(date), by="day"), fill = list(sum=0))
  stdDevPerMonth <- sumPerDay %>%
                      group_by(month_name=months(date)) %>%
                      summarise_at(vars(sum), list(expense_std_dev=sd))
  stdDevPerMonth
}

cumulativeMontlyInflation <- function(monthCPI) {
  ccpiVec <- c(general_month_info %>% filter(month_name == "June") %>% select(cpi) %>% pull() %>% nth(1))
  loopIndex = 2
  prevMonth = "June"
  for(monthName in c("July", "August", "September", "October")) {
    cpi_i <- monthCPI %>% filter(month_name == monthName) %>% select(cpi) %>% pull() %>% nth(1)
    prevMonthCPI <- monthCPI %>% filter(month_name == prevMonth) %>% select(cpi) %>% pull() %>% nth(1)
    ccpiVec <- append(ccpiVec, (((1 + prevMonthCPI)*(1 +  cpi_i))-1))
    loopIndex <- loopIndex + 1
    prevMonth <- monthName
  }
  tibble(month_name=c("June","July", "August", "September", "October"), ccpi=ccpiVec)
}

impliedDailyInflation <- function(monthCPI) {
  dcpiVec = c()
  index <- 6
  for(monthName in month.name[6:10]) {
    dm_i = monthCPI %>% filter(month_name == monthName) %>% select(days_in_month) %>% pull() %>% nth(1)
    month_cpi_val <- monthCPI %>% filter(month_name == monthName) %>% select(cpi) %>% pull() %>% nth(1)
    dcpiVec <- append(dcpiVec, (((1 + month_cpi_val)^(1 / dm_i)) - 1))
    index <- index + 1
  }
  tibble(month_name=month.name[6:10], dcpi=dcpiVec)
}

cumulativeImpliedDailyInflation <- function(monthCPI, monthsNameVec) {
  first_day_of_seq <- monthCPI %>%
                        filter(month_name == monthsNameVec[1]) %>%
                        select(start) %>%
                        pull() %>%
                        nth(1)
  last_day_of_seq <- monthCPI %>%
                      filter(month_name == last(monthsNameVec)) %>%
                      select(end) %>%
                      pull() %>%
                      nth(1)
  daysVec <- seq(
    from = first_day_of_seq,
    to = last_day_of_seq,
    by = "days"
  )
  dcpi_month_tbl <- impliedDailyInflation(monthCPI)
  cdcpi_dm <- c()
  cdcpi_ldom <- 0
  dayIndex <- 1
  prevMonthName <- monthsNameVec[1]
  for(index in 1:length(daysVec)) {
    currentMonth <- months(daysVec[index])
    if (currentMonth != prevMonthName) {
      dayIndex <- 1
      cdcpi_ldom <- last(cdcpi_dm)
    }
    
    currMonthDCPI <- dcpi_month_tbl %>%
                      filter(month_name == currentMonth) %>%
                      select(dcpi) %>%
                      pull() %>%
                      nth(1)
    
    cdcpi_dm <- append(cdcpi_dm, ((1 + cdcpi_ldom)*((1 + currMonthDCPI)^dayIndex)-1))
    
    prevMonthName <- currentMonth
    dayIndex <- dayIndex + 1
  }
  tibble(date=daysVec, cdcpi_dm=cdcpi_dm)
}