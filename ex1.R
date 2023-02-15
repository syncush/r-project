# Install Pacakages
#install.packages("tidyverse")
#install.packages("ggpubr")
#install.packages('patchwork')
# Import packages
library(tidyverse)
library(patchwork)

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
  stepPlot <- ggplot() + 
    geom_line(data=stepData, aes(x=factor(date, levels = month.name), group=1, y=expenditureMean)) +
    geom_point(data=stepData, aes(date, expenditureMean)) +
    labs(title="Daily Median Expenditures (Over a month)", x="Months", y="Expenditures (Shekels)") 
  
  boxplotData <- tbl %>% 
    filter(date >= minDate & date <= maxDate & debit > 0) %>% 
    group_by(date=months(date)) 
  
  boxplotPlot <- ggplot() +  
    geom_boxplot(data=boxplotData, aes(x=factor(date, levels = month.name), y=debit, group=date)) + 
    labs(title="Monthly Median Expenditures", x="Months", y="Expenditures (Shekels)") 
  stepPlot + boxplotPlot
}

# Nominal Expenditures
patch <- (drawExpenditureOverMonthPlot(combined_data) + drawSumExpenditureHeatmapPerCategory(combined_data)) /
          (drawMeanExpenditureOverMonth(combined_data))
patch + plot_annotation(title = 'Nominal Expenditures')

# Real Expenditures 1
combinedDataWithMonthName <- tibble(combined_data) %>% mutate(month_name=months(date))
realExpendCombinedData <- left_join(combinedDataWithMonthName, cumulativeMontlyInflation(general_month_info), by=c("month_name")) %>% mutate(debit=debit/(1+ccpi)) %>% select(colnames(combined_data))
patch <- (drawExpenditureOverMonthPlot(realExpendCombinedData) + drawSumExpenditureHeatmapPerCategory(realExpendCombinedData)) /
  (drawMeanExpenditureOverMonth(realExpendCombinedData))
patch + plot_annotation(title = 'Real Expenditures 1 (CCPI)')

# Real Expenditure 2
realExpenditureCDCPI <- left_join(tibble(combined_data), 
                                  cumulativeImpliedDailyInflation(general_month_info, month.name[6:10]),
                                  by=c("date")) %>%
                        mutate(debit=debit / (1 + cdcpi_dm)) %>% 
                        select(colnames(combined_data))
patch <- (drawExpenditureOverMonthPlot(realExpenditureCDCPI) + drawSumExpenditureHeatmapPerCategory(realExpenditureCDCPI)) /
  (drawMeanExpenditureOverMonth(realExpenditureCDCPI))
patch + plot_annotation(title = 'Real Expenditures 2 (CDCPI)')


createXMonitorChart <- function(xChartCombinedData, title) {
  ggplot(xChartCombinedData) +
    geom_point(aes(x=date, y=sum)) +
    geom_line(aes(x=date, y=sum)) +
    geom_line(aes(x=date, y=cl), color="red") +
    geom_line(aes(x=date, y=lcl), linetype="dashed", color="red") +
    geom_line(aes(x=date, y=ucl), linetype="dashed", color="red") +
    labs(title=title, x="Time", y="Daily Sum Expenditure")
}
####### Monitoring Charts
xChartCombinedData <- sumExpendituresPerDay(combined_data, min_analysis_date, max_analysis_date)
# not the correct value
firstMonth = "June"
# Dont know if baseCL is what he meant
baseCL <- 
baseSD <- xChartCombinedData %>% filter(months(date) == firstMonth) %>% summarise(stdDev=sd(sum)) %>% pull() %>% nth(1)
ucl <- baseCL + 3 * baseSD
lcl <- max(0, baseCL - 3 * baseSD)
xChartCombinedData <- xChartCombinedData %>% mutate(cl=baseCL, lcl=lcl, ucl=ucl)
createXMonitorChart(xChartCombinedData, "X-Chart Daily Sum Expenditure")
