library(tools)
library(tidyverse)
library(patchwork)

#DATA IMPORT SECTION
#formats and paths section
credit_card_date_format = "%d/%m/%Y"
path_months <- file.path(getwd(), "months")
#the user needs to open a new folder with the name 'months' in the same directory with the code 
#and copy all the csv containing the credit card data for each month as a different csv, first letter of a file needs to be capitalized
path_misc <- file.path(getwd(), "misc")
#the user needs to open a new folder with the name 'misc' in the same directory with the code 
#and copy the chequing and cpi data as csv
#END formats and paths section

checquing_account_date_format = "%d/%m/%Y"
categoryCreditCardChequingFilterName = "credit card"

# Analysis data range
min_analysis_date = as.Date("01/06/2022", credit_card_date_format)
max_analysis_date = as.Date("15/06/2022", credit_card_date_format)

# Analysis
analysis_categories = c("all")

#output csv
output_path_normalized_csv = "normalized_data.csv"

#preprocess months folder
cat("starting to work on months folder.\n")
isValidMonth <- function(months) {
  valid_months <- month.name
  return(months %in% valid_months)
}

readMonthData <- function(paths, months) {
  data_list <- list()
  for (i in seq_along(paths)) {
    data <- read.csv(paths[i])
    if(length(colnames(data)) > 5) {
      stop("you have too many columns in file ", paths[i],"\n",
           "plz check guide to see what columns you are supose to have in your credit card data csv, and start over", sep="")
    } else if (length(colnames(data)) < 5){
      stop("you have not enough columns in file ", paths[i],"\n",
           "plz check guide to see what columns you are supose to have in your credit card data csv, and start over", sep="")
    } else {
      data_list[[months[i]]] <- data 
    }
  }
  return(data_list)
}

prepareCreditCardDataset <- function(dataset) {
  english_col_names_credit = c("date", "transaction_sum", "transaction_type", "business_name", "category")
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

sortMonthData <- function(months_data) {
  months <- names(months_data)
  sorted_month_names <- month.name[sort(match(names(month_data_list_prepered), month.name))]
  months_data <- months_data[order(sorted_month_names)]
  return(months_data)
}

#preprocess starts here
if (!dir.exists(path_months)) {
  stop("The path", path_months, "does not exist.\n",
       "Please make sure the 'months' folder is located in the current working directory, and run the section again.", sep = "")
} else {
  months_files_name_list <- list.files(path_months, pattern="*.csv", full.names=TRUE)
  if (length(months_files_name_list) == 0) {
    stop("There are no CSV files in the ", path_months, "folder.\n",
         "Please add your credit card files and run the section again.\n",
         "Remember! the file name should be the name of the month with the first letter capitalized.", sep="")
  } else {
    month_files_name_no_extention <- toTitleCase(file_path_sans_ext(basename(months_files_name_list)))
    month_validation <- isValidMonth(month_files_name_no_extention)
    for (i in seq_along(month_files_name_no_extention)) {
      if (!month_validation[i]) {
        stop("The name of the file ", months_files_name_list[i], " is not a month.\n", 
             "Please write the month corresponding to the dates that are in your CSV.\n", 
             "REMEMBER! The first letter needs to be capitalized.", sep = "")
      }
    }
    months_list <- month_files_name_no_extention
    validated_paths_months <- months_files_name_list
    cat("finished the validation of csv's in months dir.\n")
    month_data_list <- readMonthData(validated_paths_months, months_list)
    month_data_list_prepered <- lapply(month_data_list,prepareCreditCardDataset)
    month_data_list_prepered_sorted <- sortMonthData(month_data_list_prepered)
    credit_card_data <- arrange(bind_rows(month_data_list_prepered_sorted), date)
    cat("finished working on months data, next stop, misc\n")
  }
}
#END preprocess months folder

#starting preprocess misc folder
english_col_names_chequing_account = c("date", "debit", "credit", "description", "category")

readChequingData <- function(misc_path, misc_files_name) {
  chequing_index <- match("Chequing", misc_files_name)
  chequing_data <- read.csv(misc_path[chequing_index])
  if(length(colnames(chequing_data)) > 5) {
    stop("you have too many columns in file ", misc_path[chequing_index],"\n",
         "plz check guide to see what columns you are supose to have in your Chequing data csv, and start over", sep="")
  } else if (length(colnames(chequing_data)) < 5){
    stop("you have not enough columns in file ", misc_path[chequing_indexi],"\n",
         "plz check guide to see what columns you are supose to have in your Chequing data csv, and start over", sep="")
  }
  chequing_data
}

readCpiData <-  function(misc_path, misc_files_name) {
  cpi_index <- match("Cpi", misc_files_name)
  cpi_data <- read.csv(misc_path[cpi_index])
  cpi_data
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
  dataset$debit <- abs(dataset$debit)
  dataset
}

cat("starting to work on misc dir\n", sep = "")

#reading Chequing and Cpi files
if (!dir.exists(path_misc)) {
  stop("The path", path_months, "does not exist.\n",
       "Please make sure the 'misc' folder is located in the current working directory, and run the section again.", sep = "")
} else {
  misc_files_name_list <- list.files(path_misc, pattern="*.csv", full.names=TRUE)
  if (length(misc_files_name_list) == 0) {
    stop("There are no CSV files in the ", path_misc, "folder.\n",
         "Please add your chequing file and run the section again.\n",
         "Remember! the file name should be the name of the month with the first letter capitalized.", sep="")
  } else {
    misc_files_name_no_extention <- toTitleCase(file_path_sans_ext(basename(misc_files_name_list)))
    if(!"Chequing" %in% misc_files_name_no_extention) {
      stop("misc folder doesnt contain a Chequing file\n, 
            plz copy your file to the dir and run the section again")
    } else {
      chequing_data <- readChequingData(misc_files_name_list, misc_files_name_no_extention)
    }
    if(!"Cpi" %in% misc_files_name_no_extention) {
      stop("misc folder doesnt contain a Cpi file\n, 
            plz copy your file to the dir and run the section again")
    } else {
      cpi_data <- readCpiData(misc_files_name_list, misc_files_name_no_extention)
    }
    chequing_data_prepered <- prepareChequingDataset(chequing_data)
    cat("finished working on misc data, last stop for data prep, combining months and chequing\n")
  }
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
    filter(date >= checquing_account_min_date & date <= checquing_account_max_date) %>%
    filter(date >= min_analysis_date & date <= max_analysis_date)
}

combined_data <- prepareCombinedData(chequing_data_prepered,
                                     categoryCreditCardChequingFilterName,
                                     credit_card_data,
                                     analysis_categories)
combined_data_months <- seq(from=min(combined_data$date), to=max(combined_data$date), by="months") %>% months()

write.csv(combined_data, file=output_path_normalized_csv, fileEncoding = "UTF-8", row.names = FALSE)

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
cpi_values_diff_between_months = as_tibble(cpi_data) %>%
  complete(month_name=month.name, fill=list(cpi=0))

general_month_info <- inner_join(cpi_values_diff_between_months %>% complete(month_name=month.name, fill=list(cpi=0)),
                                 create_month_tibble(),
                                 by=c("month_name"))

cumulativeMontlyInflation <- function(monthCPI) {
  ccpiVec <- c(general_month_info %>% filter(month_name == combined_data_months[1]) %>% select(cpi) %>% pull() %>% nth(1))
  if (length(combined_data_months) > 1) {
    loopIndex = 2
    prevMonth = combined_data_months[1]
    for(monthName in combined_data_months[2:length(combined_data_months)]) {
      cpi_i <- monthCPI %>% filter(month_name == monthName) %>% select(cpi) %>% pull() %>% nth(1)
      prevMonthCPI <- monthCPI %>% filter(month_name == prevMonth) %>% select(cpi) %>% pull() %>% nth(1)
      ccpiVec <- append(ccpiVec, (((1 + prevMonthCPI)*(1 +  cpi_i))-1))
      loopIndex <- loopIndex + 1
      prevMonth <- monthName
    }
  }
  tibble(month_name=combined_data_months, ccpi=ccpiVec)
}

impliedDailyInflation <- function(monthCPI) {
  dcpiVec = c()
  index <- 6
  for(monthName in combined_data_months) {
    dm_i = monthCPI %>% filter(month_name == monthName) %>% select(days_in_month) %>% pull() %>% nth(1)
    month_cpi_val <- monthCPI %>% filter(month_name == monthName) %>% select(cpi) %>% pull() %>% nth(1)
    dcpiVec <- append(dcpiVec, (((1 + month_cpi_val)^(1 / dm_i)) - 1))
    index <- index + 1
  }
  tibble(month_name=combined_data_months, dcpi=dcpiVec)
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

cumulativeImpliedDailyInflationTibble <- cumulativeImpliedDailyInflation(general_month_info, combined_data_months)

# function that returns the sum of expenditures between two dates 
sumExpendituresPerDay = function(df, startDate, endDate) { 
  df %>%
    filter(debit > 0) %>%
    filter(date >= startDate & date <= endDate) %>% 
    group_by(date) %>% 
    summarise(sum=sum(debit, na.rm = TRUE)) %>%
    complete(date=seq.Date(startDate, endDate, by="day"), fill = list(sum=0)) 
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
    ggplot(aes(x=factor(month, levels = month.name), y=category, fill=sum, alpha)) + 
    geom_tile() +
    scale_fill_gradient(low="green", high="red") + 
    labs(title = "Expenditure Heatmap", x = "Months", y = "Category") 
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
combinedDataWithMonthName <- tibble(combined_data) %>% 
  mutate(month_name=months(date))
realExpendCombinedData <- left_join(combinedDataWithMonthName, cumulativeMontlyInflation(general_month_info), by=c("month_name")) %>%
  mutate(debit=debit/(1+ccpi)) %>%
  select(colnames(combined_data))
patch <- (drawExpenditureOverMonthPlot(realExpendCombinedData) + drawSumExpenditureHeatmapPerCategory(realExpendCombinedData)) /
  (drawMeanExpenditureOverMonth(realExpendCombinedData))
patch + plot_annotation(title = 'Real Expenditures 1 (CCPI)')

# Real Expenditure 2
realExpenditureCDCPI <- left_join(tibble(combined_data), 
                                  cumulativeImpliedDailyInflationTibble,
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
    labs(title=title, x="Time", y="Daily Sum Expenditure (Shekels)")
}

getBaseCL <- function(sumExpendituresPerDayTibble, monthName) {
  sumExpendituresPerDayTibble %>%
    filter(months(date) == firstMonth) %>%
    summarise(mean=mean(sum)) %>%
    pull() %>%
    nth(1)
}

getBaseSD <- function(sumExpendituresPerDayTibble, monthName) {
  sumExpendituresPerDayTibble %>%
    filter(months(date) == firstMonth) %>%
    summarise(stdDev=sd(sum)) %>%
    pull() %>%
    nth(1)
}

####### Monitoring Charts
##### XBAR
xChartCombinedData <- sumExpendituresPerDay(combined_data, min_analysis_date, max_analysis_date)
# not the correct value
firstMonth = combined_data_months[1]
baseCL <- getBaseCL(xChartCombinedData, firstMonth)
baseSD <- getBaseSD(xChartCombinedData, firstMonth)
ucl <- baseCL + 3 * baseSD
lcl <- max(0, baseCL - 3 * baseSD)
xChartCombinedData <- xChartCombinedData %>% mutate(cl=baseCL, lcl=lcl, ucl=ucl)
xBarNominal <- createXMonitorChart(xChartCombinedData, "Nominal")

ccpiTibble <- cumulativeMontlyInflation(general_month_info)
updatedCCPIControlxChartCombinedData <- left_join(tibble(xChartCombinedData) %>% mutate(month_name=months(date)), ccpiTibble, by=c("month_name")) %>%
  mutate(cl=ccpi * cl, lcl=ccpi * lcl, ucl=ucl * ccpi)
xBarCCPI <- createXMonitorChart(updatedCCPIControlxChartCombinedData, "CCPI")

updatedCDCCPIControlxChartCombinedData <- left_join(tibble(xChartCombinedData), cumulativeImpliedDailyInflationTibble, by=c("date")) %>%
  mutate(cl=cdcpi_dm * cl, lcl=cdcpi_dm * lcl, ucl=ucl * cdcpi_dm)
xBarCDCPI <- createXMonitorChart(updatedCDCCPIControlxChartCombinedData, "CDCPI")

xBarNominal + xBarCCPI + xBarCDCPI + plot_annotation(title = 'XBar Daily Sum Expenditure Monitor')

# CuSum Charts
generateCucusmTibble <- function(dataTibble, inflationTibble, mode="nominal") {
  delta <- 1
  # not the correct value
  firstMonth = combined_data_months[1]
  sumExpendituresPerDayTibble <- sumExpendituresPerDay(dataTibble, min_analysis_date, max_analysis_date)
  sqrt_vx <- getBaseSD(sumExpendituresPerDayTibble, combined_data_months[1])
  k <- sqrt_vx / 2
  h <- 11.61828 * k
  cusumTibble <- sumExpendituresPerDayTibble %>%
    mutate(c_plus_zero=NA, c_minus_zero=NA)
  currentDate <- min(cusumTibble$date)
  maxDate <- max(cusumTibble$date)
  
  index <- 0
  while(currentDate <= maxDate) {
    if (index == 0) {
      c_plus_zero_i = 0
      c_minus_zero_i = 0
      index <- index + 1
      cusumTibble <- cusumTibble %>% mutate(c_plus_zero=replace(c_plus_zero, date == currentDate, c_plus_zero_i),
                                            c_minus_zero=replace(c_minus_zero, date == currentDate, c_minus_zero_i))
    } else {
      mu_0 <- inflationTibble %>%
        filter(date == currentDate) %>%
        select(c(mode)) %>%
        pull() %>%
        nth(1)
      prevDay <- currentDate - lubridate::days(1)
      prev_c_plus_zero_i = cusumTibble %>% filter(date == prevDay) %>% select(c_plus_zero) %>% nth(1)
      prev_c_minus_zero_i = cusumTibble %>% filter(date == prevDay) %>% select(c_minus_zero) %>% nth(1)
      
      xi <- cusumTibble %>% filter(date == currentDate) %>% select(sum) %>% pull() %>% nth(1)
      
      c_plus_zero_i = max(0, prev_c_plus_zero_i + xi - mu_0 - k)
      c_minus_zero_i = max(0, prev_c_minus_zero_i + mu_0 - k - xi)
      cusumTibble <- cusumTibble %>% mutate(c_plus_zero=replace(c_plus_zero, date == currentDate, c_plus_zero_i),
                                            c_minus_zero=replace(c_minus_zero, date == currentDate, c_minus_zero_i))
      index <- index + 1
      currentDate <- currentDate + lubridate::days(1)
    }
  }
  
  cusumCharHelperTibble <- cusumTibble %>% 
    mutate(k=k, minus_k=-k, h=h, minus_h=-h)
  cusumCharHelperTibble
}

drawCusum <- function(cusumTibble, title) {
  ggplot(cusumTibble) +
    geom_line(aes(x=date, y=c_plus_zero), color="black") +
    geom_point(aes(x=date, y=c_plus_zero), color="black") +
    geom_line(aes(x=date, y=c_minus_zero), color="green") +
    geom_point(aes(x=date, y=c_minus_zero), color="green") +
    geom_line(aes(x=date, y=k), color="red") +
    geom_line(aes(x=date, y=minus_k), color="red") +
    geom_line(aes(x=date, y=h), color="red",  linetype="dashed") +
    geom_line(aes(x=date, y=minus_h), color="red", linetype="dashed") +
    labs(title=title, x="Time", y="Shekels")
}

cdcpiTibble <- cumulativeImpliedDailyInflationTibble
inflationTibble <- tibble(date=seq(from=min(combined_data$date),
                                   to=max(combined_data$date),
                                   by="days"),
                          month_name=months(date),
                          ccpi=NA,
                          cdcpi=NA,
                          nominal=getBaseCL(sumExpendituresPerDay(combined_data, min_analysis_date, max_analysis_date), combined_data_months[1]))
inflationTibble <- left_join(inflationTibble, general_month_info, by=c("month_name")) %>% mutate(ccpi=cpi) %>% select(colnames(inflationTibble))
inflationTibble <- left_join(inflationTibble, cdcpiTibble, by=c("date")) %>% mutate(cdcpi=cdcpi_dm) %>% select(colnames(inflationTibble))

pCuSumNominal <- drawCusum(generateCucusmTibble(combined_data, inflationTibble, "nominal"), "Nominal")
pCuSumCCPI <- drawCusum(generateCucusmTibble(combined_data, inflationTibble, "ccpi"), 'CCPI')
pCuSumCDCPI <- drawCusum(generateCucusmTibble(combined_data, inflationTibble, "cdcpi"), 'CDCPI')
pCuSumNominal + pCuSumCCPI + pCuSumCDCPI + plot_annotation(title = 'CuSum')
