# Variables for Pavel
checquing_account_date_format = "%d.%m.%Y"
credit_card_date_format = "%d/%m/%Y"

june_csv_path = "Export_8_06_2022.csv"
july_csv_path = "Export_8_07_2022.csv"
august_csv_path = "Export_8_08_2022.csv"

checquing_account_csv_path = "chequing_account.csv"

# Read the csvs
june_csv = read.csv(june_csv_path)
july_csv = read.csv(july_csv_path)
august_csv = read.csv(august_csv_path)

checquing_account_csv = read.csv(checquing_account_csv_path)

# Change the Hebrew name of the columns into English
english_col_names_credit = c( "date", "transaction_sum", "transaction_type", "business_name", "category")
english_col_names_chequing_account = c("date", "debit", "credit", "description", "category")

colnames(june_csv) = english_col_names_credit
colnames(july_csv) = english_col_names_credit
colnames(august_csv) = english_col_names_credit

colnames(checquing_account_csv) = english_col_names_chequing_account

# Add a column named “account” with all values being either “checquing” or “credit card”
june_csv = cbind(june_csv, account="credit card")
july_csv = cbind(july_csv, account="credit card")
august_csv = cbind(august_csv, account="credit card")

checquing_account_csv = cbind(checquing_account_csv, account="chequing")

# prep dates in each sheet
june_csv$date = as.Date(june_csv$date, credit_card_date_format)
july_csv$date = as.Date(july_csv$date, credit_card_date_format)
august_csv$date = as.Date(august_csv$date, credit_card_date_format)

checquing_account_csv$date = as.Date(checquing_account_csv$date, checquing_account_date_format)


cpi_values = c(1 , 2 , 3)
credit_card_data = rbind(june_csv, july_csv, august_csv)

# remove credit card transactions
checquing_account_data = subset(checquing_account_csv, category!="credit card" | description != "ישראכרט")

# prep to merge objects
colnames(credit_card_data) = c("date", "debit", "transaction_type", "description", "category", "account")
credit_card_data = cbind(credit_card_data, credit=0)

checquing_account_data = cbind(checquing_account_data, transaction_type=NA)

merged_object = rbind(checquing_account_data, credit_card_data)
