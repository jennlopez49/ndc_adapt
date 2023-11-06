#### dates of submissions 
dates <- read.csv("ndc_content.csv")
dates$country.code <- dates$ISO

## merging 
full_data_dates <- merge(full_data, dates, by = "country.code")
full_data_dates$date <- full_data_dates$Value


# converting date to just year 

full_data_dates <- full_data_dates %>% mutate(
  date = as.Date(date, "%m/%d/%Y"),
  year = year(date)
)


### by year 
table(full_data_dates$year)


### adding in G20, Income-Status, OECD, etc
ndc_exp <- readxl::read_excel("adpatation_ndc_data.xlsx")

ndc_col <- ndc_exp[,c(2,114:119)]


## merging 

cleaned_full <- merge(full_data_dates, ndc_col, by = "country.code")

write.csv(cleaned_full, "cleaned_full.csv")
