# Load all the required libraries
library(tidyverse)
library(rvest)
library(Quandl)
library(lubridate)


# Read the historical oil price table from the KPDNKK webpage 
kpdnkk <- read_html("https://www.kpdnkk.gov.my/kpdnkk/oil-price-2018/?lang=en") %>% 
  html_nodes(".col-md-2 , #content .col-md-6:nth-child(1)") %>% 
  html_text()


# The first 4 vector strings are the header
myColName <- kpdnkk[c(1:4)] %>% 
  str_trim() %>% 
  str_extract("[[:upper:]]+[[:digit:]]{2}|DIESEL|DATE") %>% 
  str_replace_na()
myColName


# Turn the rest of the vector into a tibble and convert the columns to its appropiate type
petrolMY <- matrix(kpdnkk[c(5:length(kpdnkk))], ncol = 4, byrow = TRUE) %>%  
  as_tibble() %>% 
  mutate_at(.vars = c(2:4), funs(as.numeric(.))) %>% 
  mutate_at(.vars = 1, funs(dmy(str_replace(., "^[[:digit:]].+[[:punct:]]", "")) - 6))
petrolMY


# Put in the column name and check our tibble
colnames(petrolMY) <- myColName  
petrolMY %>% tail()


# Customize BNM link to match current month and year
currentMonth <- str_c("&EndMth=", today() %>% month())
currentYear  <- str_c("&EndYr=", today() %>%  year())
firstPart    <- "http://www.bnm.gov.my/index.php?ch=statistic&pg=stats_exchangerates&lang=en&StartMth=3&StartYr=2017"
finalPart    <- "&sess_time=1200&pricetype=Mid&unit=rm"
bnm_link     <- str_c(firstPart, currentMonth, currentYear, finalPart) 


# Read the historical exchange rate from BNM website selecting the first two columns only
bnm <- read_html(bnm_link) %>% 
  html_table(fill = TRUE) %>% 
  .[[1]] %>% 
  select(c(1:2))


# Rename our column, turn the table into a tibble and remove the unrelevant exchange rate
colnames(bnm) <- c("Date", "USD")
exchange_rate <- bnm %>% 
  as_tibble() %>% 
  filter(!row_number() >= which(is.na(USD))[1]) %>% 
  mutate(Date = dmy(Date), USD = as.numeric(USD))
exchange_rate %>% tail()

# Read API Key for Quandl from text file. Put your own API key to a text file. 
my_key <- read_table("quandl_api.txt", col_names = FALSE) %>% as.character()


# Read the historical brent crude oil price using quandl
# crude_oil <- Quandl("COM/OIL_BRENT", api_key=my_key, start_date="2017-01-01")
crudeOil_rawdata <- Quandl("CHRIS/ICE_B1", api_key=my_key, start_date="2017-03-01") %>% 
  as_tibble() %>% 
  select(Date, Settle)
crudeOil_rawdata


# Create a function to detect oil price change. 'mutate_at' will pass each selected column
priceChange <- function(x) {
  if_else(x > 0, "Up", if_else(x < 0, "Down", "Maintains"))
}


# Create a function to rename the column name. 'rename_at' will pass the names of each selected column
nameChange <- function(x) {
  str_replace(x, "_Diff", "")
}


nameChange2 <- function(x, name) {
  str_c(name, x)
}


# Create a column to specify price change and the change direction
petrolMY <- petrolMY %>% 
  mutate(RON95_Diff = RON95 - lag(RON95), 
         RON97_Diff = RON97 - lag(RON97), 
         DIESEL_Diff = DIESEL - lag(DIESEL)) %>% 
  mutate_at(vars(contains("_Diff")), funs(Chg = priceChange(.))) %>% 
  rename_at(vars(contains("_Chg")), funs(nameChange(.))) %>% 
  select(DATE, RON95, everything())


# Group USD exchange rate data by custom work-week and tidy-up the data
USD_rawdata <- exchange_rate %>% 
  filter(Date >= "2017-03-30") %>% 
  mutate(Day = wday(Date, label = TRUE, week_start = 4), Week = Date - ((as.integer(Day)) - 1) %% 7) %>% 
  select(-Date) %>% 
  spread(key = Day, value = USD) %>% 
  rename_at(vars(-starts_with("Week")), funs(nameChange2(., "USD_")))


# Check for missing values in USD data
USD_rawdata %>% 
  filter_at(vars(-starts_with("Week")), any_vars(is.na(.)))


# Fill up USD data missing values based on previous days
USD <- USD_rawdata %>% 
  gather(USD_Thu:USD_Wed, key = "Days", value = "USD_Rate") %>% 
  mutate_if(is.character, as_factor) %>% 
  arrange(Week, Days) %>% 
  fill(USD_Rate) %>% 
  spread(key = Days, value = USD_Rate)


# Group crude oil price data by custom work week
crude_oil <- crudeOil_rawdata %>% 
  arrange(Date) %>% 
  filter(Date >= "2017-03-30") %>% 
  mutate(Week = Date - wday(Date, week_start = 4) + 1, 
         Day = wday(Date, label = TRUE) %>% as.character() %>% as_factor()) %>% 
  select(-Date) %>% 
  spread(key = Day, value = Settle) %>% 
  rename_if(is.numeric, funs(nameChange2(., "Brent_")))


# Check for missing values in crude oil data
crude_oil %>% 
  filter_at(vars(-starts_with("Week")), any_vars(is.na(.)))


# Fill-up crude oil missing values



# Random testing
combine <- crude_oil %>% right_join(exchange_rate, by = c("Date" = "Date"))
sum(is.na(combine))
combine %>% filter(is.na(Settle) | is.na(USD))
combine %>% filter(Date >= "2017-03-30") %>% mutate(Day       = wday(Date, label = TRUE),
                                                    rand      = wday(Date),
                                                    Day_Thur  = wday(Date, week_start = 4),
                                                    test_Thur = Date - ((as.integer(Day_Thur) - 1) %% 7),
                                                    Day_Sun   = wday(Date),
                                                    test_Sun  = Date - ((as.integer(Day_Sun) - 5) %% 7),
                                                    test_Tues = Date - ((as.integer(Day) - 3) %% 7))







