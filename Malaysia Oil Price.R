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



# Read the historical brent crude oil price using quandl
# crude_oil <- Quandl("COM/OIL_BRENT", api_key="DM3iRry7DLwc7HogTC-w", start_date="2017-01-01")
crude_oil <- Quandl("CHRIS/ICE_B1", api_key="DM3iRry7DLwc7HogTC-w", start_date="2017-03-01") %>% 
  as_tibble() %>% 
  select(Date, Settle)
crude_oil


# Create a function to detect oil price change
priceChange <- function(x) {
  if_else(x > 0, "Up", if_else(x < 0, "Down", "Maintains"))
}



petrolMY %>% 
  mutate(RON95_Diff = RON95 - lag(RON95), 
         RON97_Diff = RON97 - lag(RON97), 
         DIESEL_Diff = DIESEL - lag(DIESEL)) %>% 
  mutate_at(vars(contains("_Diff")), funs(Change = priceChange(.))) %>% 
  select(DATE, RON95, everything())


combine <- crude_oil %>% right_join(exchange_rate, by = c("Date" = "Date"))
sum(is.na(combine))
combine %>% filter(is.na(Settle) | is.na(USD))
combine









