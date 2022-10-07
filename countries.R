## Packages
library(janitor)
library(tidyverse)
library(plyr)
library(fredr)
library(stargazer)
library(estimatr)
library(WDI)


## merge politics data and loan data 
## clean ESG data and merge 

library(ggplot2)
library(lubridate)
library(fedmatch)
## run clean-politics-data.R
## and clean_data_1499.R

loans2 = data.frame(read.csv("C:/Users/vitor/Arquivos/Harvard/ECON 1499/Term Paper/loans2_2000_2020.csv"))
# clean loans data
loans2 <- loans2 %>%
  clean_names() %>% # makes names lowercase 
  filter(!is.na(deal_amount_converted)) %>% # remove NA
  filter(deal_amount_converted > 0) %>% # remove 0 values
  filter(country != "United States") %>%
  filter(lender_parent_operating_country == "United States") %>%
  mutate(year = year(deal_active_date)) %>%
  filter(year > 2005) %>%
  mutate(lender_parent_name = clean_strings(lender_parent_name, 
                                            common_words = fedmatch::corporate_words)) %>%
  mutate(borrower_name = clean_strings(borrower_name, 
                                       common_words = fedmatch::corporate_words)) %>%
  mutate(deal_active_date = as.Date(deal_active_date, "%Y/%m/%d")) %>%
  mutate(completion_date = as.Date(completion_date, "%Y/%m/%d")) %>%
  mutate(closed_date = as.Date(closed_date, "%Y/%m/%d")) %>%
  mutate(mandated_date = as.Date(mandated_date, "%Y/%m/%d")) %>%
  mutate(launch_date = as.Date(launch_date, "%Y/%m/%d")) %>%
  mutate(lender_share = as.numeric(lender_share)) %>%
  mutate(lender_share = if_else(is.na(lender_share), 100/number_of_lenders, lender_share)) %>% # if lender share NA, set equal to fraction of lenders
  mutate(number_sic = as.numeric(substr(sic_code, 1, 4))) %>% # subtr SIC codes 
  mutate(nsic = number_sic)

## load capitalization 
banks = c("TRUIST BK/TRUIST FC" = "truist financial",
          "BANK OF AMER NA/BANK OF AMER CORP" = "bofa securities",
          "BANK OF NY MELLON/BANK OF NY MELLON CORP" = "bank of new york mellon",
          "CITIBANK NA/CITIGROUP" = "citi",
          "CIT BK NA/CIT GROUP" = "cit group incorporated",
          "COMERICA BK/COMERICA" = "comerica incorporated",
          "FIFTH THIRD BK NA/FIFTH THIRD BC" = "fifth third bank",
          "GOLDMAN SACHS BK USA/GOLDMAN SACHS GROUP THE" = "goldman sachs and company",
          "JPMORGAN CHASE BK NA/JPMORGAN CHASE & CO" = "jp morgan",
          "KEYBANK NA/KEYCORP" = "keybank",
          "MORGAN STANLEY BK NA/MORGAN STANLEY" = "morgan stanley",
          "NORTHERN TC/NORTHERN TR CORP" = "northern trust corporation",
          "PNC BK NA/PNC FNCL SVC GROUP" = "pnc bank",
          "REGIONS BK/REGIONS FC" = "regions bank",
          "SILICON VALLEY BK/SVB FNCL GRP" = "svb financial group",
          "SOUTHTRUST BK NA/LIVE OAK BSHRS CORP" = "SOUTHTRUST BANK",
          "STATE STREET B&TC/STATE STREET CORP" = "state street bank",
          "BANCORP BK/BANCORP" = "us bancorp",
          "WELLS FARGO BK NA/WELLS FARGO & CO" = "wells fargo and company",
          "ZIONS BC NA/" = "zions bancorporation",
          "HIBERNIA NB/HIBERNIA CORP" = "HIBERNIA CORP",
          "FIRST BK/FIRST BANKS INC" = "first bank national association")
bank_names = banks %>% names()
bank_assets = read.csv("C:/Users/vitor/Arquivos/Harvard/ECON 1499/Term Paper/bank_assets.csv") %>%
  clean_names() %>%
  mutate(consol_assets = if_else(consol_assets_mil != "",
                                 as.numeric(consol_assets_mil),
                                 as.numeric(consol_assets)),
         domestic_assets = if_else(domestic_assets_mil != "",
                                   as.numeric(domestic_assets_mil),
                                   as.numeric(domestic_assets)),
         date = as.Date(as.character(date), "%Y%m%d")) %>%
  rename(fed_name = bank_name_holding_co_name) %>%
  select(fed_name, date, consol_assets, domestic_assets) %>%
  filter(fed_name %in% bank_names) %>%
  mutate(clean_name = "")
for (name in names(banks)) {
  bank_assets <- bank_assets %>% mutate(clean_name = if_else(grepl(name, fed_name),
                                                             banks[[name]], clean_name))
}

bank_assets = bank_assets %>%
  select(!fed_name) %>%
  mutate(year = year(date)) %>%
  group_by(year, clean_name) %>%
  summarize(consol_assets = mean(consol_assets),
            domestic_assets = mean(domestic_assets))


fredr_set_key("083544dce386b6d96d2f4814d625b545")
## yearly change in rates 
fedfunds <- fredr(
  series_id = "FEDFUNDS",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2020-12-01"),
  frequency = "a", # yearly
  units = "lin" # change over previous value
) %>% mutate(year = year(date)) %>% select(year, value) %>%
  dplyr::rename(fedfunds = value)

# GDP Data (gdp at US$ 2010 constant)
gdp <- data.frame(WDI(indicator ="NY.GDP.MKTP.KD", country = "all", start = 2006, end = 2020))

# Loading Democracy Index from 2006 to 2020
load(file = "C:/Users/vitor/Arquivos/Harvard/ECON 1499/Term Paper/eiu.rda")
democracy_index = eiu

democracy_index$extended_country_name = ifelse(democracy_index$eiu_country == "Germany",
                                               "Germany",
                                              democracy_index$extended_country_name)

democracy_index$extended_country_name = ifelse(democracy_index$eiu_country == "Turkey",
                                               "Turkey",
                                               democracy_index$extended_country_name)

democracy_index$extended_country_name = ifelse(democracy_index$eiu_country == "Korea, Republic of",
                                               "South Korea",
                                               democracy_index$extended_country_name)

# Combining Data
countries_df = loans2 %>%
  mutate(cycle = round_any(year, 2)) %>%
  left_join(fedfunds, by = 'year') %>%
  left_join(bank_assets, by = c('year' = 'year',
                                'lender_parent_name' = 'clean_name'))%>%
  filter(is.na(domestic_assets) == F)%>%
  left_join(democracy_index, by = c('year' = 'year',
                                    'country' = 'extended_country_name')) %>%
  left_join(politics, by = c('cycle' = 'cycle',
                             'lender_parent_name' = 'bank_name'))%>%
  left_join(gdp, by = c('year', 'country')) %>%
## SEVERAL NA'S THAT I CANNOT UNDERSTAND WHY THEY APPEAR
  filter(is.na(eiu) == F)%>%
  filter(is.na(dem_60) == F)
  
  
# Regression Analysis

lm1 = lm(deal_amount ~ eiu + eiu:dem_60 + fedfunds + NY.GDP.MKTP.KD, 
         data = countries_df)%>% summary()

lm2 = lm(deal_amount ~ eiu + eiu:dem_60 + fedfunds + NY.GDP.MKTP.KD + fed_domestic_assets, 
         data = countries_df)%>% summary()

lm3 = lm(deal_amount ~ eiu + eiu:dem_60 + fedfunds + NY.GDP.MKTP.KD + fed_domestic_assets
         + factor(country), 
         data = countries_df)%>% summary()

lm4 = lm(deal_amount ~ eiu + eiu:dem_60 + fedfunds + NY.GDP.MKTP.KD + fed_domestic_assets
         + factor(lender_parent_name), 
         data = countries_df)%>% summary()



