## Packages
library(janitor)
library(tidyverse)

## The loans are from 2000-01-01 to 2020-12-31
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## merge politics data and loan data 
## clean ESG data and merge 

library(ggplot2)
library(lubridate)
library(fedmatch)
## run clean-politics-data.R
## and clean_data_1499.R
politics = readRDS('bank_data.RDS')
loans = data.frame(read.csv("C:/Users/vitor/Arquivos/Harvard/ECON 1499/Term Paper/loans2_2000_2020.csv"))


# tidyverse version 
loans <- loans %>%
  clean_names() %>% # makes names lowercase 
  filter(!is.na(deal_amount_converted)) %>% # remove NA
  filter(deal_amount_converted > 0) %>% # remove 0 values
  mutate(deal_active_date = as.Date(deal_active_date, "%Y/%m/%d")) %>%
  mutate(completion_date = as.Date(completion_date, "%Y/%m/%d")) %>%
  mutate(closed_date = as.Date(closed_date, "%Y/%m/%d")) %>%
  mutate(mandated_date = as.Date(mandated_date, "%Y/%m/%d")) %>%
  mutate(launch_date = as.Date(launch_date, "%Y/%m/%d")) %>%
  mutate(lender_share = as.numeric(lender_share)) %>%
  mutate(lender_share = if_else(is.na(lender_share), 100/number_of_lenders, lender_share)) %>% # if lender share NA, set equal to fraction of lenders
  mutate(number_sic = as.numeric(substr(sic_code, 1, 4))) %>% # subtr SIC codes 
  mutate(nsic = number_sic) 

#hist(loans$deal_amount_converted, breaks = 1000, xlim = c(0, 5000))

## clean esg data
esg = read.csv('../data/esg_ratings_factors.csv') %>% clean_names()
esg = esg %>%
  select(issuer_name, iva_industry, weighted_average_score, industry_adjusted_score,
         issuer_ticker,
         carbon_emissions_score, corruption_inst_score) %>%
  mutate(issuer_name = clean_strings(issuer_name, 
                                     common_words = fedmatch::corporate_words)) 
# risk
risk = read.csv("../data/Rating data.csv") %>% clean_names() %>%
  select(tic, datadate, splticrm, spcsrc) %>%
  mutate(date = as.Date(datadate, format = "%m/%d/%y")) %>% select(!datadate) %>%
  mutate(year = year(date)) %>%
  select(!date) %>%
  group_by(year, tic) %>%
  slice(n()) %>%
  ungroup()
ig_list = c(
  "AAA", "AA+", "AA", "AA-", "A+", "A", "A-", "BBB+", "BBB", "BBB-"
)
risk = risk %>%
  mutate(invest_grade = if_else(splticrm %in% ig_list, T, F))
# restrict loans 
loans = loans %>% 
  filter(country == "United States") %>% 
  mutate(year = year(deal_active_date)) %>%
  mutate(lender_parent_name = clean_strings(lender_parent_name, 
                                            common_words = fedmatch::corporate_words)) %>%
  mutate(borrower_name = clean_strings(borrower_name, 
                                       common_words = fedmatch::corporate_words)) 

## get average loan size per major industry group 
avg_deal = loans %>%
  mutate(cycle = plyr::round_any(year, 2)) %>%
  group_by(major_industry_group, lender_parent_name, cycle) %>%
  summarize(avg_deal_amount = mean(deal_amount, na.rm = T))
avg_spread = loans %>%
  mutate(cycle = plyr::round_any(year, 2)) %>%
  group_by(major_industry_group, lender_parent_name, cycle) %>%
  summarize(avg_spread = mean(margin_bps, na.rm = T))

## think about IV - measurement error (two scores)
## change in IR as a way to adjust for lending conditions
## eg became more dem as lending conditions got better ... 
# fred key 083544dce386b6d96d2f4814d625b545
library(fredr)
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
realrate <- fredr(
  series_id = "REAINTRATREARAT10Y",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2020-12-01"),
  frequency = "a", # yearly
  units = "lin" # change over previous value
) %>% mutate(year = year(date)) %>% select(year, value) %>%
  dplyr::rename(realrate = value)

ggplot(data = realrate, aes(x = year, y = realrate)) + 
  geom_line()

## clean politics data
## fill in cycle for prev year
politics <- rbind(politics, politics %>% mutate(cycle = cycle - 1))
politics$bank_name = tolower(politics$bank_name)

loans_politics = loans %>% ## take the last instance of the loan for now, but we need to fix this in the future
  group_by(lender_parent_name, deal_perm_id) %>%
  slice(n()) %>%
  ungroup() %>%
  select(lender_parent_name,
         year,
         lender_share,
         borrower_name,
         city, country,
         broad_industry_group,
         major_industry_group,
         sic_code,
         number_of_lenders,
         deal_amount,
         deal_active_date,
         completion_date,
         margin_bps) %>%
  inner_join(y = politics, 
             by = c("lender_parent_name" = "bank_name",
                    "year"= "cycle"))

## create concordance table 
tier_list <- list(
  a = build_tier(match_type = "exact", clean = TRUE),
  b = build_tier(match_type = "exact", clean = TRUE,
                 clean_settings = build_clean_settings(
                   common_words = rbind(tibble(abbr = c(
                     'incorporated',
                     'corporation',
                     'corpor',
                     'incorpor',
                     'holdings',
                     'group',
                     'the',
                     'usa'
                   ), long.names = ''), corporate_words),
                   stem = T,
                   remove_words = T))
)

df = loans_politics %>%
  ungroup() %>%
  mutate(id_1 = row_number()) %>%
  tier_match(data2 = esg %>% 
               group_by(issuer_name) %>% 
               slice(n()) %>%
               ungroup() %>%
               mutate(id_2 = row_number()),
             by.x = 'borrower_name', by.y = 'issuer_name',
             unique_key_1 = 'id_1', unique_key_2 = 'id_2',
             tiers = tier_list, verbose = T)
df$match_evaluation
## threshold that seems reasonable by inspection 
## correct
check <- df$matches %>% select(borrower_name, issuer_name, tier) %>% group_by(borrower_name) %>% slice(n())
# looks good!

df_matched <- df$matches %>%
  select(!id_2:tier) %>%
  select(!id_1) %>%
  mutate(cycle = plyr::round_any(year, 2)) %>%
  left_join(fedfunds, by = 'year') %>%
  left_join(realrate, by = 'year') %>%
  left_join(avg_deal, by = c('major_industry_group', 'lender_parent_name', 'cycle')) %>%
  left_join(avg_spread, by = c('major_industry_group', 'lender_parent_name', 'cycle')) %>%
  left_join(risk %>% select(!spcsrc), by = c('year', 'issuer_ticker' = 'tic')) %>%
  mutate(deal_diff = (deal_amount - avg_deal_amount)/avg_deal_amount) %>%
  mutate(weighted_average_score = weighted_average_score/max(weighted_average_score, na.rm = T),
         industry_adjusted_score = industry_adjusted_score/max(industry_adjusted_score, na.rm = T)) %>%
  mutate(invest_grade = if_else(splticrm == "", NA, invest_grade))
## code investment grade 

saveRDS(df_matched, file = '../data/final_dataset2.RDS')






