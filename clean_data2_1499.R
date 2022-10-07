## Packages
library(janitor)
library(tidyverse)
library(ggplot2)


## Importing data set (I selected only the variables that I considered useful for us,
## otherwise the file would be way larger. I have my online query saved, though)

## The loans are from 2000-01-01 to 2020-12-31
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


hist(loans$deal_amount_converted, breaks = 1000, xlim = c(0, 5000))
## Following Ivashina and Becker (2012), remove SIC Codes between 600 and 699 (financial sector)
## I am not sure if it is needed, so I am not going to do it.
## loans <- loans[loans$NSIC < 600 | loans$NSIC > 699, ]

## summary(loans)

## write.csv(loans, "C:/Users/vitor/Arquivos/Harvard/ECON 1499/Term Paper/loans2_clean_2000_2020.csv")

