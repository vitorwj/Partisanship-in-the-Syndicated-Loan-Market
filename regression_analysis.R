library(tidyverse)
library(lubridate)
library(estimatr)
library(gtsummary)
library(ggrepel)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df = readRDS('../data/final_dataset2.RDS') %>%
  mutate(deal_amount = log(deal_amount))

df %>% 
  #filter(lender_parent_name == 'jp morgan') %>%
  group_by(cycle) %>%
  select(share_donations_d) %>%
  #slice(1) %>%
  mutate(avg_donations = mean(share_donations_d)) %>%
  ggplot(aes(x = cycle, y = avg_donations)) +
  geom_line()

# top 5 industries 
top_firms = df %>% select(borrower_name, weighted_average_score) %>% group_by(borrower_name) %>% slice(1)
df %>% 
  group_by(borrower_name) %>%
  summarize(weighted_average_score = mean(weighted_average_score, na.rm = T),
            major_industry_group = first(major_industry_group)) %>%
  ggplot(aes(x = weighted_average_score)) +
  geom_histogram(bins = 20) +
  theme_minimal() +
  xlab("ESG Score") + ylab("Count") +
  labs(title = "ESG Score Distribution in Sample")
ggsave("../figures/esg_scores.pdf", width = 8, height = 5, units = "in")
## graph politics over time 
top = df %>% filter(lender_parent_name != "first bank national association") %>%
  group_by(lender_parent_name) %>% slice(1) %>% ungroup() %>% 
  arrange(desc(n_donations_all)) %>% slice(1:8) %>% pull(lender_parent_name)
data_ends <- df %>% 
  filter(lender_parent_name %in% top) %>%
  filter(cycle == 2020) %>%
  group_by(lender_parent_name) %>%
  slice(1)
df %>%
  filter(lender_parent_name %in% top) %>%
  filter(cycle >= 2000) %>%
  group_by(cycle, lender_parent_name) %>%
  slice(1) %>%
  ggplot(aes(x = cycle, y = share_donations_d, color = lender_parent_name)) +
  geom_line() + 
  theme_minimal() +
  scale_x_continuous(limits = c(2000, 2025),
                     breaks = seq(2000,2020,2) ) +
  theme(legend.position = "none") +
  scale_color_brewer(type = 'qual', palette = 2) +
  geom_text_repel(
    data = data_ends,
    aes(label = lender_parent_name),
    nudge_x = 2, direction = "y", hjust = "left",
    min.segment.length = 2
  ) +
  labs(title = "Share of individual democratic donors over time by firm") +
  ylab("Share of donations to democrats") + xlab("Election cycle")
ggsave("../figures/share_donations_indiv.pdf", width = 8, height = 5, units = "in")
df %>%
  filter(lender_parent_name %in% top) %>%
  filter(cycle >= 2000) %>%
  group_by(cycle, lender_parent_name) %>%
  slice(1) %>%
  ggplot(aes(x = cycle, y = kempf_share, color = lender_parent_name)) +
  geom_line() + 
  theme_minimal() +
  scale_x_continuous(limits = c(2000, 2025),
                     breaks = seq(2000,2020,2) ) +
  theme(legend.position = "none") +
  scale_color_brewer(type = 'qual', palette = 2) +
  geom_text_repel(
    data = data_ends,
    aes(label = lender_parent_name),
    nudge_x = 2, direction = "y", hjust = "left",
    min.segment.length = 2
  ) +
  labs(title = "Share of dollars to democrats over time by firm") +
  ylab("Share of dollars to democrats") + xlab("Election cycle")
ggsave("../figures/share_donations_dollars.pdf", width = 8, height = 5, units = "in")

## summary
df %>% summarize(n = n_distinct(borrower_name))
df %>% summarize(n = n_distinct(lender_parent_name))

## outcome var should be like "share of all loans from bank"
## but we can only identify a share of the loans, so need to adjust somewhat 
library(stargazer)
library(sandwich)
## 1 avg deal size for industry group from dealscan 
## regress outcome is diff vs average loan 
## assumption is that the firms we have scores for are random sample of firms (?)

## IF USING FINAL_DATASET2, USE FED_DOMESTIC_ASSETS. IF USING FINAL_DATASET, USE ASSETS_TOTAL

library(lfe)


lm1 = felm(deal_amount ~ weighted_average_score + weighted_average_score:kempf_dem + realrate +  fed_domestic_assets | 
       factor(major_industry_group)*factor(cycle) | 0 | lender_parent_name,
     data = df,
     cmethod = "reghdfe") 
lm2 = felm(deal_amount ~ weighted_average_score + weighted_average_score:dem_55 + realrate + fed_domestic_assets | 
       factor(cycle)*factor(major_industry_group) | 0 | lender_parent_name,
     data = df,
     cmethod = "reghdfe") 
lm3 = felm(deal_amount ~ weighted_average_score + weighted_average_score:dem_55_imp + realrate + assets_total | 
       factor(cycle)*factor(major_industry_group) | 0 | lender_parent_name,
     data = df,
     cmethod = "reghdfe") 
lm4 = felm(deal_amount ~ weighted_average_score + weighted_average_score:dem_60 + realrate + assets_total | 
       factor(cycle)*factor(major_industry_group) | 0 | lender_parent_name,
     data = df,
     cmethod = "reghdfe") 
lm5 = felm(margin_bps ~ weighted_average_score + weighted_average_score:dem_55 + realrate + assets_total | 
       factor(major_industry_group)*factor(cycle) | 0 | lender_parent_name,
     data = df,
     cmethod = "reghdfe")
lm6 = felm(deal_amount ~ weighted_average_score + weighted_average_score:kempf_dem + assets_total | 
             factor(major_industry_group)*factor(cycle) | 0 | lender_parent_name,
           data = df,
           cmethod = "reghdfe")
summary(lm2)


stargazer(lm1, lm2, lm3, lm4, lm5,
          omit.stat = "f",
          title = "Results", align=TRUE,
          type='latex', summary=FALSE)

df_restrict = df %>% filter(cycle >= 2012)
top_firms = df_restrict %>% group_by(cycle, lender_parent_name) %>% slice(1)

lm1 = felm(deal_amount ~ weighted_average_score + weighted_average_score:kempf_dem + realrate + fed_domestic_assets | 
             factor(major_industry_group)*factor(cycle) | 0 | lender_parent_name,
           data = df_restrict,
           cmethod = "reghdfe") 
lm2 = felm(deal_amount ~ weighted_average_score + weighted_average_score:dem_55 + realrate + fed_domestic_assets | 
             factor(cycle)*factor(major_industry_group) | 0 | lender_parent_name,
           data = df_restrict,
           cmethod = "reghdfe") 
lm3 = felm(deal_amount ~ weighted_average_score + weighted_average_score:dem_55_imp + realrate + fed_domestic_assets | 
             factor(cycle)*factor(major_industry_group) | 0 | lender_parent_name,
           data = df_restrict,
           cmethod = "reghdfe") 
lm4 = felm(deal_amount ~ weighted_average_score + weighted_average_score:dem_60 + realrate + fed_domestic_assets | 
             factor(cycle)*factor(major_industry_group) | 0 | lender_parent_name,
           data = df_restrict,
           cmethod = "reghdfe") 
lm5 = felm(margin_bps ~ weighted_average_score + weighted_average_score:kempf_dem + realrate + fed_domestic_assets | 
             factor(major_industry_group)*factor(cycle) | 0 | lender_parent_name,
           data = df_restrict,
           cmethod = "reghdfe")


stargazer(lm1, lm2, lm3, lm4, lm5,
          omit.stat = "f",
          title = "Results", align=TRUE,
          type='latex', summary=FALSE)


## investment grade 
library(janitor)
df$splticrm <- factor(df$splticrm , levels = c(
  "AAA", "AA+", "AA", "AA-", "A+", "A", "A-", "BBB+", "BBB", "BBB-",
  "BB+", "BB", "BB-", "B+", "B", "B-", "CCC+", "D"
))
df = df %>% mutate(splticrm_score = case_when(splticrm == "AAA" ~ 0,
                                splticrm == "AA+" ~ 1, 
                                splticrm == "AA" ~ 2, 
                                splticrm == "AA-" ~ 3, 
                                splticrm == "A+" ~ 4, 
                                splticrm == "A" ~ 5, 
                                splticrm == "A-" ~ 6, 
                                splticrm == "BBB+" ~ 7, 
                                splticrm == "BBB" ~ 8, 
                                splticrm == "BBB-" ~ 9,
                                splticrm == "BB+" ~ 10, 
                                splticrm == "BB" ~ 11, 
                                splticrm == "BB-" ~ 12, 
                                splticrm == "B+" ~ 13, 
                                splticrm == "B" ~ 14, 
                                splticrm == "B-" ~ 15, 
                                splticrm == "CCC+" ~ 16, 
                                splticrm == "D" ~ 17))
lm(weighted_average_score ~ splticrm_score, data = df) %>% summary()
df %>%
  filter(!is.na(splticrm)) %>%
  filter(splticrm != "") %>%
  ggplot(aes(x = splticrm, y = weighted_average_score, fill = invest_grade)) +
  geom_boxplot() + 
  theme_minimal() + 
  xlab("S&P Issuer Credit Rating") +
  ylab("MSCI Weighted Average Score") +
  scale_fill_brewer(name = "Investment Grade",
                    type = 'qual', palette = 2, direction = -1)
ggsave("../figures/credit_scores.pdf", width = 8, height = 5, units = "in")

ig_list = c(
  "AAA", "AA+", "AA", "AA-", "A+", "A", "A-", "BBB+", "BBB", "BBB-"
)
risk = risk %>%
  mutate(invest_grade = if_else(splticrm %in% ig_list, T, F))
## show if eventually became non investment grade 

lm1 = felm(deal_amount ~ weighted_average_score + weighted_average_score:dem_60 + fedfunds + domestic_assets | 
             factor(cycle) | 0 | lender_parent_name,
           data = df,
           cmethod = "reghdfe")
lm2 = felm(deal_amount ~ weighted_average_score + weighted_average_score:dem_60 + fedfunds + domestic_assets | 
             factor(cycle)*factor(broad_industry_group) | 0 | lender_parent_name,
           data = df,
           cmethod = "reghdfe")
lm3 = felm(deal_amount ~ weighted_average_score + weighted_average_score:dem_60 + fedfunds + domestic_assets | 
             factor(cycle)*factor(major_industry_group) | 0 | lender_parent_name,
           data = df,
           cmethod = "reghdfe")
lm4 = felm(deal_amount ~ weighted_average_score + weighted_average_score:dem_55 + fedfunds + domestic_assets | 
             factor(cycle)*factor(broad_industry_group) | 0 | lender_parent_name,
           data = df,
           cmethod = "reghdfe")
lm5 = felm(deal_amount ~ industry_adjusted_score + industry_adjusted_score:dem_60 + fedfunds + domestic_assets | 
             factor(cycle)*factor(broad_industry_group) | 0 | lender_parent_name,
           data = df,
           cmethod = "reghdfe")

lm6 = felm(deal_amount ~ industry_adjusted_score + industry_adjusted_score:share_donation_zero + fed_domestic_assets | 
             factor(major_industry_group)*factor(cycle) + factor(cycle) | 0 | lender_parent_name,
           data = df,
           cmethod = "reghdfe")
summary(lm6)

