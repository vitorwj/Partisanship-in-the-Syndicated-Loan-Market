## clean opensecrets donation data 
## merge with organization information
## get summary stats for firms 
library(tidyverse)
library(ggplot2)
library(lubridate)

## IMPORT POLITICS DATA
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df18 = read.csv("../data/2018_cycle.csv")
df20 = read.csv("../data/2020_cycle.csv")
df17 = read.csv("../data/pre_2018_contributions.csv") %>%
  dplyr::rename(recipcode = recip_code,
         orgname = org_name,
         contribid = contributor_id)
df_final = read.csv( "C:/Users/vitor/Arquivos/Writing Sample/OpenSecrets Data/donors_data_2002_2014.csv") %>%
  select(-X)
politics_un <- rbind(df18, df20) %>% rbind(df17)
rm(df17, df18, df20)
politics = filter(politics_un, cycle >= 2016) %>%
  rbind(df_final)

# define list of banks 
banks <- c(
  'Truist Financial',
  'Bofa Securities',
  'Bank of New York Mellon',
  'Bank of New York Mellon',
  'Cit Group Incorporated',
  'Citi',
  'Comerica Incorporated',
  'Fifth Third Bank',
  'First Bank National Association',
  'General Electric Capital Corporation',
  'Goldman Sachs and Company',
  'Hibernia Corp',
  'JP Morgan',
  'JP Morgan',
  'Jefferies',
  'Keybank',
  'Mandt Bank',
  'Bofa Securities',
  'Morgan Stanley',
  'Northern Trust Corporation',
  'PNC Bank',
  'Regions Bank',
  'SVB Financial Group',
  'Southtrust Bank',
  'State Street Bank',
  'US Bancorp',
  'Wells Fargo and Company',
  'Zions Bancorporation'
)
names(banks) <- c(
  'BB&T',
  'BANK OF AMERICA',
  'BANK OF NEW YORK MELLON',
  'BNY MELLON',
  'CIT GROUP',
  'CITI',
  'COMERICA',
  'FIFTH THIRD',
  'FIRST BANK',
  'GENERAL ELECTRIC CAPITAL',
  'GOLDMAN SACHS',
  'HIBERNIA CORP',
  'JP MORGAN',
  'JPM',
  'JEFFERIES',
  'KEY BANK',
  'M&T BANK',
  'MERRILL LYNCH',
  'MORGAN STANLEY',
  'NORTHERN TRUST',
  'PNC BANK',
  'REGIONS BANK',
  'SVB FINANCIAL GROUP',
  'SOUTHTRUST',
  'STATE STREET',
  'US BANCO',
  'WELLS FARGO',
  'ZIONS BANCO'
)

# sort w/ names 
politics <- politics %>% mutate(clean_employer = '')
for (name in names(banks)) {
  politics <- politics %>% mutate(clean_employer = if_else(grepl(name, employer),
                                               banks[[name]], clean_employer))
}

# summary occupation
occupation <- politics %>% group_by(occupation) %>% dplyr::summarize(n = n())
# remove large categories with no relevance to banking 
filter <- c(
  "SOFTWARE ENGINEER",
  "SOFTWARE DEVELOPER",
  "IT",
  "TELLER",
  "PROGRAMMER",
  "COMPUTER PROGRAMMER",
  "LAWYER",
  "RETIREE",
  "RETIRED",
  "ATTORNEY" 
)
politics <- politics %>% filter(!grepl(paste0(filter, collapse = "|"), occupation))

# code orgs
politics = politics %>%
  mutate(recip = ifelse(recipcode == "DL" | recipcode == "DW" | recipcode == "DP" | recipcode == "DI" | recipcode == "DN" | recipcode == "DO", "D", 
                        ifelse(recipcode == "RL" | recipcode == "RW" | recipcode == "RP" | recipcode == "RI" | recipcode == "RN" | recipcode == "RO", "R", 0))) %>%
   filter(recip == "R" | recip == "D")

## drop duplicate contributions per cycle 
stat_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

politics <- politics %>%
  group_by(cycle, contribid) %>%
  mutate(recip = stat_mode(recip)) %>% # modal donation as "cycle id"
  slice(n()) %>%
  ungroup()
## now we're left w just 27k 

# coding for individual banks 
# graph results 
#library(plyr) # CANNOT RUN STAT_MODE WITH THIS LOADED
library(ggrepel)

top = politics %>% group_by(clean_employer) %>% dplyr::summarize(n = n()) %>% ungroup() %>% arrange(desc(n)) %>% slice(1:8) %>% pull(clean_employer)

clean_politics = politics %>%
  select(date, cycle, amount, clean_employer, recip) %>%
  mutate(date = as.Date(date)) %>% 
  mutate(dem = if_else(recip == "D", 1, 0)) %>%
  group_by(cycle, clean_employer) %>%
  dplyr::summarize(share_d = sum(dem)/n(), n_donations = n()) %>%
  ungroup() %>%
  mutate(dem_55 = case_when(share_d >= .55 ~ T,
                            share_d <= .45 ~ F,
                            T ~ NA),
         dem_60 = case_when(share_d >= .60 ~ T,
                            share_d <= .40 ~ F,
                            T ~ NA)) 


data_ends <- clean_politics %>% 
  filter(clean_employer %in% top) %>%
  filter(cycle == 2020)
clean_politics %>%
  filter(clean_employer %in% top) %>%
  filter(cycle >= 2002) %>%
  ggplot(aes(x = cycle, y = share_d, color = clean_employer)) +
  geom_line() + 
  theme_minimal() +
  scale_x_continuous(limits = c(2002, 2025),
                     breaks = seq(2002,2020,2) ) +
  theme(legend.position = "none") +
  scale_color_brewer(type = 'qual', palette = 2) +
  geom_text_repel(
    data = data_ends,
    aes(label = clean_employer),
    nudge_x = 2, direction = "y", hjust = "left",
    min.segment.length = 2
  ) +
  labs(title = "Share of donations to democrats over time") +
  ylab("Share of donations to democrats") + xlab("Election cycle")


### FILTER FOR TOP POSITIONS 

filter2 = c("CEO", 
            "VP", 
            "CHAIR",
            "CHAIRMAN",
            "VICE PRESIDENT",
            "PRESIDENT",
            "CFO",
            "VICE PRESIDEN",
            "VICE CHAIRMAN",
            "V P",
            "SVP",
            "SENIOR VICE PRESIDENT"
           )

imp_politics = politics
imp_politics$occupation = toupper(imp_politics$occupation) 
imp_politics$occupation = gsub("\\.", "", imp_politics$occupation)
imp_politics = imp_politics %>% filter(grepl(paste0(filter2, collapse = "|"),
                                             occupation))

# code orgs
imp_politics <- imp_politics %>% mutate(recip = case_when(substr(recipcode, 1, 1) == "D" ~ "D",
                                                  substr(recipcode, 1, 1) == "R" ~ "R",
                                                  T ~ "NA")) %>%
  filter(recip != "NA")

## drop duplicate contributions per cycle 
imp_politics <- imp_politics %>%
  group_by(cycle, contribid) %>%
  mutate(recip = stat_mode(recip)) %>% # modal donation as "cycle id"
  slice(n()) %>%
  ungroup()
## now we're left w just 2.6k (1/10th)

# combine w kempf data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readstata13)
# run clean-politics-data to get imp_politics and clean_politics

politics = clean_politics %>%
  left_join(imp_politics %>%
              select(cycle, amount, clean_employer, recip) %>%
              mutate(dem = if_else(recip == "D", 1, 0)) %>%
              group_by(cycle, clean_employer) %>%
              dplyr::summarize(share_d_imp = sum(dem)/n(), n_donations_imp = n()) %>%
              ungroup() %>%
              mutate(dem_55_imp = case_when(share_d_imp >= .55 ~ T,
                                            share_d_imp <= .45 ~ F,
                                            T ~ NA),
                     dem_60_imp = case_when(share_d_imp >= .60 ~ T,
                                            share_d_imp <= .40 ~ F,
                                            T ~ NA)),
            by = c('cycle', 'clean_employer'))

# now join the kempf data
df_kempf = read.dta13("../data/CRP_raw_contrib.dta") %>% 
  dplyr::rename(dem_dollar = Democrats,
         gop_dollar = Republicans) %>%
  mutate(share = dem_dollar/(dem_dollar+gop_dollar)) %>%
  mutate(kempf_dem = case_when(share >= 0.55 ~ 1,
                               share <= 0.45 ~ 0)) %>%
  dplyr::rename(kempf_share = share) %>% 
  dplyr::rename(cycle = year)

gvkey <- c('011856',
           '011856',
           '002019',
           '149738',
           '003243',
           '003231',
           '004640',
           '177009',
           '005047',
           '114628',
           '002968',
           '006682',
           '009783',
           '004699',
           '012124',
           '007982',
           '008245',
           '004674',
           '017120',
           '010035',
           '004723',
           '008007',
           '011687')
banks <- c(
  'Truist Financial',
  'Bofa Securities',
  'Bank of New York Mellon',
  'Cit Group Incorporated',
  'Citi',
  'Comerica Incorporated',
  'Fifth Third Bank',
  'First Bank National Association',
  'General Electric Capital Corporation',
  'Goldman Sachs and Company',
  'JP Morgan',
  'Jefferies',
  'Keybank',
  'Mandt Bank',
  'Morgan Stanley',
  'Northern Trust Corporation',
  'PNC Bank',
  'Regions Bank',
  'SVB Financial Group',
  'State Street Bank',
  'US Bancorp',
  'Wells Fargo and Company',
  'Zions Bancorporation'
)

gvkey = tibble(banks, gvkey)
df_kempf = df_kempf %>%
  left_join(gvkey %>%
              mutate(gvkey = as.numeric(gvkey)), by = c('gvkey_bank' = 'gvkey'))
politics = politics %>% 
  left_join(df_kempf, by = c('cycle' = 'cycle', 'clean_employer' = 'banks'))

## politics has everything
politics = politics %>%
  dplyr::rename(bank_name = clean_employer,
         share_donations_d = share_d,
         n_donations_all = n_donations,
         share_donations_d_imp = share_d_imp)

politics %>%
  select(cycle, bank_name, share_donations_d, share_donations_d_imp, kempf_share) %>%
  pivot_longer(share_donations_d:kempf_share, names_to = 'type', values_to = 'dem_share') %>%
  filter(bank_name == 'citi') %>% ggplot(aes(x = cycle, y = dem_share, fill = type)) +
  geom_col(position = 'dodge') + 
  theme_minimal()

lm(kempf_share ~ n_donations_all + cycle + share_donations_d, data = politics) %>%
  summary()
glm(kempf_dem ~ n_donations_all + cycle + dem_55, data = politics, family = "binomial") %>%
  summary()

1/(1+exp(-(-46.25))) - 1/(1+exp(-(1.62-46.25)))

politics %>%
  ggplot(aes(x = share_donations_d, y = kempf_share,
             color = factor(cycle))) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  theme_minimal() +
  xlab("Share Dem. Contributors") + ylab("Share Dem. $s") + 
  labs(title = "Comparing Firm Classification") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_brewer(type = 'seq', palette = 4, name = "Cycle")
politics %>%
  ggplot(aes(x = share_donations_d_imp, y = kempf_share,
             color = factor(cycle))) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  scale_y_continuous(limits = c(0,1)) +
  theme_minimal() +
  xlab("Share Dem. Contributors (Executives)") + ylab("Share Dem. $s") + 
  labs(title = "Comparing Firm Classification") +
  scale_color_brewer(type = 'seq', palette = 4, name = "Cycle")

write_csv(politics, "bank_politics_comparison.csv")

politics %>%
  select(cycle, bank_name, share_donations_d, share_donations_d_imp, kempf_share) %>%
  pivot_longer(share_donations_d:kempf_share, names_to = 'type', values_to = 'dem_share') %>%
  filter(bank_name == name) %>% ggplot(aes(x = cycle, y = dem_share, fill = type)) +
  geom_col(position = 'dodge') + 
  theme_minimal()
## politics is everything
rm(clean_politics, df_kempf, imp_politics, occupation)

# politics 

## add in additional bank data
library(readxl)
library(janitor)
more_bank <- read_excel("../data/additional_banks_data.xlsx") %>%
  clean_names() %>%
  rename(gvkey = global_company_key) %>%
  select(gvkey, data_date, assets_total:total_borrowings)
more_bank <- more_bank %>%
  mutate(gvkey = as.numeric(gvkey)) %>%
  mutate(cycle = year(data_date)) %>%
  group_by(cycle, gvkey) %>%
  slice(1) %>%
  ungroup() %>%
  select(!data_date)
## fix weird gvkey stuffw citibank 
more_bank <- more_bank %>%
  mutate(gvkey = case_when(gvkey == 3066 ~ 3243,
                           T ~ gvkey))

politics <- politics %>%
  left_join(more_bank, by = c('gvkey_bank' = 'gvkey', 'cycle'))
rm(more_bank)

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
bank_assets = read.csv("../data/bank_assets.csv") %>%
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
  summarize(fed_consol_assets = mean(consol_assets),
            fed_domestic_assets = mean(domestic_assets))
politics$bank_name = tolower(politics$bank_name)

politics <- politics %>%
  left_join(bank_assets, by = c('cycle' = 'year', 'bank_name' = 'clean_name')) 

saveRDS(politics, file = 'bank_data.RDS')
#rm(bank_assets)
#rm(gvkey)






