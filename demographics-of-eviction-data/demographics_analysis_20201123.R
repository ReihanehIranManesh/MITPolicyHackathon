# Demographics of Eviction 

# Renee 10.30.2020
# Last Modified: 11.09.2020 (edit prop_moe function, add code for table A1)

# This script loads county-level estimates as well as renter-level estimates
# to calculate descriptive statistics and figures/tables for the main text
# and table A1

#------- SET UP ----------
library(tidyverse)
library(magrittr)
library(here)
library(data.table)

# load county-level denominators, all_county_denominators
fread(file = here("Uncertainty Update",
                  "Working Files",
                  "county_denominators.csv")) %>%
  mutate(cofips = str_pad(cofips,
                          width = 5,
                          side = "left",
                          pad = "0")) -> all_county_denominators

# load file containing county estimates for numerators; results_all
fread(file = here("Uncertainty Update",
                  "Working Files",
                  "county_numerators.csv")) %>% 
  mutate(cofips = str_pad(cofips,
                          width = 5,
                          side = "left",
                          pad = "0")) -> results_all

# load renter-per-line file, list of dataframes by county; 
# "expanded_rates_all"
load(file = here("Working Files",
                 "expanded_rates_all.rda"))

all_county_denominators %<>% 
  filter(cofips %in% results_all$cofips)

# make zero counts explicit; change hisp category to latino
all_county_denominators %<>% 
  mutate(race = ifelse(race == "hisp",
                       "latino",
                       race)) %>% 
  group_by(state, county, cofips) %>% 
  complete(race = c("asian", "black", "latino", "white", "other"),
           sex = c("female", "male"),
           fill = list(renters = 0,
                       variance = 0))

##--------- DEFINE FUNCTIONS -------

prop_moe <- function(x, y, a, b){
  p <- x/y
  undersqr <- (a^2 - ((p^2)*(b^2)))
  return(ifelse(undersqr > 0,
                ((1/y)*sqrt(undersqr)),
                ((1/y)*sqrt(a^2 + ((p^2)*(b^2))))
  ) )
}

sum_moes <- function(x){
  return(sqrt(sum(x^2)))
}

product_moe <- function(x, y, a, b){
  first <- (x^2)*(b^2)
  second <- (y^2)*(a^2)
  return(sqrt(first + second))
}

#------- GENERAL STATS ---------
# how many eviction filings recorded? on average
results_all %>% 
  filter(metric == "filing",
         parameter == "point") %>% 
  pull(value) %>% 
  sum()
# 1.44 million filings

# how many judgments?
results_all %>% 
  filter(metric == "evict",
         parameter == "point") %>% 
  pull(value) %>% 
  sum()
# 660,000 judgments

#------- COUNTY LEVEL RACIAL DISPARITIES --------
# proportion of renters from each racial group in our sample

# work with moe, convert back to variance after
all_county_denominators %>% 
  mutate(moe = sqrt(variance)*1.645) %>% 
  group_by(race) %>% 
  summarize(renters = sum(renters),
            moe = sum_moes(moe)) %>% 
  ungroup() %>% 
  mutate(total = sum(renters),
         total.moe = sum_moes(moe)) %>% 
  mutate(prop = renters/total,
         prop.moe = prop_moe(renters, total, moe, total.moe),
         prop.var = (prop.moe/1.645)^2,
         lwb = prop - 2*sqrt(prop.var),
         upb = prop + 2*sqrt(prop.var)) %>% 
  select(race, prop, lwb, upb) %>% View()

# proportion of defendants from each racial group in our sample
results_all %>% 
  filter(metric == "filing") %>% 
  group_by(race, parameter) %>% 
  summarize(value = sum(value)) %>% 
  pivot_wider(names_from = parameter,
              values_from = value) %>% 
  ungroup() %>% 
  mutate(denom = sum(point)) %>% 
  mutate(prop.p = point/denom,
         prop.var = variance/(denom^2)) %>% 
  mutate(lower.b = (prop.p - 2*(sqrt(prop.var))),
         upper.b = (prop.p + 2*(sqrt(prop.var)))) %>%
  select(race, prop.p, lower.b, upper.b) %>% 
  mutate_at(.vars = vars(-race),
            .funs = ~.*100) %>% View()

# what % of black renters live in a county where the share of Black defendants > share of Black renters?
results_all %>% 
  filter(metric == "filing") %>% 
  pivot_wider(names_from = parameter,
              values_from = value) %>% 
  group_by(cofips, race) %>% 
  summarize(point = sum(point),
            variance = sum(variance)) %>% 
  group_by(cofips) %>% 
  mutate(denom = sum(point)) %>% 
  mutate(prop.p = point/denom,
         prop.var = variance/(denom^2)) %>% 
  filter(race == "black") %>% 
  select(-race) %>% 
  mutate(lower.b = (prop.p - 2*(sqrt(prop.var))),
         upper.b = (prop.p + 2*(sqrt(prop.var)))) %>% 
  select(cofips, prop.p, lower.b, upper.b) %>% 
  left_join(all_county_denominators %>%
              group_by(cofips, race) %>% 
              summarize(renters = sum(renters, na.rm = T)) %>% 
              group_by(cofips) %>% 
              mutate(prop.renters = renters/sum(renters, na.rm = T)) %>% 
              filter(race == "black") %>% 
              select(-race)) %>% 
  mutate(point.higher = prop.p>prop.renters,
         lwb.higher = lower.b>prop.renters,
         upb.higher = upper.b>prop.renters) %>%
  select(cofips, renters, point.higher, lwb.higher, upb.higher) -> black_county_compare
  
black_county_compare %>% 
  group_by(point.higher) %>% 
  summarise(total_renters = sum(renters, na.rm = T)) %>% 
  mutate(total_renters_prop = total_renters/sum(total_renters, na.rm = T)) %>% View()
# point estimate = 80.981% of black renters live in a county where % defendants > % renters

black_county_compare %>% 
  group_by(lwb.higher) %>% 
  summarise(total_renters = sum(renters)) %>% 
  mutate(total_renters_prop = total_renters/sum(total_renters, na.rm = T)) %>% View()
# lower bound: 78.726%

black_county_compare %>% 
  group_by(upb.higher) %>% 
  summarise(total_renters = sum(renters)) %>% 
  mutate(total_renters_prop = total_renters/sum(total_renters, na.rm = T)) %>% View()
# upper bound: 86.404% 
  
##--------- FIGURE 1 -------
results_all %>% 
  filter(metric %in% c("filing", "evict")) %>% 
  group_by(race, parameter, metric) %>% 
  summarize(value = sum(value)) %>%
  group_by(metric, parameter) %>% 
  mutate(denom = sum(value),
         value = ifelse(parameter == "point",
                        value/denom,
                        value/(denom^2))) %>%
  select(-denom) %>%
  pivot_wider(names_from = c(parameter),
              values_from = value,
              names_sep = "_") %>% 
  ungroup() %>% 
  mutate(lower.b = (point - 2*(sqrt(variance))),
         upper.b = (point + 2*(sqrt(variance)))) %>%
  bind_rows(all_county_denominators %>% 
              mutate(race = ifelse(race == "hisp",
                                   "latino",
                                   race)) %>% 
              mutate(moe = sqrt(variance)*1.645) %>% 
              group_by(race) %>% 
              summarize(renters = sum(renters),
                        moe = sum_moes(moe)) %>% 
              ungroup() %>% 
              mutate(total = sum(renters),
                     total.moe = sum_moes(moe)) %>% 
              mutate(prop = renters/total,
                     prop.moe = prop_moe(renters, total, moe, total.moe),
                     prop.var = (prop.moe/1.645)^2,
                     lower.b = prop - 2*sqrt(prop.var),
                     upper.b = prop + 2*sqrt(prop.var)) %>%
              mutate(point = prop,
                     variance = prop.var,
                     metric = "renters") %>% 
              select(race, metric, point, variance, lower.b, upper.b)) %>% 
  mutate(race = ifelse(race == "latino",
                       "Latinx",
                       str_to_title(race)),
         metric = fct_relevel(as.factor(metric),
                              "renters",
                              "filing",
                              "evict")) %>% 
  filter(race != "Other") %>% 
  arrange(race) %>%
  ggplot(aes(x = race,
             y = point,
             fill = metric)) +
  geom_col(width = 0.8,
           position = position_dodge(width = 0.85),
           color = "black") +
  geom_errorbar(aes(ymin = lower.b, ymax = upper.b),
                position = position_dodge(width = 0.85),
                width = 0.6,
                color = "darkred") +
  scale_fill_grey(start = 1,
                  end = 0,
                  labels = c("Share of Renters", "Share of Defendants", "Share of Judgments")) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = "", fill = "") +
  theme(legend.position = "bottom")

ggsave(filename = here("Figures", 
                       "figure1.png"))

##--------- TABLE 1 -----------
# vector of top ten largest counties
all_county_denominators %>% 
  group_by(cofips) %>% 
  summarize(renters = sum(renters)) %>% 
  arrange(desc(renters)) %>% 
  slice(1:10) %>% 
  pull(cofips) -> large_counties

# prepare denominators file
all_county_denominators %>% 
  mutate(moe = sqrt(variance)*1.645) %>% 
  filter(cofips %in% large_counties) %>% 
  group_by(cofips, race) %>% 
  summarize(renters = sum(renters),
            renters_moe = sum_moes(moe)) %>% 
  group_by(cofips) %>% 
  mutate(total_renters = sum(renters),
         total_moe = sum_moes(renters_moe)) %>% 
  ungroup() %>% 
  mutate(renters_prop = renters/total_renters,
         renters_prop_moe = prop_moe(renters, total_renters, renters_moe, total_moe),
         renters_prop_se = renters_prop_moe/1.645) %>%
  select(cofips, race, total_renters, renters_prop, renters_prop_se) -> denoms_b_race

results_all %>% 
  filter(metric == "filing") %>% 
  pivot_wider(names_from = parameter,
              values_from = value) %>% 
  group_by(cofips, race) %>% 
  summarize(point = sum(point),
            variance = sum(variance)) %>% 
  group_by(cofips) %>% 
  mutate(denom = sum(point)) %>% 
  mutate(prop.p = point/denom,
         prop.var = variance/(denom^2)) %>% 
  filter(race %in% c("black", "white", "latino")) %>%
  # select(-race) %>% 
  mutate(prop.se = sqrt(prop.var)*100,
         prop.p = prop.p*100) %>% 
  # mutate(lower.b = (prop.p - 2*(sqrt(prop.var))),
  #        upper.b = (prop.p + 2*(sqrt(prop.var)))) %>% 
  select(cofips, race, prop.p, prop.se) %>% 
  filter(cofips %in% large_counties) %>% 
  left_join(denoms_b_race) %>% 
  arrange(desc(total_renters)) %>% 
  mutate(renters_prop = renters_prop*100,
         renters_prop_se = renters_prop_se*100) -> table_1_df

View(table_1_df)

# get average diff between prop black defendants and prop black renters
table_1_df %>% 
  filter(race == "black") %>% 
  mutate(diff = (prop.p)-(renters_prop)) %>% 
  pull(diff) %>% 
  mean()
# 12.43143

# get variance of this estimate
table_1_df %>% 
  filter(race == "black") %>% 
  mutate(var_sum = sum(prop.se^2, renters_prop_se^2)) %>% 
  pull(var_sum) %>% 
  sum()/100
# 0.0033822

12.43143 - 2*sqrt(0.0033822)
12.43143 + 2*sqrt(0.0033822)
  
#------- COUNTY LEVEL SEX DISPARITIES--------

# how many male or female evicted renters? using evict
results_all %>% 
  ungroup() %>% 
  filter(metric == "evict",
         parameter == "point") %>%
  group_by(sex) %>% 
  summarize(total = sum(value))

# get confidence interval
results_all %>% 
  ungroup() %>% 
  filter(metric == "evict") %>%
  group_by(sex, parameter) %>% 
  summarize(total = sum(value)) %>% 
  pivot_wider(names_from = parameter,
              values_from = total) %>% 
  mutate(lwb = point - 2*(sqrt(variance)),
         upb = point + 2*(sqrt(variance)))

# get sex disparity within race
results_all %>% 
  ungroup() %>% 
  # filter(race == "black") %>% 
  filter(metric == "evict",
         sex != "unknown") %>%
  group_by(race, sex, parameter) %>% 
  summarize(total = sum(value)) %>% 
  pivot_wider(names_from = parameter,
              values_from = total) %>% 
  mutate(lwb = point - 2*(sqrt(variance)),
         upb = point + 2*(sqrt(variance)))

#------- CALCULATE RENTER-PER-LINE STATISTICS ---------

# expand out expanded_rates_all
bind_rows(expanded_rates_all) -> expanded_rates_all

# how many rows?
nrow(expanded_rates_all)
# 29,149,128 renters

# fix some things
## change negative rates to zero, change infinities to zero, NaNs to zero
expanded_rates_all %>% 
  mutate_at(.vars = vars(evict:serial_race_sex_ratio),
            .funs = function(x) ifelse(is.infinite(x) | is.nan(x) | x < 0, 0, x)) -> expanded_rates_all

## GENERAL STATS ##
expanded_rates_all %>% 
  summarize(filing_mean = mean(filing),
            filing_med = median(filing),
            filing_sd = sd(filing),
            evict_mean = mean(evict),
            evict_med = median(evict),
            evict_sd = sd(evict))

# by race
expanded_rates_all %>%
  group_by(race) %>%
  summarize(mean_filings = mean(filing_race,
                                na.rm = TRUE),
            med_filings = median(filing_race,
                                 na.rm = TRUE),
            sd_filings = sd(filing_race),
            med_evictions = median(evict_race,
                                   na.rm = TRUE),
            mean_evictions = mean(evict_race,
                                  na.rm = TRUE),
            sd_evictions = sd(evict_race))

# find what % of Black renters live in a county rate with an eviction rate more than double the white one
results_all %>% 
  filter(metric == "evict", parameter == "point") %>% 
  group_by(state, cofips, race) %>% 
  summarize(value = sum(value)) %>% 
  left_join(all_county_denominators %>% 
              group_by(state, cofips, race) %>% 
              summarize(renters = sum(renters))) %>% 
  mutate(erate = value/renters) %>% 
  filter(race %in% c("black", "white")) %>% 
  group_by(state, cofips) %>% 
  mutate(double = (erate[race == "black"]>= 2*(erate[race == "white"]))) %>% 
  filter(race == "black") %>% 
  group_by(double) %>% 
  summarize(renters = sum(renters)) %>% 
  mutate(prop = renters/sum(renters))

# run t.tests
# Latino-White Differences
expanded_rates_all %>% 
  filter(race %in% c("latino", "white")) -> lw

# filing rates
t.test(filing_race ~ race, 
       alternative = "greater",
       data = lw) 

# eviction rates
t.test(evict_race ~ race, 
       alternative = "greater",
       data = lw) 

## get race-sex rates
expanded_rates_all %>%
  group_by(race_sex) %>%
  summarize(mean_filings = mean(filing_race_sex,
                                na.rm = TRUE),
            sd_filings = sd(filing_race_sex),
            mean_evictions = mean(evict_race_sex,
                                  na.rm = TRUE),
            sd_evictions = sd(evict_race_sex))

##--------- FIGURE 2 ---------
expanded_rates_all %>% 
  select(cofips,
         race,
         sex,
         race_sex,
         filing_race_sex,
         evict_race_sex) %>% 
  pivot_longer(cols = c(filing_race_sex,
                        evict_race_sex),
               names_to = "type",
               values_to = "value") %>% 
  mutate(type = plyr::revalue(type,
                              c(filing_race_sex = "Filing Rate",
                                evict_race_sex = "Eviction Rate"))) %>% 
  filter(race %in% c("asian", "black", "latino", "white")) %>% 
  mutate(sex_rev = factor(ifelse(sex == "female",
                                 1,
                                 2),
                          labels = c("Female", "Male")),
         race_rev = factor(ifelse(race == "asian",
                                  1,
                                  ifelse(race == "black",
                                         2,
                                         ifelse(race == "latino",
                                                3,
                                                4))),
                           labels = c("Asian", "Black", "Latinx", "White"))) %>% 
  ggplot(aes(x = race_rev,
             y = value)) +
  geom_boxplot() +
  facet_grid(cols = vars(sex_rev),
             rows = vars(fct_rev(type))) +
  coord_cartesian(ylim = c(0, 0.1)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 11),
        axis.text = element_text(size = 11))

ggsave(filename = here("Figures",
                       "figure2.png"))

##--------- SEX RATIOS --------
expanded_rates_all %>%
  summarize(filing_ratio_means = mean(filing_sex_ratio,
                                      na.rm = TRUE),
            filing_ratio_medians = median(filing_sex_ratio,
                                          na.rm = TRUE),
            eviction_ratio_means = mean(evict_sex_ratio,
                                        na.rm = TRUE),
            eviction_ratio_medians = median(evict_sex_ratio,
                                            na.rm = TRUE))
# try calculating a different way?
results_all %>% 
  filter(metric %in% c("filing.adj", "evict"), 
         sex %in% c("female", "male"),
         parameter == "point") %>% 
  group_by(state, cofips, sex, metric) %>% 
  summarize(value = sum(value)) %>% 
  left_join(all_county_denominators %>% 
              group_by(state, cofips, sex) %>% 
              summarize(renters = sum(renters))) %>% 
  mutate(rate = value/renters) %>% 
  group_by(state, cofips, metric) %>% 
  mutate(ratio = rate[sex == "female"]/rate[sex == "male"],
         ratio = ifelse(is.infinite(ratio), 0, ratio)) %>%
  group_by(metric) %>% 
  summarize(mean = weighted.mean(ratio, renters, na.rm = T))

# by race
expanded_rates_all %>% 
  group_by(race) %>%
  summarize(filing_ratio_means = mean(filing_race_sex_ratio,
                                      na.rm = TRUE),
            filing_ratio_medians = median(filing_race_sex_ratio,
                                          na.rm = TRUE),
            eviction_ratio_means = mean(evict_race_sex_ratio,
                                        na.rm = TRUE),
            eviction_ratio_medians = median(evict_race_sex_ratio,
                                            na.rm = TRUE))

results_all %>% 
  filter(metric %in% c("filing.adj", "evict"), 
         sex %in% c("female", "male"),
         parameter == "point") %>% 
  group_by(state, cofips, sex, metric, race) %>% 
  summarize(value = sum(value)) %>% 
  left_join(all_county_denominators %>% 
              group_by(state, cofips, sex, race) %>% 
              summarize(renters = sum(renters))) %>% 
  mutate(rate = value/renters) %>% 
  group_by(state, cofips, metric, race) %>% 
  mutate(ratio = rate[sex == "female"]/rate[sex == "male"],
         ratio = ifelse(is.infinite(ratio), 0, ratio)) %>%
  group_by(metric, race) %>% 
  summarize(mean = weighted.mean(ratio, renters, na.rm = T))

##--------- FIGURE 3 ----------
expanded_rates_all %>%
  select(cofips,
         race,
         sex,
         race_sex,
         evict_race_sex_ratio) %>%
  rename(value = evict_race_sex_ratio) %>%
  drop_na() %>% 
  mutate(race_rev = factor(ifelse(race == "asian",
                                  1,
                                  ifelse(race == "black",
                                         2,
                                         ifelse(race == "latino",
                                                3,
                                                4))),
                           labels = c("Asian", "Black", "Latinx", "White"))) %>% 
  ggplot(aes(x = race_rev,
             y = value)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 1) +
  coord_cartesian(ylim = c(0, 2)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) -> fig3

fig3

ggsave(filename = here("Figures", 
                       "figure3.png"))

##--------- SERIAL RATES --------
expanded_rates_all %>% 
  group_by(race) %>% 
  summarize(mean_serial = mean(serial_race, na.rm = T),
            sd_serial = sd(serial_race, na.rm = T))

##--------- FIGURE 4 ----------
expanded_rates_all %>% 
  select(cofips,
         race,
         serial_race) %>% 
  rename(value = serial_race) %>% 
  drop_na() %>% 
  mutate(race_rev = factor(ifelse(race == "asian",
                                  1,
                                  ifelse(race == "black",
                                         2,
                                         ifelse(race == "latino",
                                                3,
                                                4))),
                           labels = c("Asian", "Black", "Latinx", "White"))) %>% 
  ggplot(aes(x = race_rev,
             y = value)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 0.4)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(filename = here("Figures",
                       "figure4.png"))
  
#------- TABLE A1 ---------
# prepare denominators file
all_county_denominators %>% 
  mutate(moe = sqrt(variance)*1.645) %>% 
  group_by(cofips, race) %>% 
  summarize(renters = sum(renters),
            renters_moe = sum_moes(moe)) %>% 
  group_by(cofips) %>% 
  mutate(total_renters = sum(renters),
         total_moe = sum_moes(renters_moe)) %>% 
  ungroup() %>% 
  mutate(renters_prop = renters/total_renters,
         renters_prop_moe = prop_moe(renters, total_renters, renters_moe, total_moe),
         renters_prop_se = renters_prop_moe/1.645) %>%
  select(cofips, race, total_renters, renters_prop, renters_prop_se) -> denoms_b_race

results_all %>% 
  filter(metric == "filing") %>% 
  pivot_wider(names_from = parameter,
              values_from = value) %>% 
  group_by(cofips, race) %>% 
  summarize(point = sum(point),
            variance = sum(variance)) %>% 
  group_by(cofips) %>% 
  mutate(denom = sum(point)) %>%
  mutate(prop.p = point/denom,
         prop.var = variance/(denom^2)) %>% 
  filter(race %in% c("black", "white", "latino")) %>%
  mutate(prop.se = sqrt(prop.var)*100,
         prop.p = prop.p*100) %>% 
  select(cofips, denom, race, prop.p, prop.se) %>% 
  rename(total_filings = denom) %>% 
  left_join(denoms_b_race) %>% 
  # arrange(desc(total_renters)) %>% 
  mutate(renters_prop = renters_prop*100,
         renters_prop_se = renters_prop_se*100) %>%
  pivot_wider(names_from = race,
              values_from = c(renters_prop, renters_prop_se, prop.p, prop.se)) %>%
  mutate(black_renters = renters_prop_black,
         black_renters_se = renters_prop_se_black,
         black_filings = prop.p_black,
         black_filings_se = prop.se_black,
         latinx_renters = renters_prop_latino,
         latinx_renters_se = renters_prop_se_latino,
         latinx_filings = prop.p_latino,
         latinx_filings_se = prop.se_latino,
         white_renters = renters_prop_white,
         white_renters_se = renters_prop_se_white,
         white_filings = prop.p_white,
         white_filings_se = prop.se_white) %>% 
  select(cofips, total_renters, total_filings,
         black_renters:white_filings_se) -> table_a1_df
  
View(table_a1_df)

fwrite(table_a1_df,
       file = here("table_A1.csv"))

