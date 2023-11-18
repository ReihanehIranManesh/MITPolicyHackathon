# Demographics of Eviction

# Renee created 11.03.2020
# last edited: 11.09.2020 -- edit prop_moe function

# This script uses IPUMS data and ACS data to calculate
# county-level counts of renters stratified by race and sex and associated variances

###------- SET UP-----------
# load libraries
library(here)
library(data.table)
library(stats)
library(tidyverse)
library(tidycensus)

# load IPUMS dataset
ipums <- read.csv(file = here("Uncertainty Update",
                              "Data",
                              "usa_00005.csv"))

# load 2010 PUMA relationship file
pumas <- read.csv(file = here("Uncertainty Update",
                              "Data",
                              "state.county.puma.csv"))
names(pumas) <- c("STATEFP",
                  "COUNTYFP",
                  "TRACTCE",
                  "PUMA5CE")

# get GEOID.Tract variable for puma-tract relationship file
pumas %>% 
  mutate(STATEFP = str_pad(STATEFP,
                           width = 2,
                           side = "left",
                           pad = "0"),
         COUNTYFP = str_pad(COUNTYFP,
                            width = 3,
                            side = "left",
                            pad = "0"),
         TRACTCE = str_pad(TRACTCE,
                           width = 6,
                           side = "left",
                           pad = "0"),
         PUMA5CE = str_pad(PUMA5CE,
                           width = 5,
                           side = "left",
                           pad = "0")) %>% 
  mutate(GEOID.Tract = paste(STATEFP,
                             COUNTYFP,
                             TRACTCE,
                             sep = ""),
         state.puma = paste(STATEFP,
                            PUMA5CE,
                            sep = "")) %>% 
  select(GEOID.Tract,
         state.puma) -> puma.tract

##---------- DEFINE FUNCTIONS -----------

# function to add MOEs
sum_moes <- function(x){
  return(sqrt(sum(x^2)))
}

# function to calculate MOE of proportions
prop_moe <- function(x, y, a, b){
  p <- x/y
  undersqr <- (a^2 - ((p^2)*(b^2)))
  return(ifelse(undersqr > 0,
                ((1/y)*sqrt(undersqr)),
                ((1/y)*sqrt(a^2 + ((p^2)*(b^2))))
  ) )
}

# function to calculate MOE of products
product_moe <- function(x, y, a, b){
  first <- (x^2)*(b^2)
  second <- (y^2)*(a^2)
  return(sqrt(first + second))
}

##---------- PREPARE IPUMS DATASET-------------
ipums %>% 
  # filter to only renting adults
  filter(OWNERSHP == 2,
         AGE >= 18,
         GQ != 3 | GQ != 4) %>% 
  mutate(STATEFIP = str_pad(STATEFIP,
                            width = 2,
                            side = "left",
                            pad = "0"),
         PUMA = str_pad(PUMA,
                        width = 5,
                        side = "left",
                        pad = "0")) %>% 
  # get unique puma code
  mutate(state.puma = paste(STATEFIP,
                            PUMA,
                            sep = "")) %>% 
  # recode race variable
  mutate(race = case_when(RACE == 1 & HISPAN == 0 ~ "white",
                          RACE == 2 & HISPAN == 0 ~ "black",
                          HISPAN != 0 ~ "hisp",
                          RACE == 4 | RACE == 5 | RACE == 6 & HISPAN == 0 ~ "asian",
                          RACE == 7 | RACE == 8 | RACE == 9 | RACE == 3 & HISPAN == 0 ~ "other")) %>% 
  rename(sex = SEX,
         perwt = PERWT) %>% 
  select(state.puma,
         race,
         sex,
         perwt) -> ipums_renters

sum(ipums_renters$perwt)
# 79,154,898 adult renters in the USA from 2012-2016

# aggregate by puma
ipums_renters %>% 
  mutate(sex = recode(sex,
                      "1" = "male",
                      "2" = "female")) %>% 
  group_by(state.puma,
           race,
           sex) %>% 
  summarize(renters = sum(perwt)) -> ipums_renters
# check that no renters were lost in the aggregating step
sum(ipums_renters$renters)

# get variance around each point estimate
# following a multinomial distribution where var = n*p*(1-p)
ipums_renters %>% 
  group_by(state.puma) %>% 
  mutate(total_renters = sum(renters)) %>% 
  ungroup() %>% 
  mutate(prop = renters/total_renters,
         variance = renters*(1-prop)) %>% 
  select(state.puma, race, sex, renters, variance) -> ipums_renters

##---------- PREPARE ACS DATA ---------

fips_codes %>%
  filter(state_code <= "56") %>%
  distinct(state) %>% 
  pull() -> us_states

map_df(us_states, function(x){
  get_acs(geography = "tract", 
          variables = c(rhh.latin = "B25003I_003",
                        rhh.white = "B25003H_003",
                        rhh.black = "B25003B_003",
                        rhh.asian = "B25003D_003",
                        rhh.asian2 = "B25003E_003",
                        rhh.other = "B25003F_003",
                        rhh.other2 = "B25003G_003",
                        rhh.other3 = "B25003C_003"), 
          year = 2016,
          state = x,
          key = "63153a9c483891cfc1fc7be953ff2c09982c9d1f")
}) -> acs

acs %>% 
  group_by(GEOID, NAME) %>% 
  mutate(race = case_when(str_detect(variable, "black") ~ "black",
                          str_detect(variable, "latin") ~ "hisp",
                          str_detect(variable, "white") ~ "white",
                          str_detect(variable, "asian") ~ "asian",
                          str_detect(variable, "other") ~ "other"
                          )) %>%
  group_by(GEOID, race) %>% 
  summarize(estimate = sum(estimate),
            moe = sum_moes(moe)) %>% 
  rename(GEOID.Tract = GEOID) %>% 
  mutate(state = str_sub(GEOID.Tract, 1, 2),
         county = str_sub(GEOID.Tract, 3, 5),
         tract = str_sub(GEOID.Tract, 6, 11)) %>% 
  select(GEOID.Tract, state:tract, race:moe) -> acs_hh

# combine PUMA relationship file with ACS to get PUMA by tract
acs_hh %>% 
  left_join(puma.tract) -> acs_puma

# check which tracts do not have a PUMA listed in the PUMA-tract relationship file
# add missing PUMAs
new.pumas <- tibble(GEOID.Tract = unique(acs_puma[which(is.na(acs_puma$state.puma)), 1]$GEOID.Tract),
                    state.puma = c(puma.tract[puma.tract$GEOID.Tract == "02270000100", 2],
                                   puma.tract[puma.tract$GEOID.Tract == "04019002701", 2],
                                   puma.tract[puma.tract$GEOID.Tract == "04019002903", 2],
                                   puma.tract[puma.tract$GEOID.Tract == "04019410501", 2],
                                   puma.tract[puma.tract$GEOID.Tract == "04019410502", 2],
                                   puma.tract[puma.tract$GEOID.Tract == "04019410503", 2],
                                   puma.tract[puma.tract$GEOID.Tract == "04019470400", 2],
                                   puma.tract[puma.tract$GEOID.Tract == "04019470500", 2],
                                   puma.tract[puma.tract$GEOID.Tract == "06037930401", 2],
                                   rep("3601500", 9),
                                   rep(NA, 3), # could not find county fips changes
                                   rep("4600200", 3),
                                   "5151095"))

puma.tract %>%
  bind_rows(new.pumas) -> puma.tract

# create cty_puma variable and aggregate tract-level counts up to the 
# county-puma level

acs_puma %>% 
  mutate(cty_puma = paste(county,
                          state.puma,
                          sep = "_")) %>% 
  group_by(cty_puma, # county_puma variable contains state, county, and puma information
           state,
           county,
           state.puma,
           race) %>% 
  summarize(estimate = sum(estimate),
            moe = sum_moes(moe)) -> acs_ctyp

acs_ctyp %>% 
  distinct(cty_puma) %>% 
  nrow() == nrow(acs_ctyp)/5

###------- SPLITTING TO COUNTY-PUMA---------
# scenario 1: one PUMA corresponds to one whole county
# scenario 2: one PUMA consists of part of exactly one county
# scenario 3: one PUMA consists of several whole counties
# scenario 4: one PUMA consists of several partial counties or a mix of different whole counties and partial counties

# separate PUMAs that have tracts from only one county (scenarios 1 and 2)
# i.e. puma only consists of one unique cty_puma
acs_ctyp %>% 
  ungroup() %>% 
  group_by(state.puma) %>% 
  filter(n_distinct(county) == 1) -> pumas_1_2

# separate PUMAS that have tracts from several counties (scenarios 3 and 4)
# i.e. puma consists of more than one unique cty_puma -> tracts from more than one county
acs_ctyp %>% 
  ungroup() %>% 
  group_by(state.puma) %>% 
  filter(n_distinct(county) > 1) -> pumas_3_4

## for pumas_1_2
# check that there is one unique puma and county-puma per grouping
nrow(pumas_1_2)/5 == length(unique(pumas_1_2$state.puma)) 
nrow(pumas_1_2)/5 == length(unique(pumas_1_2$cty_puma))

# counts for every county-puma is simply the counts of that puma
# no need for acs household counts
pumas_1_2 %>% 
  select(1:4) %>%
  distinct() %>% 
  left_join(ipums_renters) -> all12

## for pumas_3_4

# first get, for every county-puma-race group, aggregate estimate and MOE for county_puma
# and puma
# then get the same for the proportion

pumas_3_4 %>% 
  group_by(state.puma, race) %>% 
  mutate(total_estimate = sum(estimate),
         total_moe = sum_moes(moe)) %>%
  ungroup() %>% 
  group_by(state.puma, cty_puma, race, total_estimate, total_moe) %>% 
  summarize(estimate = sum(estimate),
            moe = sum_moes(moe)) %>% 
  ungroup() %>%
  mutate(prop = estimate/total_estimate) %>% 
  mutate(prop_moe = prop_moe(x = estimate, 
                             y = total_estimate, 
                             a = moe, 
                             b = total_moe)) %>% 
  select(state.puma:race, prop, prop_moe) -> pumas_3_4_prop

## convert propotions into cross-classified counts of renters for each county-puma
## using ipums data at the puma level

# add sex variable
# assume that sex proportions within racial categories are the same between cty-pumas in the same puma
# e.g. if cty_puma A has 0.25 of white renting households (from NHGIS), 
# I assume cty_puma A has 0.25 of the white female renters and 0.25 of the white male renters in that PUMA

# repeat race ratios for each sex category
pumas_3_4_prop %>% 
  mutate(sex = "male") -> male
pumas_3_4_prop %>% 
  mutate(sex = "female") %>% 
  rbind(male) %>% 
  select(state.puma:race, sex, prop:prop_moe) %>% 
  arrange(state.puma, cty_puma, race, sex) -> pumas34_long

# props with NaN were from cases with 0 counts for the whole PUMA (0/0 = NaN)
# counts with NaN are driven by this -> recode all NaN to 0, if whole PUMA has zero counts, 
# county-PUMA will also have zero counts

pumas34_long[is.nan(pumas34_long$prop), "prop"] <- 0 
pumas34_long[is.nan(pumas34_long$prop_moe), "prop_moe"] <- 0

## multiply proportions with relevant counts of renters from iPUMS

# add PUMA-level renters + variance to pumas34_long
# calculate proportion of each race x sex group in each county_puma
# and the moe

pumas34_long %>% 
  left_join(ipums_renters) %>%
  mutate(renters_moe = sqrt(variance)*1.645) %>% 
  mutate(estimate = prop*renters,
         moe = product_moe(prop, renters, prop_moe, renters_moe)) %>%
  mutate(state = str_sub(state.puma, 1, 2),
         county = str_sub(cty_puma, 1, 3),
         renters = estimate,
         variance = (moe/1.645)^2) %>%
  select(cty_puma, state, county, state.puma,
         race, sex, renters, variance) %>%
  bind_rows(all12) -> all
  
# check if all cty_pumas are accounted for
nrow(pumas34_long[which(!pumas34_long$cty_puma %in% all$cty_puma), "cty_puma"]) == 0

# note that difference in row numbers for pumas34_long and all is due to
# 0 counts in the ipums dataset for a particular cross-classified group in a puma

###------- AGGREGATING TO COUNTIES ---------
# aggregate renter counts up to the county level
all %>% 
  group_by(state,
           county,
           race,
           sex) %>%
  summarize(renters = sum(renters, na.rm = T), # NA is for categories with 0 counts in county-puma
            variance = sum(variance, na.rm = T)) -> all_county

sum(ipums_renters$renters) == sum(all_county$renters)

# are all 3142 counties accounted for?
all_county %>%
  ungroup() %>% 
  distinct(state,
           county) %>%
  nrow() == 3142

###------- CLEAN AND SAVE -----------

# add county and state names
all_county %>% 
  mutate(cofips = paste0(state,
                         county)) %>% 
  rename(county_code = county,
         state_code = state) %>% 
  left_join(fips_codes %>% 
              select(state, state_code, county_code, county),
            by = c("state_code" = "state_code", 
                   "county_code" = "county_code")) %>% 
  ungroup() %>% 
  mutate(county = str_to_upper(str_remove(county, " County"))) %>% 
  select(state,
         county,
         cofips,
         race:variance) -> all_county_denominators

# really high variances come from the really high MOE for the proportions
# which come from the really high ACS MOEs for tract-level renter hhs by race

# save output
fwrite(all_county_denominators,
       file = here("Uncertainty Update",
                   "Working Files",
                   "county_denominators.csv"))

