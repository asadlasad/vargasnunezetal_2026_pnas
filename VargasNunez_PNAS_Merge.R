##### Creating a panel dataset for analyses
# Step 1 - Get counties in month-year 
# Step 2 - Add all the covariates
# Step 3 - Calculate, then add all the detainer counts
# Step 4 - Calculate, then add all the transfer counts
# Step 5 - Calculate, then add all the departed counts

###
rm(list = ls())
set.seed(123)

library(haven) # version 2.5.3
library(tidyverse) # version 2.0.0
library(readxl) # version 1.4.3
library(janitor) # version 2.2.0
library(gridExtra) # version 2.3
library(fect) # version 2.0.3
library(tidycensus) # version 1.4.4


##############################
#### States of Interest ######
##############################

# States of interest - not including AK or HI
states_of_interest <-
  state.abb %>%
  data.frame(state = .) %>%
  filter(!(state %in% c("AK", "HI"))) %>%
  bind_rows(data.frame(state = c("DC"))) %>%
  arrange(state) %>%
  pull(state)

##############################
##### Load County Info #######
##############################

# Load county names and fips
county_fips <- read_xlsx("county_fips_updated.xlsx")

# Modify labels and remove territories abroad
county_fips %>%
  filter(!state %in% c("AS", "GU", "MP",
                       "PR", "UM", "VI")) %>%
  mutate(state_fips = stringr::str_pad(state_fips, 2, pad = "0"),
         county_fips = stringr::str_pad(county_fips, 3, pad = "0"),
         county_name = gsub(" County","", county_name)) %>%
  rename(county = county_name) -> county_fips

############################################
######## Merge Panel SC Data ###############
############################################

# Load Panel Asad and Baer-Bositis (2022) SC Panel Data
data_sc <- read.csv("secure_communities_panel.csv") %>%
  select(-c(X, state_fips.y, fips, state_fips.x)) %>%
  rename(secure_applied = secure2)

### Add leading zeroes to FIPS
data_sc %>%
  mutate(state_fips = stringr::str_pad(statefips, 2, pad = "0"),
         fips = stringr::str_pad(county, 5, pad = '0') ) %>%
  select(-c(borough_secure,
            city_secure,
            parish_secure,
            date_statewide_secure,
            year_statewide_secure,
            date_secure,
            month_secure,
            day_secure,
            year_secure,
            state_secure,
            date)) %>%
  group_by(fips) %>%
  fill(year_month_sc, 
       .direction = "updown") %>% 
  ungroup() -> data_sc

###################################
####### Add County Names ##########
###################################

# Load census population dataset 2010-2019
census_10_19 <- janitor::clean_names(read_csv("census_pop_county_2010_2019.csv"))

# Clean dataset
census_10_19 %>%
  select(state, county, ctyname) %>%
  mutate(county_name = ctyname %>% 
           str_replace_all("city", "City")) %>%
  rename(state_fips = state,
         county_fips = county) %>%
  mutate(fips = paste0(state_fips, county_fips) ) %>%
  filter(county_fips != "000") -> census_10_19



###################################
#### Add State Signing Dates ######
###################################

# Load Ariel White Data
data_aw <- janitor::clean_names(read_xlsx("Ariel White Dataset Coded.xlsx",
                                          skip = 1))

# Label NAs in Ariel White
data_aw[data_aw=="NA"] <- NA

# Fill-in missing NAs in State
data_aw %>% 
  mutate(abbrev = case_when(state == "Illinois" & is.na(abbrev) ~ "IL",
                            state == "Alabama" & is.na(abbrev) ~ "AL",
                            TRUE ~ abbrev)) -> data_aw

# Fill-in names 
data_aw %>% 
  rename(state = abbrev,
         state_name = state) %>%
  mutate(county = tolower(county)) %>%
  right_join(county_fips %>%
              mutate(county = tolower(county)) ,
            by = c("state",
                   "county") ) -> data_aw

#### Fix naming issues
## Dekalb
data_aw$state_fips[data_aw$county == "dekalb" & data_aw$state == "MO"] <- '29'
data_aw$county_fips[data_aw$county == "dekalb" & data_aw$state == "MO"] <- '63'

## Dona Ana
# replace name
data_aw$county[data_aw$county == "doña ana"] <- "dona ana"

# replace fips
data_aw$state_fips[data_aw$county == "dona ana"] <- "35"
data_aw$county_fips[data_aw$county == "dona ana"] <- "13"

## laporte
data_aw$state_fips[data_aw$county == "laporte" & data_aw$state == "IN"] <- '18'
data_aw$county_fips[data_aw$county == "laporte" & data_aw$state == "IN"] <- '91'

## NYC
data_aw$state_fips[data_aw$county == "new york city" & data_aw$state == "NY"] <- '36'
data_aw$county_fips[data_aw$county == "new york city" & data_aw$state == "NY"] <- '61'

# skagway
data_aw$state_fips[data_aw$county == "skagway-hoonah-angoon census area"] <- '02'
data_aw$county_fips[data_aw$county == "skagway-hoonah-angoon census area"] <- '232'

# check missing fips
data_aw %>%
  filter(is.na(county_fips)) %>%
  print(n = Inf) %>%
  select(county, state_code_fips, county_code_fips)

# Create FIPS indicator for data AW
data_aw %>% 
  select(4:12, state_fips, county_fips,
         -c(state_code_fips, county_code_fips, place_code_fips,
            change, aor, year_enacted_yyyy, month_enacted_mm,
            day_enacted_dd)) %>%
  filter(!is.na(state_fips),
         !is.na(county_fips)) %>%
  mutate(state_fips = str_pad(state_fips, 2, pad = "0"),
         county_fips = str_pad(county_fips, 3, pad = "0")) %>%
  mutate(fips = paste0(state_fips, county_fips)) -> data_aw_merge


### Check repeated FIPS labels
data_aw_merge %>% 
  add_count(fips) %>% 
  filter(n > 1)

# Make indicator signing date
data_aw_merge %>%
  filter(!is.na(year_signed_yyyy),
         !is.na(month_signed_mm),
         !is.na(day_signed_dd)) %>% 
  mutate(date_state_signed = ymd(paste0(year_signed_yyyy, sep = "-", month_signed_mm,
                                        sep = "-", day_signed_dd)) ) %>%
  select(fips, state, county, date_state_signed) %>%
  mutate(fips = str_pad(fips, 5, pad = "0") ) -> data_aw_merge 


### Merge Data SC with Data Signing
data_sc %>%
  select(-county) %>%
  left_join(data_aw_merge, by = "fips") %>%
  group_by(state_name) %>% 
  fill(date_state_signed, .direction = "updown") %>% 
  ungroup() %>%
  group_by(fips) %>%
  ungroup() -> data_aw_merge
  

###################################
## Add ACS Non-citizen Pop ########
###################################

# If loading data, run the following
non_citizen_county_pops <- read_csv("non_citizen_pop.csv") %>%
  select(c(year, fips, noncit_pop, moe)) %>%
  rename(noncit_pop_moe = moe)


# Merge data
data_aw_merge %>%
  left_join(non_citizen_county_pops, by = c("fips", "year")) %>%
  filter(year %in% c(2009:2013) ) -> data_sc_aw



###################################
#### Add 2009 - 2019 population data
###################################

census_10_19 <- janitor::clean_names(read_csv("census_pop_county_2010_2019.csv"))

census_10_19 %>%
  select(4:19) %>%
  rename(state_fips = state,
         county_fips = county) %>%
  mutate(fips = paste0(state_fips, county_fips) ) -> census_10_19

### Need to go from wide to long
census_10_19 %>% 
  select(-estimatesbase2010, -census2010pop) %>%
  pivot_longer(cols = c(-fips, -stname, -ctyname,
                        -state_fips, -county_fips),
               names_to = "year", 
               values_to = "value") %>%
  mutate(year2 = as.numeric(str_sub(year, 12))) -> census_10_19

census_10_19 %>%
  select(fips, year2, value) %>%
  rename(year = year2) -> census_10_19 

#### Add 2009 year
census_00_09 <- janitor::clean_names(read_csv("census_pop_county_2000_2009.csv"))

# 
census_00_09 %>%
  select(4:19) %>%
  rename(state_fips = state,
         county_fips = county) %>%
  mutate(fips = paste0(state_fips, county_fips) ) -> census_00_09

### Need to go from wide to long
census_00_09 %>% 
  select(-estimatesbase2000, -census2000pop) %>%
  pivot_longer(cols = c(-fips, -stname, -ctyname,
                        -state_fips, -county_fips),
               names_to = "year", 
               values_to = "value") %>%
  mutate(year2 = as.numeric(str_sub(year, 12))) -> census_00_09

## Select key variables and year 2009
census_00_09 %>%
  select(fips, year2, value) %>%
  rename(year = year2) %>%
  filter(year == 2009) -> census_00_09 

# Rbind data
census_00_19 <- rbind(census_10_19, census_00_09)

# Expand grid
expand.grid(fips = unique(census_00_19$fips), 
            year = c(2009:2019)) %>%
  left_join(census_00_19, by = c("fips", "year")) -> census_00_19_v2


# Merge the final data together
data_sc_aw %>%
  left_join(census_00_19_v2, by = c("fips", "year")) %>%
  rename(population = value) %>%
  mutate(fips = as.numeric(fips) ) -> data_sc_aw



#########################################
####### Adding Control Variables ########
#########################################
# Step 1 - Load MIT Election Data
# Step 2 - Load Demographic covariates using tidycensus
# Step 3 - Load UCR data
# Step 4 - Add 287(g) Agreements

##### STEP 1 ##### 
#### Load MIT Election Data
pres_vote_10_19 <- janitor::clean_names(read_csv("county_pres_00_20.csv"))


## Select the Republican rate per county
pres_vote_10_19 %>%
  group_by(county_fips, year) %>%
  mutate(vote_prop = round((candidatevotes/totalvotes), digits = 3)) %>%
  filter(party == "REPUBLICAN") %>%
  mutate_at(vars(-state_po), tolower) %>%
  rename(state_name = state_po,
         fips = county_fips) %>% 
  select(year, state_name,  
         fips,
         party, vote_prop) %>%
  mutate(year = as.numeric(year),
         fips = as.character(fips),
         vote_prop = as.numeric(vote_prop)) %>%
  filter(year %in% c(2008, 2012) ) ->  pres_vote_10_19

## Add year labeling
pres_vote_10_19 %>%
  arrange(fips, year) %>%
  group_by(fips) %>%
  tidyr::complete(year = 2008:2013) %>%
  fill(everything(), .direction = "down") -> pres_vote_10_19


## Merge with rest of data
pres_vote_10_19 %>%
  mutate(fips = as.numeric(fips)) %>%
  right_join(data_sc_aw, by = c("fips", "year",
                                "state_name")) -> data_deportation 

####Get County Covariates####
##Load Census API Key
options(tigris_use_cache = TRUE)
api_key <- "your_key_here"
census_api_key(api_key, install = TRUE, overwrite = TRUE)

years <- c(2009:2020)

##Get Years and Variables Ready
years_state <-
  expand.grid(years = c(2009:2020),
              state = c(state.abb, "DC")) %>%
  mutate(state = as.character(state)) %>% 
  arrange(state, years) %>% 
  mutate(year_state = paste(state, years, sep = "-"), 
         state = as.factor(state))

vars <-
  years %>%
  map(~load_variables(year = .x,
                      dataset = "acs5",
                      cache = TRUE)) %>%
  setNames(years) %>%
  bind_rows(.id = "years")

variables_of_interest <- c("B17017_031", "B01003_001", "B17017_002",
                           "B03003_001", "B03003_002", "B03003_003",
                           "C16002_001", "C16002_002", "C16002_003",
                           "C16002_004", "C16002_005", "B99051_001",
                           "B99051_002", "B99051_003", "B99051_004",
                           "B99051_005", "B99051_006", "B99051_007",
                           "B16002_001", "B16002_002", "B16002_003",
                           "B16002_004", "B16002_005", "B05001_006")

variables_of_interest_df <-
  vars %>%
  filter(name %in% variables_of_interest)

variables_of_interest_final <-
  variables_of_interest_df %>%
  dplyr::select(years, name, geography) %>%
  group_by(years, geography) %>%
  dplyr::summarize(variables = list(name)) %>%
  mutate(years = as.numeric(years))

variables_of_interest_df_final <-
  vars %>%
  filter(name %in% variables_of_interest) %>%
  mutate(new_name =
           concept %>%
           paste(label %>%
                   str_replace_all("!!", " ")) %>%
           str_to_lower() %>%
           str_replace_all('["]', "") %>%
           str_replace_all(" ", "_") %>%
           str_replace_all(":", "") %>%
           str_replace_all("[.]", "")) %>%
  rename("year" = "years") %>%
  mutate(new_name = ifelse(concept == "HOUSEHOLD LANGUAGE BY LINGUISTIC ISOLATION", NA, new_name)) %>%
  mutate(geography = ifelse(is.na(geography) == TRUE, "tract", geography)) %>%
  group_by(name) %>%
  fill(new_name, .direction = "updown")

vars1 <-
  variables_of_interest_df_final %>%
  filter(year < 2016) %>%
  pull(name) %>%
  unique()

vars2 <-
  variables_of_interest_df_final %>%
  filter(year > 2016) %>%
  pull(name) %>%
  unique()

##Pull Data Using Tidycensus
county_pops <-
  years_state %>%
  filter(years < 2016) %>%
  pmap(~get_acs(geography = "county",
                variables = vars1,
                geometry = TRUE,
                survey = "acs5",
                year = .x,
                state = .y) %>%
         mutate(year = .x,
                state = .y)) %>%
  bind_rows() %>%
  bind_rows(years_state %>%
              filter(years >= 2016) %>%
              pmap(~get_acs(geography = "county",
                            variables = vars2,
                            geometry = TRUE,
                            survey = "acs5",
                            year = .x,
                            state = .y) %>%
                     mutate(year = .x,
                            state = .y)) %>%
              bind_rows()) %>%
  clean_names() %>%
  separate(col = "name", into = c("county", "state"), sep = ", ") %>%
  left_join(variables_of_interest_df_final %>%
              dplyr::select(year, name, new_name) %>%
              distinct() %>%
              mutate(year = as.numeric(year)),
            by = c("year", "variable" = "name")) %>%
  dplyr::select(-variable) %>%
  pivot_wider(names_from = "new_name",
              names_glue = "{new_name}_{.value}",
              values_from = c("estimate", "moe")) %>%
  arrange(year) %>%
  rename("hispanic_or_latino_pop_estimate" = "hispanic_or_latino_origin_estimate_total_hispanic_or_latino_estimate",
         "hispanic_or_latino_pop_moe" = "hispanic_or_latino_origin_estimate_total_hispanic_or_latino_moe",
         "not_hispanic_or_latino_pop_estimate" = "hispanic_or_latino_origin_estimate_total_not_hispanic_or_latino_estimate",
         "not_hispanic_or_latino_pop_moe" = "hispanic_or_latino_origin_estimate_total_not_hispanic_or_latino_moe",
         "total_pop_estimate" = "total_population_estimate_total_estimate",
         "total_pop_moe" = "total_population_estimate_total_moe",
         "total_households_estimate" = "household_language_by_households_in_which_no_one_14_and_over_speaks_english_only_or_speaks_a_language_other_than_english_at_home_and_speaks_english_very_well_estimate_total_estimate",
         "total_households_moe" = "household_language_by_households_in_which_no_one_14_and_over_speaks_english_only_or_speaks_a_language_other_than_english_at_home_and_speaks_english_very_well_estimate_total_moe",
         "english_only_households_estimate" = "household_language_by_households_in_which_no_one_14_and_over_speaks_english_only_or_speaks_a_language_other_than_english_at_home_and_speaks_english_very_well_estimate_total_english_only_estimate",
         "english_only_households_moe" = "household_language_by_households_in_which_no_one_14_and_over_speaks_english_only_or_speaks_a_language_other_than_english_at_home_and_speaks_english_very_well_estimate_total_english_only_moe",
         "spanish_households_estimate" = "household_language_by_households_in_which_no_one_14_and_over_speaks_english_only_or_speaks_a_language_other_than_english_at_home_and_speaks_english_very_well_estimate_total_spanish_estimate",
         "spanish_households_moe" = "household_language_by_households_in_which_no_one_14_and_over_speaks_english_only_or_speaks_a_language_other_than_english_at_home_and_speaks_english_very_well_estimate_total_spanish_moe",
         "spanish_households_limited_english_speaking_estimate" = "household_language_by_households_in_which_no_one_14_and_over_speaks_english_only_or_speaks_a_language_other_than_english_at_home_and_speaks_english_very_well_estimate_total_spanish_no_one_14_and_over_speaks_english_only_or_speaks_english_very_well_estimate",
         "spanish_households_limited_english_speaking_moe" = "household_language_by_households_in_which_no_one_14_and_over_speaks_english_only_or_speaks_a_language_other_than_english_at_home_and_speaks_english_very_well_estimate_total_spanish_no_one_14_and_over_speaks_english_only_or_speaks_english_very_well_moe",
         "spanish_households_non_limited_english_speaking_estimate" = "household_language_by_households_in_which_no_one_14_and_over_speaks_english_only_or_speaks_a_language_other_than_english_at_home_and_speaks_english_very_well_estimate_total_spanish_at_least_one_person_14_and_over_speaks_english_only_or_speaks_english_very_well_estimate",
         "spanish_households_non_limited_english_speaking_moe" = "household_language_by_households_in_which_no_one_14_and_over_speaks_english_only_or_speaks_a_language_other_than_english_at_home_and_speaks_english_very_well_estimate_total_spanish_at_least_one_person_14_and_over_speaks_english_only_or_speaks_english_very_well_moe",
         "households_below_poverty_line_estimate" = "poverty_status_in_the_past_12_months_by_household_type_by_age_of_householder_estimate_total_income_in_the_past_12_months_below_poverty_level_estimate",
         "households_below_poverty_line_moe" = "poverty_status_in_the_past_12_months_by_household_type_by_age_of_householder_estimate_total_income_in_the_past_12_months_below_poverty_level_moe",
         "households_at_or_above_poverty_line_estimate" = "poverty_status_in_the_past_12_months_by_household_type_by_age_of_householder_estimate_total_income_in_the_past_12_months_at_or_above_poverty_level_estimate",
         "households_at_or_above_poverty_line_moe" = "poverty_status_in_the_past_12_months_by_household_type_by_age_of_householder_estimate_total_income_in_the_past_12_months_at_or_above_poverty_level_moe",
         "native_pop_imputed_estimate" = "imputation_of_citizenship_status_estimate_total_native_estimate",
         "native_pop_imputed_moe" = "imputation_of_citizenship_status_estimate_total_native_moe",
         "foreign_born_pop_imputed_estimate" = "imputation_of_citizenship_status_estimate_total_foreign_born_estimate",
         "foreign_born_pop_imputed_moe" = "imputation_of_citizenship_status_estimate_total_foreign_born_moe",
         "total_households_estimate1" = "household_language_by_household_limited_english_speaking_status_estimate_total_estimate",
         "total_households_moe1" = "household_language_by_household_limited_english_speaking_status_estimate_total_moe",
         "english_only_households_estimate1" = "household_language_by_household_limited_english_speaking_status_estimate_total_english_only_estimate",
         "english_only_households_moe1" = "household_language_by_household_limited_english_speaking_status_estimate_total_english_only_moe",
         "spanish_households_estimate1" = "household_language_by_household_limited_english_speaking_status_estimate_total_spanish_estimate",
         "spanish_households_moe1" = "household_language_by_household_limited_english_speaking_status_estimate_total_spanish_moe",
         "spanish_households_limited_english_speaking_estimate1" = "household_language_by_household_limited_english_speaking_status_estimate_total_spanish_limited_english_speaking_household_estimate",
         "spanish_households_limited_english_speaking_moe1" = "household_language_by_household_limited_english_speaking_status_estimate_total_spanish_limited_english_speaking_household_moe",
         "spanish_households_non_limited_english_speaking_estimate1" = "household_language_by_household_limited_english_speaking_status_estimate_total_spanish_not_a_limited_english_speaking_household_estimate",
         "spanish_households_non_limited_english_speaking_moe1" = "household_language_by_household_limited_english_speaking_status_estimate_total_spanish_not_a_limited_english_speaking_household_moe",
         "native_pop_imputed_estimate1" = "allocation_of_citizenship_status_estimate_total_native_estimate",
         "native_pop_imputed_moe1" = "allocation_of_citizenship_status_estimate_total_native_moe",
         "foreign_born_pop_imputed_estimate1" = "allocation_of_citizenship_status_estimate_total_foreign_born_estimate",
         "foreign_born_pop_imputed_moe1" = "allocation_of_citizenship_status_estimate_total_foreign_born_moe",
         "noncitizen_pop_estimate" = "nativity_and_citizenship_status_in_the_united_states_estimate_total_not_a_us_citizen_estimate",
         "noncitizen_pop_moe" = "nativity_and_citizenship_status_in_the_united_states_estimate_total_not_a_us_citizen_moe",
         "noncitizen_pop_estimate1" = "citizenship_status_in_the_united_states_estimate_total_not_a_us_citizen_estimate",
         "noncitizen_pop_moe1" = "citizenship_status_in_the_united_states_estimate_total_not_a_us_citizen_moe") %>%
  dplyr::select(-any_of(c(paste0(variables_of_interest_df_final$new_name, "_estimate"), paste0(variables_of_interest_df_final$new_name, "_moe")))) %>%
  pivot_longer(cols = c(ends_with("estimate"), ends_with("estimate1"),
                        ends_with("moe"), ends_with("moe1")),
               names_to = "variable",
               values_to = "estimate_or_moe") %>%
  mutate(variable =
           variable %>%
           str_replace_all("1", "")) %>%
  filter(is.na(estimate_or_moe) == FALSE) %>%
  pivot_wider(names_from = "variable",
              values_from = "estimate_or_moe") %>% 
  mutate(hispanic_or_latino_population_percentage_estimate = hispanic_or_latino_pop_estimate/total_pop_estimate,
         noncitizen_pop_percentage_estimate = noncitizen_pop_estimate/total_pop_estimate,
         households_below_poverty_line_percentage_estimate = households_below_poverty_line_estimate/total_households_estimate,
         foreign_born_pop_percentage_estimate = foreign_born_pop_imputed_estimate/total_pop_estimate,
         spanish_households_percentage_estimate = spanish_households_estimate/total_households_estimate,
         spanish_households_limited_english_speaking_percentage_estimate = spanish_households_limited_english_speaking_estimate/total_households_estimate)

## Create this as a stand-alone dataset in case tidycensus fails
## or to save time next time you want to add the data
write.csv(county_pops, 'county_pops.csv')


### Merge with dataset
county_pops <- county_pops %>%
  sf::st_drop_geometry( ) %>% 
  rename(fips = geoid, 
         state_name = state) %>%
  mutate(fips = as.numeric(fips)) %>%
  select(fips, year, total_pop_estimate, noncitizen_pop_estimate,
         hispanic_or_latino_population_percentage_estimate,
         hispanic_or_latino_pop_estimate, households_below_poverty_line_percentage_estimate) 


# Merge
data_deportation %>%
  left_join(county_pops, 
            by = c("fips", "year")) -> data_deportation


##### STEP 3 ##### 
#### Add Crime Data from UCR ####

# Read in data from the UCR 
# Taken from ICPSR - 

ucr_2008 <- rio::import("offenses_known_monthly_2008.dta")

ucr_2009 <- rio::import("offenses_known_monthly_2009.dta")

ucr_2010 <- rio::import("offenses_known_monthly_2010.dta")

ucr_2011 <- rio::import("offenses_known_monthly_2011.dta")

ucr_2012 <- rio::import("offenses_known_monthly_2012.dta")

ucr_2013 <- rio::import("offenses_known_monthly_2013.dta")

## Bind the years together

ucr_08_13 <- rbind(ucr_2008, ucr_2009, ucr_2010,
                   ucr_2011, ucr_2012, ucr_2013)

## Keep only the variables you need
ucr_08_13 <- ucr_08_13 %>%
  mutate_all(~ ifelse(. == "", NA, .)) %>%
  filter(!is.na(fips_state_code),
         !is.na(fips_county_code) ) %>%
  mutate(fips_state_county_code = paste0(fips_state_code, fips_county_code)) %>%
  filter(!state %in% states_of_interest, # Removed alaska and hawaii 
         year %in% c(2009:2013),
         !is.na(fips_state_county_code)) %>%
  select(agency_name, state, state_abb, year, month, date, 
         fips_state_county_code, population,
         actual_all_crimes,
         actual_index_total, 
         tot_clr_all_crimes,
         tot_clr_assault_total, tot_clr_rape_total,
         tot_clr_robbery_total, tot_clr_theft_total,
         tot_clr_index_total, tot_clr_index_violent, tot_clr_index_property) 

#### Crime Reporting Problems
### Observe Problem in NYC
ucr_08_13 %>%
  filter(fips_state_county_code == "36061") %>%
  group_by(date) %>%
  summarize(crimes_tot = sum(actual_all_crimes),
            tot_index = sum(actual_index_total, na.rm = T),
            pop_tot = sum(population)) %>%
  select(date, crimes_tot, tot_index, pop_tot) -> ucr_nyc_crime

##### Fix crime counts 
### There are two issues
## First issue - The crime from 2009-2012 is missing every 3 months
## ex) Month 1 may be 100, Month 2 may be 90, Month 3 may be 5000
## Need to take the sum of all 3 months and take the avg to apply to
## those 3 months


## Take the avg every 3 months pre-2013
# Subset to create its own data pre-2013
ucr_nyc_crime_before_2013 <- ucr_nyc_crime %>%
  filter(date < as.Date("2013-01-01"))

# Format date and create quarter indicators
ucr_nyc_crime_before_2013$date <- as.Date(ucr_nyc_crime_before_2013$date, "%Y-%m-%d")
ucr_nyc_crime_before_2013$time <- zoo::as.yearqtr(ucr_nyc_crime_before_2013$date, "%Y-%m-%d")

ucr_nyc_crime_before_2013 <- ucr_nyc_crime_before_2013 %>%
  group_by(time) %>%
  mutate(avg_crimes = round(sum(crimes_tot/3), 0),
         avg_index = round(sum(tot_index/3), 0)) %>%
  ungroup() %>%
  select(-c(crimes_tot, tot_index, time) ) %>%
  rename(crimes_tot = avg_crimes,
         tot_index = avg_index) 



## Subset the after 2013 dates
ucr_nyc_crime_after_2013 <- ucr_nyc_crime %>%
  filter(date >= as.Date("2013-01-01")) %>%
  mutate(date = as.Date(date, "%Y-%m-%d"))


# Merge the crime before and after
ucr_nyc_crime <- rbind(ucr_nyc_crime_before_2013, ucr_nyc_crime_after_2013)

# Get population
ucr_nyc_pop <- ucr_nyc_crime %>%
  select(date, pop_tot)

### Second Issue - All crime is assigned to Manthattan
### Fix by distributing crime across all 5 boroughs

# Create proportions by population
ucr_nyc_crime <- ucr_nyc_crime %>%
  mutate(crimes_tot_36005 = round((crimes_tot*.17), 0), # Bronx has 17% of pop, gets 17% of crimes
         crimes_tot_36047 = round((crimes_tot*.31), 0), # Kings has 31% of pop, gets 31% of crimes
         crimes_tot_36081 = round((crimes_tot*.27), 0), # Queens has 27% of pop, gets 27% of crimes
         crimes_tot_36085 = round((crimes_tot*.06), 0), # Staten Island has 6% of pop, gets 6% of crimes
         crimes_tot_36061 = round((crimes_tot*.19), 0)) %>% # Manhattan has 19% of pop, gets 19% of crimes 
  pivot_longer(
    cols = starts_with("crimes_tot_"),
    names_to = "fips_state_county_code",
    values_to = "value"
  ) %>%
  mutate(fips_state_county_code = as.numeric(sub("crimes_tot_", "", fips_state_county_code))) %>%
  select(date, fips_state_county_code, value)


### Get total counts 
ucr_08_13 %>%
  group_by(fips_state_county_code, date) %>%
  mutate(tot_crimes = sum(actual_all_crimes, na.rm = T), 
         tot_index = sum(actual_index_total, na.rm = T),
         pop_tot = sum(population, na.rm = T)) %>%
  slice(1) -> ucr_08_13

### Problem is NOT limited to NYC - happens in other states 
## Fill-in the data where drops come every 12 months or every quarter
ucr_08_13 <- ucr_08_13 %>%
  mutate(date = as.Date(date, "%Y-%m-%d"),
         fips_state_county_code = as.numeric(fips_state_county_code)) %>%
  group_by(fips_state_county_code, date) %>%
  ungroup() %>% 
  mutate(year = year(date), 
         month = month(date)) %>% 
  group_by(fips_state_county_code, year) %>% 
  mutate(perc = actual_all_crimes/sum(actual_all_crimes, na.rm = TRUE), 
         end_of_year_reporting_flag = ifelse(perc == 1 & month == 12, 1, 0),
         end_of_year_reporting_flag = max(end_of_year_reporting_flag, na.rm = TRUE),
         actual_all_crimes_new = ifelse(end_of_year_reporting_flag == 1, sum(actual_all_crimes, na.rm = TRUE)/12, actual_all_crimes)) %>% 
  ungroup() %>% 
  mutate(quarter = ceiling(month/3)) %>% 
  group_by(fips_state_county_code, year, quarter) %>% 
  mutate(month_num_quarter = 1:n(),
         perc_quarter = actual_all_crimes/sum(actual_all_crimes, na.rm = TRUE), 
         end_of_quarter_reporting_flag = ifelse(perc_quarter >= 0.99 & month_num_quarter == 3, 1, 0),
         end_of_quarter_reporting_flag = max(end_of_quarter_reporting_flag, na.rm = TRUE), 
         actual_all_crimes_new2 = 
           case_when(end_of_quarter_reporting_flag == 1 & end_of_year_reporting_flag == 1 ~ actual_all_crimes_new,
                     end_of_quarter_reporting_flag == 0 & end_of_year_reporting_flag == 1 ~ actual_all_crimes_new, 
                     end_of_quarter_reporting_flag == 1 & end_of_year_reporting_flag == 0 ~ sum(actual_all_crimes, na.rm = TRUE)/3,
                     TRUE ~ actual_all_crimes)) %>% 
  ungroup() 


### Clean out wrong info
ucr_08_13$tot_crimes[ucr_08_13$fips_state_county_code == "36061"] <- NA

ucr_08_13$tot_crimes[ucr_08_13$fips_state_county_code == "36005"] <- NA

ucr_08_13$tot_crimes[ucr_08_13$fips_state_county_code == "36047"] <- NA

ucr_08_13$tot_crimes[ucr_08_13$fips_state_county_code == "36081"] <- NA

ucr_08_13$tot_crimes[ucr_08_13$fips_state_county_code == "36085"] <- NA


## join the right info on tot_crimes
ucr_08_13 <- ucr_08_13 %>%
  mutate(date = as.Date(date, "%Y-%m-%d"),
         fips_state_county_code = as.numeric(fips_state_county_code)) %>%
  left_join(ucr_nyc_crime, by = c("date", "fips_state_county_code")) %>%
  mutate(tot_crimes = ifelse(!is.na(value), value, tot_crimes)) %>%
  select(-value)




### Bc of the UCR structure, the boroughs all have a population of 0
## Bronx has about 17% of population
# 2009
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36005" &
                       ucr_08_13$year == 2009] <- round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                                                                               ucr_08_13$year == 2009])*.17), 2)

# 2010
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36005" &
                       ucr_08_13$year == 2010] <- round( ((ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                                                                                  ucr_08_13$year == 2010])*.17), 2)

# 2011
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36005" &
                       ucr_08_13$year == 2011] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                                                                                   ucr_08_13$year == 2011])*.17), 2)

# 2012
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36005" &
                       ucr_08_13$year == 2012] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                                                                                   ucr_08_13$year == 2012])*.17), 2)

# 2013
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36005" &
                       ucr_08_13$year == 2013] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                                                                                   ucr_08_13$year == 2013])*.17), 2)

## Kings county has about 31%; FIPS 36047
# 2009
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36047" &
                       ucr_08_13$year == 2009] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                                                                                   ucr_08_13$year == 2009])*.31), 2)

# 2010
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36047" &
                       ucr_08_13$year == 2010] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                                                                                   ucr_08_13$year == 2010])*.31), 2)

# 2011
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36047" &
                       ucr_08_13$year == 2011] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                                                                                   ucr_08_13$year == 2011])*.31), 2)

# 2012
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36047" &
                       ucr_08_13$year == 2012] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                                                                                   ucr_08_13$year == 2012])*.31), 2)

# 2013
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36047" &
                       ucr_08_13$year == 2013] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" & 
                                                                                   ucr_08_13$year == 2013])*.31), 2)


## Queens has about 27%; FIPS 36081
# 2009
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36081" &
                       ucr_08_13$year == 2009] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                                                                                   ucr_08_13$year == 2009])*.27), 2)

# 2010
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36081" &
                       ucr_08_13$year == 2010] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" & 
                                                                                   ucr_08_13$year == 2010])*.27), 2)

# 2011
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36081" &
                       ucr_08_13$year == 2011] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" & 
                                                                                   ucr_08_13$year == 2011])*.27), 2)

# 2012
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36081" &
                       ucr_08_13$year == 2012] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" & 
                                                                                   ucr_08_13$year == 2012])*.27), 2)

# 2013
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36081" &
                       ucr_08_13$year == 2013] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                                                                                   ucr_08_13$year == 2013])*.27), 2)


## Staten Island has abut 6%; FIPS 36085
# 2009
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36085" &
                       ucr_08_13$year == 2009] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" & 
                                                                                   ucr_08_13$year == 2009])*.06), 2)

# 2010
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36085" &
                       ucr_08_13$year == 2010] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" & 
                                                                                   ucr_08_13$year == 2010])*.06), 2)

# 2011
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36085" &
                       ucr_08_13$year == 2011] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" & 
                                                                                   ucr_08_13$year == 2011])*.06), 2)

# 2012
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36085" &
                       ucr_08_13$year == 2012] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" & 
                                                                                   ucr_08_13$year == 2012])*.06), 2)

# 2013
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36085" &
                       ucr_08_13$year == 2013] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                                                                                   ucr_08_13$year == 2013])*.06), 2)

## Manhattan has about 19%
# 2009
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                       ucr_08_13$year == 2009] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" & 
                                                                                   ucr_08_13$year == 2009])*.19), 2)
# 2010
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                       ucr_08_13$year == 2010] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" & 
                                                                                   ucr_08_13$year == 2010])*.19), 2)

# 2011
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                       ucr_08_13$year == 2011] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" & 
                                                                                   ucr_08_13$year == 2011])*.19), 2)

# 2012
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                       ucr_08_13$year == 2012] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" & 
                                                                                   ucr_08_13$year == 2012])*.19), 2)

# 2013
ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" &
                       ucr_08_13$year == 2013] <-  round(( (ucr_08_13$pop_tot[ucr_08_13$fips_state_county_code == "36061" & 
                                                                                   ucr_08_13$year == 2013])*.19), 2)

## Get Crimes per Population
ucr_08_13 <- ucr_08_13 %>%
  mutate(tot_crimes_pop = (tot_crimes/pop_tot)*1000)



##### Merge #####################

# Clean dataset and make sure date format works
ucr_08_13 %>%
  mutate(date = format(as.Date(date, format="%Y-%m-%d"), "%Y-%m"),
         fips_state_county_code = as.numeric(fips_state_county_code)) %>%
  select(-month) %>%
  rename(fips = fips_state_county_code,
         month_year_running = date,
         state_name = state_abb,
         population_ucr = population) -> data_merge

# Merge with dataset
data_deportation %>%
  left_join(data_merge,
            by = c("fips", "state_name", 
                   "month_year_running",
                   "year")) -> data_merge


##### STEP 4 ##### 
#### Add 287(g) Agreements Label ####

# Load data and filter for our desired years and format
asad_287 <- read_dta('287g.dta') %>%
  filter(year %in% c(2009:2013)) %>%
  select(year, county, agency_287g, city_287g,
         state_287g, date_signed_287g, date_expired_287g,
         active_287g) %>%
  rename(fips = county) %>%
  group_by(fips) %>%
  fill(c(date_signed_287g, date_expired_287g),
       .direction = "updown") %>% 
  ungroup()

# Change dates into month-year
asad_287 <- asad_287 %>%
  mutate(date_signed_287g = format(as.Date(asad_287$date_signed_287g), "%Y-%m"),
         date_expired_287g = format(as.Date(asad_287$date_expired_287g), "%Y-%m"),
         fips = as.numeric(str_pad(fips, 5, pad = "0")) )

# Merge with dataset
data_merge %>%
  left_join(asad_287,
            by = c("fips", "year")) -> data_merge

##### Add indicator for the activation
data_merge %>%
  mutate(date_state_signed = format(as.Date(date_state_signed), "%Y-%m"),
         active_287g = ifelse(
           !is.na(date_signed_287g) & month_year_running >= date_signed_287g &
             (is.na(date_expired_287g) | 
                month_year_running <= date_expired_287g), 1, 0) ) %>%
  filter(year %in% c(2009:2013)) -> data_merge


#############################################
######## Create Indicators ##################
#############################################

data_merge %>%
  mutate(fips = as.numeric(fips),
         month_year_running = ym(month_year_running),
         date_state_signed = ym(date_state_signed)) %>% 
  group_by(fips) %>%
  mutate(date_state_signed2 = floor_date(date_state_signed, unit = "month"),
         months_diff_signed = interval(date_state_signed2, 
                                       month_year_running) %/% months(1) ,
         treatment_sc_signed = case_when(months_diff_signed < 0 ~ 0,
                                         months_diff_signed >= 0 ~ 1)) %>%
  select(-c(date_state_signed2)) %>%
  ungroup() -> data_sc_final_09_19_signingdates



#############################################
########## Add TRAC Counts ##################
#############################################

### Load the TRAC data
data <- read.delim("detainerall.txt")

# Make the prepare date as a 'date' variable 
data$prepare_date2 <- lubridate::dmy(data$prepare_date)

# Get date by month_year 
data$prepare_month_year <- format(as.Date(data$prepare_date2), "%Y-%m")

# Get date by year
data$prepare_year <- format(as.Date(data$prepare_date2), "%Y")

# Switch TRAC state into factor
data$state <- as.factor(data$state)


#############################################
######### Add Detainer Counts ###############
#############################################


### Mutate TRAC variables and select data needed
data %>%
  select(prepare_date,  
         county, city, state, 
         prepare_date2, prepare_month_year, prepare_year) %>%
  mutate(county = tolower(gsub(" County","", county)) ) %>%
  mutate(county = dplyr::recode(county,
                               'de kalb' = "dekalb") ) -> data_detainers

## Merge County FIPS to fill-in missing info in TRAC data
data_detainers <- county_fips %>%
  mutate(county = tolower(county)) %>%
  right_join(data_detainers, by = c('state', 'county')) %>%
  mutate(fips = paste0(state_fips, county_fips) )  %>% # Create 5 code FIPS
  filter(state %in% states_of_interest, # Remove US territories 
         !fips %in% c("12025", "30113", "41006", "51780", "51560")) # Remove FIPS that we do not want 


####################################       
#### Get counts by year ############
####################################       

# Create counts by year
data_detainers %>%
  filter(!county_fips == "NA",
         !state_fips == "NA") %>% 
  group_by(fips, prepare_year) %>%
  summarise(n = n()) %>%
  select(prepare_year, fips, n) -> data_detainers_counts

## Total year counts
data_detainers_counts %>%
  ungroup() %>%
  filter(prepare_year %in% c(2009:2013) ) %>%
  summarise(count = sum(n))
 
##########################################       
#### Get counts by month-year ############
##########################################       

# Get counts by month-year
data_detainers %>%
  filter(!county_fips == "NA",
         !state_fips == "NA") %>% 
  group_by(fips, prepare_month_year) %>%
  summarise(detainers = n()) %>%
  ungroup() %>%
  rename(month_year_running = prepare_month_year) -> data_detainers_counts_my


# Merge data with county names
county_fips %>%
  mutate(fips = paste0(state_fips, county_fips, "")) %>%
  right_join(data_detainers_counts_my, by = 'fips') -> data_detainers_counts_my


#############################################
### Add Transfer and Departed Counts ########
#############################################

# Fips of interest
options(tigris_use_cache = TRUE)
api_key <- "your_key_here"
census_api_key(api_key, install = TRUE, overwrite = TRUE)

#
fips_crosswalk <- 
  tidycensus::fips_codes %>% 
  mutate(county = 
           county %>% 
           str_to_title())


##### Randomizes the row 
data_transfer_departed <- data %>%
  mutate(prepare_date2 = dmy(prepare_date)) %>%
  select(prepare_date, unique_id, source,
         county, city, state,
         init_book_in_date, # Booking Date (ICE Transfer)
         departed_date, # Deported date
         prepare_date2) %>% 
  mutate(ID = row_number(),
         init_book_in_date2 = format(as.Date(init_book_in_date,
                                             format = "%m/%d/%Y"),
                                     format = "%m-%d-%Y"),
         departed_date_clean =
           departed_date %>%
           str_replace_all(" .*", "") %>%
           mdy(),
         departed_date5 =
           case_when(is.na(departed_date) == FALSE & is.na(departed_date_clean) == TRUE ~ as.Date(as.numeric(departed_date), origin = "1900-01-01"),
                     TRUE ~ as.Date(departed_date_clean)) ) %>%
  mutate(across(where(is.character),
                ~.x %>%
                  na_if("")),
         county =
           county %>%
           str_replace_all("  ", " ") %>%
           str_to_title() %>%
           na_if("Unknown County") %>%
           na_if("Unknown") %>%
           na_if("NA") %>%
           str_replace_all("La Porte County", "Laporte County") %>%
           str_replace_all("De Kalb County", "Dekalb County") %>%
           str_replace_all("\\bContra Costa\\b(?!\\s+(County))", "Contra Costa County") %>%
           str_replace_all("\\bYavapai\\b(?!\\s+(County))", "Yavapai County") %>%
           str_replace_all("Macintosh County", "Mcintosh County") %>%
           str_replace_all("\\bBaltimore\\b(?!\\s+(County|City))", "Baltimore County"),
         county = case_when(state == "IL" ~ str_replace_all(county, "La Salle", "Lasalle"),
                            TRUE ~ county),
         state = case_when(unique_id == 704833079 & source == "D" ~ "IN", 
                           TRUE ~ state)
  ) %>%
  select(-c(departed_date,
            init_book_in_date)) %>%
  rename(#county = county2,
    init_book_in_date = init_book_in_date2,
    departed_date = departed_date5) %>%
  mutate(
    prepare_year = year(prepare_date2) ) %>%
  filter(prepare_year %in% c("2009", "2010", "2011", "2012", "2013"),
         state %in% states_of_interest)  %>%
  left_join(fips_crosswalk %>% 
              dplyr::select(state, county, state_code, county_code) %>% 
              distinct() %>% 
              na.omit(), 
            by = c("state", "county")) %>% 
  rename(state_fips = state_code,
         county_fips = county_code) %>%
  mutate(fips = paste0(state_fips, county_fips) ) %>%
  filter(!fips %in% c("12025", "30113", "41006", "51780", "51560", "NANA")) %>% # remove FIPS that no longer exist or are wrongly coded
  filter(is.na(init_book_in_date) == FALSE) %>% 
  arrange(unique_id, source, init_book_in_date) %>%
  group_by(source, unique_id) %>%
  dplyr::slice_sample(n = 1) %>%
  ungroup()


#### Fix dates into date format

# Get transfer date by month_year
data_transfer_departed$transfer_date_month_year <- format(as.Date(data_transfer_departed$init_book_in_date,
                                                                  format = "%m-%d-%Y"),
                                                          '%Y-%m')
# Get transfer date by year
data_transfer_departed$transfer_date_year <- format(as.Date(data_transfer_departed$init_book_in_date,
                                                            format = "%m-%d-%Y"),
                                                    '%Y')



####################################       
#### Get counts by year ############
####### of TRANSFERS ############ 
#################################### 


# Create counts by year
data_transfer_departed %>%
  filter(!fips == "NANA") %>% 
  group_by(fips, transfer_date_year) %>%
  summarise(transfer = n()) %>%
  select(transfer_date_year, fips, transfer) -> data_transfer_counts

data_transfer_counts

##########################################       
#### Get counts by month-year ############
##########################################       

# Get counts by month-year
data_transfer_departed %>%
  filter(!county_fips == "NA",
         !state_fips == "NA") %>% 
  group_by(fips, transfer_date_month_year) %>%
  summarise(transfer = n()) %>%
  ungroup() %>%
  select(transfer_date_month_year, fips, transfer) %>%
  rename(month_year_running = transfer_date_month_year) %>%
  filter(!is.na(month_year_running) ) -> data_transfer_counts_my


data_transfer_counts_my

### Merge
data_detainers_transfers_counts_my <- data_detainers_counts_my %>%
  full_join(data_transfer_counts_my, 
            by = c('fips', 'month_year_running')) %>%
  group_by(fips) %>%
  fill(c(state, state_fips, county_fips, county),
       .direction = "updown") %>%
  ungroup()

####################################       
#### Get counts by year ############
####### of DEPORTATIONS ############ 
#################################### 

data_departed <- 
  data %>%
  mutate(prepare_date2 = dmy(prepare_date)) %>%
  select(prepare_date, unique_id, source,
         county, city, state,
         init_book_in_date, # Booking Date (ICE Transfer)
         departed_date, # Deported date
         prepare_date2) %>% 
  mutate(ID = row_number(),
         init_book_in_date2 = format(as.Date(init_book_in_date,
                                             format = "%m/%d/%Y"),
                                     format = "%m-%d-%Y"),
         departed_date_clean =
           departed_date %>%
           str_replace_all(" .*", "") %>%
           mdy(),
         departed_date5 =
           case_when(is.na(departed_date) == FALSE & is.na(departed_date_clean) == TRUE ~ as.Date(as.numeric(departed_date), origin = "1900-01-01"),
                     TRUE ~ as.Date(departed_date_clean)) ) %>%
  mutate(across(where(is.character),
                ~.x %>%
                  na_if("")),
         county =
           county %>%
           str_replace_all("  ", " ") %>%
           str_to_title() %>%
           na_if("Unknown County") %>%
           na_if("Unknown") %>%
           na_if("NA") %>%
           str_replace_all("La Porte County", "Laporte County") %>%
           str_replace_all("De Kalb County", "Dekalb County") %>%
           str_replace_all("\\bContra Costa\\b(?!\\s+(County))", "Contra Costa County") %>%
           str_replace_all("\\bYavapai\\b(?!\\s+(County))", "Yavapai County") %>%
           str_replace_all("Macintosh County", "Mcintosh County") %>%
           str_replace_all("\\bBaltimore\\b(?!\\s+(County|City))", "Baltimore County"),
         county = case_when(state == "IL" ~ str_replace_all(county, "La Salle", "Lasalle"),
                            TRUE ~ county),
         state = case_when(unique_id == 704833079 & source == "D" ~ "IN", 
                           TRUE ~ state)
  ) %>%
  select(-c(departed_date,
            init_book_in_date)) %>%
  rename(#county = county2,
    init_book_in_date = init_book_in_date2,
    departed_date = departed_date5) %>%
  mutate(
    prepare_year = year(prepare_date2) ) %>%
  filter(prepare_year %in% c("2009", "2010", "2011", "2012", "2013"),
         state %in% states_of_interest)  %>%
  left_join(fips_crosswalk %>% 
              dplyr::select(state, county, state_code, county_code) %>% 
              distinct() %>% 
              na.omit(), 
            by = c("state", "county")) %>% 
  rename(state_fips = state_code,
         county_fips = county_code) %>%
  mutate(fips = paste0(state_fips, county_fips) ) %>%
  filter(!fips %in% c("12025", "30113", "41006", "51780", "51560", "NANA")) %>%
  filter(is.na(departed_date) == FALSE) %>% 
  arrange(unique_id, source, init_book_in_date) %>%
  group_by(source, unique_id) %>%
  dplyr::slice_sample(n = 1) %>%
  ungroup() 


# Get departed date by month_year 
data_departed$departed_date_month_year <- format(as.Date(data_departed$departed_date,
                                                                  format = "%Y-%m"),  
                                                          "%Y-%m")


# Get departed date by year
data_departed$departed_date_year <- format(as.Date(data_departed$departed_date,
                                                            format = "%m-%d-%Y"), "%Y")



# Create counts by year
data_departed %>%
  filter(!fips == "NANA") %>% 
  group_by(fips, departed_date_year) %>%
  summarise(departed = n()) %>%
  select(departed_date_year, fips, departed) -> data_departed_counts

data_departed_counts

##########################################       
#### Get counts by month-year ############
##########################################       

# Get counts by month-year
data_departed %>%
  filter(!county_fips == "NA",
         !state_fips == "NA") %>% 
  group_by(fips, departed_date_month_year) %>%
  summarise(departed = n()) %>%
  ungroup() %>%
  select(departed_date_month_year, fips, departed) %>%
  rename(month_year_running = departed_date_month_year) %>%
  filter(!is.na(month_year_running) ) -> data_departed_counts_my


data_departed_counts_my


## Mutate
data_detainers_transfers_departed_counts_my <- full_join(data_detainers_transfers_counts_my,
                                                         data_departed_counts_my,
                                                         by = c("fips",
                                                                'month_year_running') ) %>%
  mutate(fips = as.numeric(fips),
         departed = if_else(is.na(departed), 0, departed),
         transfer = if_else(is.na(transfer), 0, transfer),
         detainers = if_else(is.na(detainers), 0, detainers)) %>%
  group_by(fips) %>% 
  fill(c(state, state_fips, county_fips, county),
       .direction = "updown") %>%
  ungroup()

###################################################
############# Put it all together #################
###################################################

data_merged_final <- 
  data_sc_final_09_19_signingdates %>% 
  mutate(month_running = month(month_year_running),
         year_running = year(month_year_running)) %>% 
  left_join(data_detainers_transfers_departed_counts_my %>%
              mutate(month_year_running_full_date = as.Date(paste0(month_year_running, "-01")),
                     month_running = month(month_year_running_full_date),
                     year_running = year(month_year_running_full_date)) %>% 
              dplyr::select(c(fips, month_running, year_running, state_fips,
                              detainers, transfer, departed)),
            by = c("fips", 'month_running', 
                   "year_running", "state_fips")) %>% 
  mutate(across(c("detainers", "transfer", "departed"), ~replace_na(.x, 0))) %>%
  filter(!fips %in% c("51780", "51560", "41006", # remove FIPS that no longer exist or are wrongly coded
                      "12025", "30113"),
         state_name %in% states_of_interest)

write.csv(data_merged_final, 'data_merged_final.csv')





