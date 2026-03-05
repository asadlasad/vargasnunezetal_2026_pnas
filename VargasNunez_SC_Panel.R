### Take Asad and Baer-Bositis(2025)
### to make a panel dataset for period of interest
### Use securecommunities.dta 

rm(list = ls())
set.seed(123)

library(haven) # version 2.5.3
library(tidyverse) # version 2.0.0
library(readxl) # version 1.4.3
library(janitor) # version 2.2.0
library(gridExtra) # version 2.3


# Load SC Data
data_sc <- read_dta("securecommunities.dta")

### Add leading zeroes
data_sc %>%
  mutate(state_fips = str_pad(statefips, 2, pad = "0")) -> data_sc

### Load dataset with names 
states <- data.frame(
  state_name = c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE',
                 'DC', 'FL', 'GA', 'HI', 'ID', 'IL', 'IN',
                 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA',
                 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV',
                 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH',
                 'OK', 'OR', 'PA', 'PR', 'RI', 'SC', 'SD',
                 'TN', 'TX', 'UT', 'VT', 'VA', 'VI', 'WA',
                 'WV', 'WI', 'WY'),
  fips = c(01, 02, 04, 05, 06, 08, 09, 10, 11,
           12, 13, 15, 16, 17, 18, 19, 20, 21,
           22, 23, 24, 25, 26, 27, 28, 29, 30,
           31, 32, 33, 34, 35, 36, 37, 38, 39,
           40, 41, 42, 72, 44, 45, 46, 47, 48,
           49, 50, 51, 78, 53, 54, 55, 56),
  total_counties = c(67, 30, 15, 75, 58, 64, 8, 3,
                     1, 67, 159, 5, 44, 102, 92, 99,
                     105, 120, 64, 16, 24, 14, 83, 87,
                     82, 115, 56, 93, 17, 10, 21, 33, 62,
                     100, 53, 88, 77, 36, 67, 78, 5, 46, 66,
                     95, 254, 29, 14, 133, 0, 39, 55, 72, 23)) %>%
  mutate(state_fips = ifelse(row_number()<= 7, paste0("0", fips), fips))

# Merge datasets
data_sc %>%
  full_join(states, by = "state_fips") -> data_sc2


#### Make dataframe with all months from 2009 - 2013

# Make months
month <- rep(rep(seq(01, 12, by = 1), 6), 51)
# Make years
year <- rep(rep(2008:2013, each=12), 51)

# Make dates
dates <- data.frame(year, month) 

# Date format
dates$date <- lubridate::my(paste0(month, "-", year))

# Split into month_year
dates$month_year_running <- format(as.Date(dates$date), "%Y-%m")

# State Names
dates$state_name = rep(c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE',
                         'DC', 'FL', 'GA', 'HI', 'ID', 'IL', 'IN',
                         'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA',
                         'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV',
                         'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH',
                         'OK', 'OR', 'PA', 'RI', 'SC', 'SD',
                         'TN', 'TX', 'UT', 'VT', 'VA', 'WA',
                         'WV', 'WI', 'WY'), each = 72)

dates$state_fips = rep(c(01, 02, 04, 05, 06, 08, 09, 10, 11,
                         12, 13, 15, 16, 17, 18, 19, 20, 21,
                         22, 23, 24, 25, 26, 27, 28, 29, 30,
                         31, 32, 33, 34, 35, 36, 37, 38, 39,
                         40, 41, 42, 44, 45, 46, 47, 48,
                         49, 50, 51, 53, 54, 55, 56), each = 72)

#### Merge

data_sc2 %>%
  right_join(dates, by = c("state_name",
                           "year")) -> data_sc2


##############################################
### Make indicator replacing old 'secure'
##############################################

# Indicator = 1 when Secure Communities applied, 0 otherwise
# Convert date 
data_sc2$year_month_sc <- format(as.Date(data_sc2$date_secure), "%Y-%m")

# Indicator
data_sc2 %>%
  mutate(secure2 = case_when(month_year_running >= year_month_sc ~ 1,
                             TRUE ~ 0)) -> data_sc2

# Save panel dataset
write.csv(data_sc2, "secure_communities_panel.csv")

