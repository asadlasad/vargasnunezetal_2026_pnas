rm(list = ls() )
### Final Dataset for Analyses
### This file makes the SI 
# Note - 'enacted' and 'applied' are used interchangeably
# same with 'departed' and 'removal'


library(haven) # version 2.5.3
library(tidyverse) # version 2.0.0
library(readxl) # version 1.4.3
library(janitor) # version 2.2.0
library(gridExtra) # version 2.3
library(fect) # version 2.0.3
library(tidycensus) # version 1.4.4


##

## Load data
data_merged <- rio::import('data_merged_final.csv')

## Prep Data
# Package does not like missing data
data_pnas <- data_merged %>%
  mutate(treatment_sc_signed = as.character(treatment_sc_signed),
         secure_applied = as.character(secure_applied)) %>%
  mutate(across(c(treatment_sc_signed, 
                  secure_applied), ~ na_if(na_if(. , ""), " "))) %>%
  mutate(treatment_sc_signed = case_when(is.na(treatment_sc_signed) ~ "0",
                                         TRUE ~ treatment_sc_signed), ## Create Signing Indicator
    secure_applied = case_when(is.na(secure_applied) ~ "0",
                               TRUE ~ secure_applied), ## Create Enactment Indicator
    date_state_signed = ymd(date_state_signed)) %>% ## Turn into Date
  mutate(month_year_running = format(as.Date(month_year_running, "%Y-%m")), # Create Running Month-Year Indicator
         min_date_state_signed = format(as.Date(date_state_signed, "%Y-%m")), # Get first instance
         date_county_enacted = format(as.Date(paste0(year_month_sc, "-01"))) ) %>%
  rename(treatment_sc = treatment_sc_signed, #### SIGNING DATE
         treatment_sc_applied = secure_applied) %>% #### APPLIED DATE
  mutate(
    detainers_log1 = log(detainers + 1),
    detainers_binary = ifelse(detainers > 0, 1, 0),
    detainers_binary2 = case_when(detainers == 0 ~ 0,
                                  detainers >= 1 ~ 1),
    detainers_log = log(detainers),
    transfer_log1 = log(transfer + 1),
    transfer_binary = ifelse(transfer > 0, 1, 0),
    transfer_log = log(transfer),
    departed_log1 = log(departed + 1),
    departed_binary = ifelse(departed > 0, 1, 0),
    departed_log = log(departed),
    treatment_sc = as.numeric(treatment_sc),
    treatment_sc_applied = as.numeric(treatment_sc_applied)) %>%
  select(state_name, fips, year, month_year_running, 
         treatment_sc, 
         detainers, transfer, departed,
         detainers_log1, detainers_binary, detainers_log, 
         transfer_log1, transfer_binary, transfer_log,
         departed_log1, departed_binary, departed_log,
         noncit_pop, 
         noncitizen_pop_estimate,
         year,
         date_county_enacted,
         date_state_signed,
         total_pop_estimate,
         months_diff_signed,
         treatment_sc, # Signing of SC
         treatment_sc_applied, # Enactment of SC
         population,
         state_fips,
         tot_crimes_pop,
         vote_prop,
         active_287g,
         hispanic_or_latino_population_percentage_estimate,
         hispanic_or_latino_pop_estimate,
         households_below_poverty_line_percentage_estimate) %>%
  mutate(detainers_log1 = as.numeric(detainers_log1),
         detainers_log = as.numeric(detainers_log),
         transfer_log1 = as.numeric(transfer_log1),
         transfer_log = as.numeric(transfer_log),
         departed_log = as.numeric(departed_log),
         departed_log1 = as.numeric(departed_log1),
         fips = as.numeric(fips),
         time = as.numeric(dplyr::recode(month_year_running,
                                         "2009-01-01" = 1, "2009-02-01" = 2, "2009-03-01" = 3,
                                         "2009-04-01" = 4, "2009-05-01" = 5, "2009-06-01" = 6,
                                         "2009-07-01" = 7, "2009-08-01" = 8, "2009-09-01" = 9,
                                         "2009-10-01" = 10, "2009-11-01" = 11, "2009-12-01" = 12,
                                         "2010-01-01" = 13, "2010-02-01" = 14, "2010-03-01" = 15,
                                         "2010-04-01" = 16, "2010-05-01" = 17, "2010-06-01" = 18,
                                         "2010-07-01" = 19, "2010-08-01" = 20, "2010-09-01" = 21,
                                         "2010-10-01" = 22, "2010-11-01" = 23, "2010-12-01" = 24,
                                         "2011-01-01" = 25, "2011-02-01" = 26, "2011-03-01" = 27, 
                                         "2011-04-01" = 28, "2011-05-01" = 29, "2011-06-01" = 30, 
                                         "2011-07-01" = 31, "2011-08-01" = 32, "2011-09-01" = 33, 
                                         '2011-10-01' = 34, '2011-11-01' = 35, "2011-12-01" = 36, 
                                         "2012-01-01" = 37, "2012-02-01" = 38, "2012-03-01" = 39, 
                                         "2012-04-01" = 40, "2012-05-01" = 41, "2012-06-01" = 42, 
                                         "2012-07-01" = 43, "2012-08-01" = 44, "2012-09-01" = 45,
                                         "2012-10-01" = 46, "2012-11-01" = 47, "2012-12-01" = 48,
                                         "2013-01-01" = 49, "2013-02-01" = 50, "2013-03-01" = 51, 
                                         "2013-04-01" = 52, "2013-05-01" = 53, "2013-06-01" = 54,
                                         "2013-07-01" = 55, "2013-08-01" = 56, "2013-09-01" = 57,
                                         "2013-10-01" = 58, "2013-11-01" = 59, "2013-12-01" = 60)) ) %>%
  filter(time %in% c(1:60)) 



#### Data Signing and Enactment Dates
table(data_pnas$date_state_signed,
      data_pnas$state_name)

### Total FIPS
length(unique(data_pnas$fips))

### FIPS by state
data_pnas %>%
  group_by(state_name) %>%
  summarise(fips_count = n_distinct(fips)) %>%
  print(n = Inf)

##########################
### Make figure S1
##########################

s1a <- data_pnas %>%
  filter(month_year_running == "2009-01-01") %>%
  mutate(treatment_sc_applied = as.factor(treatment_sc_applied)) %>%
  usmap::plot_usmap(data = ., values = "treatment_sc_applied",
                    exclude = c("AK","HI")) +
  scale_fill_manual(values = c('0' = "white", '1' = "blue"),
                    labels = c("No", "Yes"),
                    name = "SC") + 
  theme(legend.position = "right") +
  ggtitle("SC Rolled out by 2009-01")
s1a
ggsave('s1a.jpeg')

###
s1b <- data_pnas %>%
  filter(month_year_running == "2011-06-01") %>%
  mutate(treatment_sc_applied = as.factor(treatment_sc_applied)) %>%
  usmap::plot_usmap(data = ., values = "treatment_sc_applied",
                    exclude = c("AK","HI")) +
  scale_fill_manual(values = c('0' = "white", '1' = "blue"),
                    labels = c("No", "Yes"),
                    name = "SC") + 
  theme(legend.position = "right") +
  ggtitle("SC Rolled out by 2011-06")
s1b
ggsave('s1b.jpeg')

##
s1c <- data_pnas %>%
  filter(month_year_running == "2013-12-01") %>%
  mutate(treatment_sc_applied = as.factor(treatment_sc_applied)) %>%
  usmap::plot_usmap(data = ., values = "treatment_sc_applied",
                    exclude = c("AK","HI")) +
  scale_fill_manual(values = c('1' = "blue"),
                    labels = c("Yes"),
                    name = "SC") + 
  theme(legend.position = "right") +
  ggtitle("SC Rolled out by 2013-12")
s1c
ggsave('s1c.jpeg')


ggpubr::ggarrange(s1a,
                  s1b,
                  s1c,
                  nrow = 3)

##########################
#### Treatment Rollout 
####### Status   
##########################

#### Enactment
s2_enactment <- panelView::panelview(detainers_binary ~ treatment_sc_applied, 
                                            data = data_pnas, 
                                            index = c("fips","time"), 
                                            xlab = "Time in Months", 
                                            ylab = "Unit (FIPS)", display.all = T,
                                            gridOff = TRUE, by.timing = TRUE,
                                            background = "white",
                                            main = "Treatment Status: Enactment Date")
s2_enactment
ggsave("s2_enactment.jpeg")

#### Signing
s3_signing <- panelView::panelview(detainers_binary ~ treatment_sc, 
                                          data = data_pnas, 
                                          index = c("fips","time"), 
                                          xlab = "Time in Months", 
                                          ylab = "Unit (FIPS)", display.all = T,
                                          gridOff = TRUE, by.timing = TRUE,
                                          background = "white",
                                          main = "Treatment Status: Signing Date")
s3_signing
ggsave("s3_signing.jpeg")



###################################       
#### Figure S4
###################################  

### Note that we lose the states that did NOT sign at all

## Enactment BEFORE state signing
enacted_before <- data_pnas %>%
  mutate(enacted_before = if_else(date_county_enacted < date_state_signed, 1, 0)) %>%
  filter(enacted_before == 1) 

# total FIPS in dataset
n_distinct(enacted_before$fips)

## Enactment AFTER signing
enacted_after <- data_pnas %>%
  mutate(enacted_before = if_else(date_county_enacted < date_state_signed, 1, 0)) %>%
  filter(enacted_before == 0) 
n_distinct(enacted_after$fips)

# Making Figure S4
s4 <- data_pnas %>%
  mutate(enacted_before = if_else(date_county_enacted < date_state_signed, "Yes", "No")) %>%
  usmap::plot_usmap(data = ., values = "enacted_before",
                    exclude = c("AK","HI")) +
  scale_fill_manual(values = c('Yes' = "white", 'No' = "blue"),
                    labels = c("No", "Yes"),
                    name = "Enacted Before Signing?") + 
  theme(legend.position = "top")
s4
ggsave('s4.jpeg')


######################################
#### Table S2
######################################

# Reminder - only continental states included
data_pnas %>%
  mutate(month_diff = lubridate::time_length(interval(date_county_enacted, date_state_signed) %/% months(1)),
         "months") %>%
  group_by(fips) %>%
  summarise(
    mean_diff_fips  = mean(month_diff, na.rm = TRUE),
  ) %>%
  summarise(
    overall_mean = mean(mean_diff_fips, na.rm = TRUE),
    overall_median = median(mean_diff_fips, na.rm = TRUE),
    overall_sd = sd(mean_diff_fips, na.rm = TRUE)
  ) %>%
  print()

## by states
states_descriptives <- data_pnas %>%
  mutate(month_diff = time_length(interval(date_county_enacted, date_state_signed), "months")) %>%
  group_by(state_name) %>%
  summarise(
    mean_diff_fips  = mean(month_diff, na.rm = TRUE),
    median_months = median(month_diff, na.rm = TRUE),
    sd_months    = sd(month_diff, na.rm = TRUE)
  ) %>%
  filter(!is.na(mean_diff_fips)) %>%
  rename(`state` = state_name, 
         `mean difference (enacted - signing)` = mean_diff_fips,
         `median months (enacted - signing)` = median_months, 
         `SD` = sd_months)

### Create Table S2
print(xtable::xtable(states_descriptives), include.rownames = F)




######################################
#### Data Descriptives
######################################


#### S5 - S7
# Detainers by time
s5 <- data_pnas %>%
  group_by(time) %>%
  summarise(sum_detainers = sum(detainers)) %>%
  ggplot(aes(x = time, 
             y = sum_detainers)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_x_continuous(
    breaks = seq(0, 60, by = 1),
    labels = seq(0, 60, by = 1)) +
  ylab("Total Detainers") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Detainers Counts")
s5
ggsave("s5.jpeg")


## Histogram detainers
s6 <- data_pnas %>% 
  ggplot(aes(x = detainers)) + 
  geom_histogram(bins = max(data_pnas$detainers),
                 position = "identity",
                 color = "red") +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  xlab("Monthly Detainers") +
  ylab("Counts")
s6
ggsave("s6.jpeg")

## Histogram detainers binary
s7 <- data_pnas %>% 
  ggplot(aes(x = detainers_binary)) + 
  geom_histogram(position = "identity") +
  theme_bw() +
  scale_x_continuous(breaks = c(0,1) ) +
  scale_y_continuous(labels = scales::comma) +
  xlab("Detainers Binary") +
  ylab("Detainers Counts")
s7
ggsave("s7.jpeg")


#### S8 - S10

# Transfers by time
s8 <- data_pnas %>%
  group_by(time) %>%
  summarise(sum_transfers = sum(transfer)) %>%
  ggplot(aes(x = time, y = sum_transfers)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_x_continuous(
    breaks = seq(0, 60, by = 1),
    labels = seq(0, 60, by = 1)) +
  ylab("Total Transfer Counts") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Transfer Counts")
s8
ggsave("s8.jpeg")


## Histogram transfers
s9 <- data_pnas %>% 
  ggplot(aes(x = transfer)) + 
  geom_histogram(bins = max(data_pnas$transfer),
                 position = "identity",
                 color = "red") +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  xlab("Monthly Transfers") +
  ylab("Counts")
s9
ggsave("s9.jpeg")

# Histogram Transfers binary
s10 <- data_pnas %>% 
  ggplot(aes(x = transfer_binary)) + 
  geom_histogram(position = "identity") +
  theme_bw() +
  scale_x_continuous(breaks = c(0,1) ) +
  scale_y_continuous(labels = scales::comma) +
  xlab("Transfers Binary") +
  ylab("Transfers Counts")
s10
ggsave("s10.jpeg")



#### S11 - S14
# Removals by time
s11 <- data_pnas %>%
  group_by(time) %>%
  summarise(sum_departed = sum(departed)) %>%
  ggplot(aes(x = time, 
             y = sum_departed)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_x_continuous(
    breaks = seq(0, 60, by = 1),
    labels = seq(0, 60, by = 1)) +
  ylab("Total Removal Counts") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Removals Counts")
s11
ggsave("s11.jpeg")

# Histogram Removals
s12 <- data_pnas %>% 
  ggplot(aes(x = departed)) + 
  geom_histogram(bins = max(data_pnas$departed),
                 position = "identity",
                 color = "red") +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  xlab("Monthly Removals") +
  ylab("Counts")
s12
ggsave("s12.jpeg")

# Histogram Removals binary
s13 <- data_pnas %>% 
  ggplot(aes(x = departed_binary)) + 
  geom_histogram(position = "identity") +
  theme_bw() +
  scale_x_continuous(breaks = c(0,1) ) +
  xlab("Removals Binary") +
  ylab("Removals Counts")
s13
ggsave("s13.jpeg")


### Total Detainers, Transfers, Removals by year
data_pnas %>% 
  group_by(year) %>% 
  summarise(total_detainers = sum(detainers), 
            total_transfers = sum(transfer), 
            total_removals = sum(departed))



###################################       
##### Basic Descriptives ##########   
###################################       

summary_table <- data_pnas %>% 
  summarise(detainers = sum(detainers, na.rm = T),
            transfers = sum(transfer, na.rm = T),
            removals = sum(departed, na.rm = T)) 

# Summary by states
summary_table_state <- data_pnas %>% 
  group_by(state_name) %>%
  summarise(detainers = sum(detainers, na.rm = T),
            transfers = sum(transfer, na.rm = T),
            removals = sum(departed, na.rm = T)) 


### Summary and SD
# Detainers
summary(data_pnas$detainers)
# Detainers - SD
sd(data_pnas$detainers, na.rm = T)


# Transfers
summary(data_pnas$transfer)
# Transfers - SD
sd(data_pnas$transfer, na.rm = T)


# Removals
summary(data_pnas$departed)
# Transfers - SD
sd(data_pnas$departed, na.rm = T)



###################################       
########## Analyses ###############       
######### Unadjusted ##############   
###################################       


###################################       
########### log + 1 ###############       

##### Figure S14
### Enacted
figure_s14_unadj_applied <- fect(detainers_log1 ~ treatment_sc_applied, 
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T,
                           vartype = "bootstrap",
                           seed = 123,
                           nboots = 200)
# Est Avg Effect
print(figure_s14_unadj_applied$att.avg)

# Plot results
plot_figure_s14_unadj_applied <- plot(figure_s14_unadj_applied, 
                                main = "Secure Communities Enactment Date - Unadjusted", 
                                ylab = "ln(Detainers + 1)", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s14_unadj_applied
ggsave('plot_figure_s14_unadj_applied.jpeg')

#### Signed

figure_s14_unadj_signed <- fect(detainers_log1 ~ treatment_sc, 
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T,
                           vartype = "bootstrap",
                           seed = 123,
                           nboots = 200)
# Est Avg Effect
print(figure_s14_unadj_signed$att.avg)

# Plot results
plot_figure_s14_unadj_signed <- plot(figure_s14_unadj_signed, 
                                main = "Secure Communities Signing Date - Unadjusted", 
                                ylab = "ln(Detainers + 1)", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s14_unadj_signed
ggsave('plot_figure_s14_unadj_signed.jpeg')


# Storing the avg att
plot_figure_s14_unadj_signed$att.avg


##### Figure S15

### Enacted
figure_s15_unadj_applied <- fect(transfer_log1 ~ treatment_sc_applied, 
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T,
                           vartype = "bootstrap",
                           seed = 123,
                           nboots = 200)

# Est Avg Effect
print(figure_s15_unadj_applied$att.avg)


# Plot results
plot_figure_s15_unadj_applied <- plot(figure_s15_unadj_applied, 
                                main = "Secure Communities Enactment Date - Unadjusted", 
                                ylab = "ln(Transfers + 1)", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s15_unadj_applied
ggsave('plot_figure_s15_unadj_applied.jpeg')


### Signed
figure_s15_unadj_signed <- fect(transfer_log1 ~ treatment_sc, 
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T,
                           vartype = "bootstrap",
                           seed = 123,
                           nboots = 200)
## Est Avg Effect
print(figure_s15_unadj_signed$att.avg)

# Plot results
plot_figure_s15_unadj_signed <- plot(figure_s15_unadj_signed, 
                                main = "Secure Communities Signing Date - Unadjusted", 
                                ylab = "ln(Transfers + 1)", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s15_unadj_signed
ggsave('plot_figure_s15_unadj_signed.jpeg')




##### Make S16
### Enacted
figure_s16_unadj_applied <- fect(departed_log1 ~ treatment_sc_applied, 
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T,
                           vartype = "bootstrap",
                           seed = 123,
                           nboots = 200)
# Plot results
plot_figure_s16_unadj_applied <- plot(figure_s16_unadj_applied, 
                                main = "Secure Communities Enactment Date - Unadjusted", 
                                ylab = "ln(Removals + 1)", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s16_unadj_applied
ggsave('plot_figure_s16_unadj_applied.jpeg')



### Signed
figure_s16_unadj_signed <- fect(departed_log1 ~ treatment_sc, 
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T,
                           vartype = "bootstrap",
                           seed = 123,
                           nboots = 200)
# Plot results
plot_figure_s16_unadj_signed <- plot(figure_s16_unadj_signed, 
                                main = "Secure Communities Signing Date - Unadjusted", 
                                ylab = "ln(Removals + 1)", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s16_unadj_signed
ggsave('plot_figure_s16_unadj_signed.jpeg')



## Est Avg Effect
print(figure_s16_unadj_signed$att.avg)




###################################       
##### Tests Pre-Trends ############   
###################################       


#### Detainers
figure_s17a_unadj <- fect(detainers_binary ~ treatment_sc, 
                             data = data_pnas, 
                             index = c("fips","time"), 
                             method = "fe", 
                             force = "two-way",
                             parallel = T,
                             se = T,
                             vartype = "bootstrap",
                             seed = 123,
                             nboots = 200)

# Plot Signing Date and Detainers
plot_figure_s17a_unadj <- plot(figure_s17a_unadj, 
                               type = "equiv", ylim = c(-.15,.15),
                               main = "Pre-Trends Test - Secure Communities Signing Date - Unadjusted") +
  ylab("Detainers - Binary") +
  xlab("Months Before Treatment")
plot_figure_s17a_unadj
ggsave('plot_figure_s17a_unadj.jpeg')


### Transfers
figure_s17b_unadj <- fect(transfer_binary ~ treatment_sc, 
                             data = data_pnas, 
                             index = c("fips","time"), 
                             method = "fe", 
                             force = "two-way",
                             parallel = T,
                             se = T,
                             vartype = "bootstrap",
                             seed = 123,
                             nboots = 200)


#
plot_figure_s17b_unadj <- plot(figure_s17b_unadj,
                               type = "equiv", ylim = c(-.15,.15),
                               main = "Pre-Trends Test - Secure Communities Signing Date - Unadjusted") +
  ylab("Transfers - Binary") +
  xlab("Months Before Treatment")
plot_figure_s17b_unadj
ggsave('plot_figure_s17b_unadj.jpeg')


## Removals
figure_s17c_unadj <- fect(departed_binary ~ treatment_sc, 
                             data = data_pnas, 
                             index = c("fips","time"), 
                             method = "fe", 
                             force = "two-way",
                             parallel = T,
                             se = T,
                             vartype = "bootstrap",
                             seed = 123,
                             nboots = 200)


plot_figure_s17c_unadj <- plot(figure_s17c_unadj,
                               type = "equiv", ylim = c(-.15,.15),
                               main = "Pre-Trends Test - Secure Communities Signing Date - Unadjusted") +
  ylab("Removals - Binary") +
  xlab("Months Before Treatment")

plot_figure_s17c_unadj
ggsave('plot_figure_s17c_unadj.jpeg')



###################################       
### Alternative Specifications ####   
###################################       


########### IFEct #################
######### Figure S18 ##############       

### Detainers
figure_s18a_ife_unadj <- fect(detainers_binary ~ treatment_sc, 
                                 data = data_pnas, 
                                 index = c("fips","time"), 
                                 method = "ife", 
                                 force = "two-way",
                                 parallel = T,
                                 se = T,
                                 vartype = "bootstrap",
                                 seed = 123,
                                 nboots = 200)
## Est Avg Effect
print(figure_s18a_ife_unadj$att.avg)

# Plot results
plot_figure_s18a_ife_unadj <- plot(figure_s18a_ife_unadj, 
                                      main = "Secure Communities Signing Date - Unadjusted (IFEct)", 
                                      ylab = "Detainers - Binary", 
                                      xlab = "Months Before and After Treatment",
                                      stats = "F.p",
                                      show.count = T,
                                      cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s18a_ife_unadj
ggsave('plot_figure_s18a_ife_unadj.jpeg')

### Transfers
figure_s18b_ife_unadj <- fect(transfer_binary ~ treatment_sc, 
                                 data = data_pnas, 
                                 index = c("fips","time"), 
                                 method = "ife", 
                                 force = "two-way",
                                 parallel = T,
                                 se = T,
                                 vartype = "bootstrap",
                                 seed = 123,
                                 nboots = 200)
## Est Avg Effect
print(figure_s18b_ife_unadj$est.avg)

# Plot results
plot_figure_s18b_ife_unadj <- plot(figure_s18b_ife_unadj, 
                                      main = "Secure Communities Signing Date - Unadjusted (IFEct)", 
                                      ylab = "Transfer - Binary", 
                                      xlab = "Months Before and After Treatment",
                                      stats = "F.p",
                                      show.count = T,
                                      cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s18b_ife_unadj
ggsave('plot_figure_s18b_ife_unadj.jpeg')

### Removals
figure_s18c_ife_unadj <- fect(departed_binary ~ treatment_sc, 
                                 data = data_pnas, 
                                 index = c("fips","time"), 
                                 method = "ife", 
                                 force = "two-way",
                                 parallel = T,
                                 se = T,
                                 vartype = "bootstrap",
                                 seed = 123,
                                 nboots = 200)
## Est Avg Effect
print(figure_s18c_ife_unadj$est.avg)

# Plot results
plot_figure_s18c_ife_unadj <- plot(figure_s18c_ife_unadj, 
                                      main = "Secure Communities Signing Date - Unadjusted (IFEct)", 
                                      ylab = "Removals - Binary", 
                                      xlab = "Months Before and After Treatment",
                                      stats = "F.p",
                                      show.count = T,
                                      cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s18c_ife_unadj
ggsave('plot_figure_s18c_ife_unadj.jpeg')


##### Matrix Computation ##########
######## Figure S19 ###############       



### Detainers
figure_s19a_mc_unadj <- fect(detainers_binary ~ treatment_sc, 
                                data = data_pnas, 
                                index = c("fips","time"), 
                                method = "mc", 
                                force = "two-way",
                                parallel = T,
                                se = T,
                                vartype = "bootstrap",
                                seed = 123,
                                nboots = 200)
## Est Avg Effect
print(figure_s19a_mc_unadj$est.avg)

# Plot results
plot_figure_s19a_mc_unadj <- plot(figure_s19a_mc_unadj, 
                                     main = "Secure Communities Signing Date - Unadjusted (MC)", 
                                     ylab = "Detainers - Binary", 
                                     xlab = "Months Before and After Treatment",
                                     stats = "F.p",
                                     show.count = T,
                                     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s19a_mc_unadj
ggsave('plot_figure_s19a_mc_unadj.jpeg')

### Transfers
figure_s19b_mc_unadj <- fect(transfer_binary ~ treatment_sc, 
                                data = data_pnas, 
                                index = c("fips","time"), 
                                method = "mc", 
                                force = "two-way",
                                parallel = T,
                                se = T,
                                vartype = "bootstrap",
                                seed = 123,
                                nboots = 200)
## Est Avg Effect
print(figure_s19b_mc_unadj$est.avg)

# Plot results
plot_figure_s19b_mc_unadj <- plot(figure_s19b_mc_unadj, 
                                     main = "Secure Communities Signing Date - Unadjusted (MC)", 
                                     ylab = "Transfer - Binary",
                                     xlab = "Months Before and After Treatment",
                                     stats = "F.p",
                                     show.count = T,
                                     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s19b_mc_unadj
ggsave('plot_figure_s19b_mc_unadj.jpeg')

### Removals
figure_s19c_mc_unadj <- fect(departed_binary ~ treatment_sc, 
                                data = data_pnas, 
                                index = c("fips","time"), 
                                method = "mc", 
                                force = "two-way",
                                parallel = T,
                                se = T,
                                vartype = "bootstrap",
                                seed = 123,
                                nboots = 200)
## Est Avg Effect
print(figure_s19c_mc_unadj$est.avg)

# Plot results
plot_figure_s19c_mc_unadj <- plot(figure_s19c_mc_unadj, 
                                     main = "Secure Communities Signing Date - Unadjusted (MC)", 
                                     ylab = "Removals - Binary", 
                                     xlab = "Months Before and After Treatment",
                                     stats = "F.p",
                                     show.count = T,
                                     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s19c_mc_unadj
ggsave('plot_figure_s19c_mc_unadj.jpeg')




###################################       
##### Table S3 - Unadjusted #######   
################################### 

#### Detainers
table_s3a_unadj_signing <- fect(detainers_binary ~ treatment_sc, 
                          data = data_pnas, 
                          index = c("fips","time"), 
                          method = "fe", 
                          force = "two-way",
                          parallel = T,
                          se = T,
                          vartype = "bootstrap",
                          seed = 123,
                          nboots = 200)

### Transfers
table_s3b_unadj_signing <- fect(transfer_binary ~ treatment_sc, 
                          data = data_pnas, 
                          index = c("fips","time"), 
                          method = "fe", 
                          force = "two-way",
                          parallel = T,
                          se = T,
                          vartype = "bootstrap",
                          seed = 123,
                          nboots = 200)


## Removals
table_s3c_unadj_signing <- fect(departed_binary ~ treatment_sc, 
                          data = data_pnas, 
                          index = c("fips","time"), 
                          method = "fe", 
                          force = "two-way",
                          parallel = T,
                          se = T,
                          vartype = "bootstrap",
                          seed = 123,
                          nboots = 200)

#####
### Enactment
table_s3a_unadj_applied <- fect(detainers_binary ~ treatment_sc_applied, 
                             data = data_pnas, 
                             index = c("fips","time"), 
                             method = "fe", 
                             force = "two-way",
                             parallel = T,
                             se = T,
                             vartype = "bootstrap",
                             seed = 123,
                             nboots = 200)

### Transfers
table_s3b_unadj_applied <- fect(transfer_binary ~ treatment_sc_applied, 
                             data = data_pnas, 
                             index = c("fips","time"), 
                             method = "fe", 
                             force = "two-way",
                             parallel = T,
                             se = T,
                             vartype = "bootstrap",
                             seed = 123,
                             nboots = 200)

### Removal
table_s3c_unadj_applied <- fect(departed_binary ~ treatment_sc_applied, 
                             data = data_pnas, 
                             index = c("fips","time"), 
                             method = "fe", 
                             force = "two-way",
                             parallel = T,
                             se = T,
                             vartype = "bootstrap",
                             seed = 123,
                             nboots = 200)


# Create names
columns_table_s3 <- c("Signing", "Enactment")
rows_table_s3 <- c("Detainers ATT", "Detainers 95% CI Low", "Detainers 95% CI High",
                 "Transfers ATT", "Transfers 95% CI Low", "Transfers 95% CI High", 
                 "Removals ATT", "Removals 95% CI Low", "Removals 95% CI High")

# Table S3
table_s3 <- data.frame(matrix(ncol = length(columns_table_s3), 
                            nrow = length(rows_table_s3)))
colnames(table_s3) <- columns_table_s3
rownames(table_s3) <- rows_table_s3


### Collect Coefficients
# Enactment Detainers
table_s3$Enactment[1] <- round((table_s3a_unadj_applied$est.avg[1]*100), 2)
table_s3$Enactment[2] <- round((table_s3a_unadj_applied$est.avg[3]*100), 2)
table_s3$Enactment[3] <- round((table_s3a_unadj_applied$est.avg[4]*100), 2)

# Overall Transfers
table_s3$Enactment[4] <- round((table_s3b_unadj_applied$est.avg[1]*100), 2)
table_s3$Enactment[5] <- round((table_s3b_unadj_applied$est.avg[3]*100), 2)
table_s3$Enactment[6] <- round((table_s3b_unadj_applied$est.avg[4]*100), 2)

# Overall Removals
table_s3$Enactment[7] <- round((table_s3c_unadj_applied$est.avg[1]*100), 2)
table_s3$Enactment[8] <- round((table_s3c_unadj_applied$est.avg[3]*100), 2)
table_s3$Enactment[9] <- round((table_s3c_unadj_applied$est.avg[4]*100), 2)



# Signing Detainers
table_s3$Signing[1] <- round((table_s3a_unadj_signing$est.avg[1]*100), 2)
table_s3$Signing[2] <- round((table_s3a_unadj_signing$est.avg[3]*100), 2)
table_s3$Signing[3] <- round((table_s3a_unadj_signing$est.avg[4]*100), 2)

# Signing Transfers
table_s3$Signing[4] <- round((table_s3b_unadj_signing$est.avg[1]*100), 2)
table_s3$Signing[5] <- round((table_s3b_unadj_signing$est.avg[3]*100), 2)
table_s3$Signing[6] <- round((table_s3b_unadj_signing$est.avg[4]*100), 2)

# Signing Removals
table_s3$Signing[7] <- round((table_s3c_unadj_signing$est.avg[1]*100), 2)
table_s3$Signing[8] <- round((table_s3c_unadj_signing$est.avg[3]*100), 2)
table_s3$Signing[9] <- round((table_s3c_unadj_signing$est.avg[4]*100), 2)

xtable::xtable(table_s3)

###################################       
###### Table S4 - Adjusted ########   
################################### 

# Detainers
table_s4a_adj_applied <- fect(detainers_binary ~ treatment_sc_applied
                           + total_pop_estimate 
                           + noncitizen_pop_estimate 
                           + hispanic_or_latino_population_percentage_estimate
                           + households_below_poverty_line_percentage_estimate 
                           + vote_prop
                           + tot_crimes_pop
                           + active_287g, 
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T,
                           vartype = "bootstrap",
                           seed = 123,
                           nboots = 200)

### Transfers
table_s4b_adj_applied <- fect(transfer_binary~ treatment_sc_applied
                              + total_pop_estimate 
                              + noncitizen_pop_estimate 
                              + hispanic_or_latino_population_percentage_estimate
                              + households_below_poverty_line_percentage_estimate 
                              + vote_prop
                              + tot_crimes_pop
                              + active_287g, 
                              data = data_pnas, 
                              index = c("fips","time"), 
                              method = "fe", 
                              force = "two-way",
                              parallel = T,
                              se = T,
                              vartype = "bootstrap",
                              seed = 123,
                              nboots = 200)



### Removals
table_s4c_adj_applied <- fect(departed_binary ~ treatment_sc_applied
                              + total_pop_estimate 
                              + noncitizen_pop_estimate 
                              + hispanic_or_latino_population_percentage_estimate
                              + households_below_poverty_line_percentage_estimate 
                              + vote_prop
                              + tot_crimes_pop
                              + active_287g, 
                              data = data_pnas, 
                              index = c("fips","time"), 
                              method = "fe", 
                              force = "two-way",
                              parallel = T,
                              se = T,
                              vartype = "bootstrap",
                              seed = 123,
                              nboots = 200)



### Signed
# Detainers
table_s4a_adj_signing <- fect(detainers_binary ~ treatment_sc
                           + total_pop_estimate 
                           + noncitizen_pop_estimate 
                           + hispanic_or_latino_population_percentage_estimate
                           + households_below_poverty_line_percentage_estimate 
                           + vote_prop
                           + tot_crimes_pop
                           + active_287g, 
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T,
                           vartype = "bootstrap",
                           seed = 123,
                           nboots = 200)


### Transfers
table_s4b_adj_signing <- fect(transfer_binary ~ treatment_sc
                           + total_pop_estimate 
                           + noncitizen_pop_estimate 
                           + hispanic_or_latino_population_percentage_estimate
                           + households_below_poverty_line_percentage_estimate 
                           + vote_prop
                           + tot_crimes_pop
                           + active_287g, 
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T,
                           vartype = "bootstrap",
                           seed = 123,
                           nboots = 200)


### Removals
table_s4c_adj_signing <- fect(departed_binary ~ treatment_sc
                           + total_pop_estimate 
                           + noncitizen_pop_estimate 
                           + hispanic_or_latino_population_percentage_estimate
                           + households_below_poverty_line_percentage_estimate 
                           + vote_prop
                           + tot_crimes_pop
                           + active_287g, 
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T,
                           vartype = "bootstrap",
                           seed = 123,
                           nboots = 200)


# Create names
tables4_columns <- c("Signing", "Enactment")
tables4_rows <- c("Detainers ATT", "Detainers 95% CI Low", "Detainers 95% CI High",
                  "Transfers ATT", "Transfers 95% CI Low", "Transfers 95% CI High", 
                  "Removals ATT", "Removals 95% CI Low", "Removals 95% CI High")

# Table S4
tables4 <- data.frame(matrix(ncol = length(tables4_columns), 
                             nrow = length(tables4_rows)))
colnames(tables4) <- tables4_columns
rownames(tables4) <- tables4_rows


### Collect Coefficients
# Enactment Detainers
tables4$Enactment[1] <- round((table_s4a_adj_applied$est.avg[1]*100), 2)
tables4$Enactment[2] <- round((table_s4a_adj_applied$est.avg[3]*100), 2)
tables4$Enactment[3] <- round((table_s4a_adj_applied$est.avg[4]*100), 2)

# Enactment Transfers
tables4$Enactment[4] <- round((table_s4b_adj_applied$est.avg[1]*100), 2)
tables4$Enactment[5] <- round((table_s4b_adj_applied$est.avg[3]*100), 2)
tables4$Enactment[6] <- round((table_s4b_adj_applied$est.avg[4]*100), 2)

# Enactment Removals
tables4$Enactment[7] <- round((table_s4c_adj_applied$est.avg[1]*100), 2)
tables4$Enactment[8] <- round((table_s4c_adj_applied$est.avg[3]*100), 2)
tables4$Enactment[9] <- round((table_s4c_adj_applied$est.avg[4]*100), 2)


# Signing Detainers
tables4$Signing[1] <- round((table_s4a_adj_signing$est.avg[1]*100), 2)
tables4$Signing[2] <- round((table_s4a_adj_signing$est.avg[3]*100), 2)
tables4$Signing[3] <- round((table_s4a_adj_signing$est.avg[4]*100), 2)

# Signing Transfers
tables4$Signing[4] <- round((table_s4b_adj_signing$est.avg[1]*100), 2)
tables4$Signing[5] <- round((table_s4b_adj_signing$est.avg[3]*100), 2)
tables4$Signing[6] <- round((table_s4b_adj_signing$est.avg[4]*100), 2)

# Signing Removals
tables4$Signing[7] <- round((table_s4c_adj_signing$est.avg[1]*100), 2)
tables4$Signing[8] <- round((table_s4c_adj_signing$est.avg[3]*100), 2)
tables4$Signing[9] <- round((table_s4c_adj_signing$est.avg[4]*100), 2)


xtable::xtable(tables4)


###### Make Figures S20 - S22 #######   


### Make S20
# Enactment
plot_figure_s20a_adj_applied <- plot(table_s4a_adj_applied, 
                                main = "Secure Communities Enactment Date - Adjusted", 
                                ylab = "Detainers - Binary", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s20a_adj_applied
ggsave("plot_figure_s20a_adj_applied.jpeg")


# Signing
plot_figure_s20a_adj_signing <- plot(table_s4a_adj_signing, 
                                main = "Secure Communities Signing Date - Adjusted", 
                                ylab = "Detainers - Binary", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s20a_adj_signing
ggsave("plot_figure_s20a_adj_signing.jpeg")



### Make S21
# Enactment
plot_figure_s21a_adj_applied <- plot(table_s4b_adj_applied, 
                                main = "Secure Communities Enactment Date - Adjusted", 
                                ylab = "Transfers - Binary", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s21a_adj_applied
ggsave('plot_figure_s21a_adj_applied.jpeg')


# Signing
plot_figure_s21b_adj_signing <- plot(table_s4b_adj_signing, 
                                main = "Secure Communities Signing Date - Adjusted", 
                                ylab = "Transfer - Binary", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s21b_adj_signing
ggsave("plot_figure_s21b_adj_signing.jpeg")


### Make S22
# Plot results
plot_figure_s22a_adj_applied <- plot(table_s4c_adj_applied, 
                                main = "Secure Communities Enactment Date - Adjusted", 
                                ylab = "Removals - Binary", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
 
plot_figure_s22a_adj_applied
ggsave('plot_figure_s22a_adj_applied.jpeg')


### Signed
plot_figure_s22b_adj_signing <- plot(table_s4c_adj_signing, 
                                main = "Secure Communities Signing Date - Adjusted", 
                                ylab = "Removals - Binary", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s22b_adj_signing
ggsave('plot_figure_s22b_adj_signing.jpeg')




###### Make Figures S23 - S25 #######   

### 
### Enacted
figure_s23a_log1_adj_applied <- fect(detainers_log1 ~ treatment_sc_applied
                           + total_pop_estimate 
                           + noncitizen_pop_estimate 
                           + hispanic_or_latino_population_percentage_estimate
                           + households_below_poverty_line_percentage_estimate 
                           + vote_prop
                           + tot_crimes_pop
                           + active_287g, 
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T,
                           vartype = "bootstrap",
                           seed = 123,
                           nboots = 200)
# Plot results
plot_figure_s23a_log1_adj_applied <- plot(figure_s23a_log1_adj_applied, 
                                main = "Secure Communities Enactment Date - Adjusted", 
                                ylab = "ln(Detainers + 1)", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s23a_log1_adj_applied
ggsave('plot_figure_s23a_log1_adj_applied.jpeg')



### Signed
figure_s23b_log1_adj_signing <- fect(detainers_log1 ~ treatment_sc
                           + total_pop_estimate 
                           + noncitizen_pop_estimate 
                           + hispanic_or_latino_population_percentage_estimate
                           + households_below_poverty_line_percentage_estimate 
                           + vote_prop
                           + tot_crimes_pop
                           + active_287g, 
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T,
                           vartype = "bootstrap",
                           seed = 123,
                           nboots = 200)
# Plot results
plot_figure_s23b_log1_adj_signing <- plot(figure_s23b_log1_adj_signing, 
                                main = "Secure Communities Signing Date - Adjusted", 
                                ylab = "ln(Detainers + 1)", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s23b_log1_adj_signing
ggsave('plot_figure_s23b_log1_adj_signing.jpeg')


### S24
# Enacted
figure_s24a_log1_adj_applied <- fect(transfer_log1 ~ treatment_sc_applied
                           + total_pop_estimate 
                           + noncitizen_pop_estimate 
                           + hispanic_or_latino_population_percentage_estimate
                           + households_below_poverty_line_percentage_estimate 
                           + vote_prop
                           + tot_crimes_pop
                           + active_287g, 
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T,
                           vartype = "bootstrap",
                           seed = 123,
                           nboots = 200)
# Plot results
plot_figure_s24a_log1_adj_applied <- plot(figure_s24a_log1_adj_applied, 
                                main = "Secure Communities Enactment Date - Adjusted", 
                                ylab = "ln(Transfers + 1)", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s24a_log1_adj_applied
ggsave('plot_figure_s24a_log1_adj_applied.jpeg')




### Signed
figure_s24b_log1_adj_signing <- fect(transfer_log1 ~ treatment_sc
                           + total_pop_estimate 
                           + noncitizen_pop_estimate 
                           + hispanic_or_latino_population_percentage_estimate
                           + households_below_poverty_line_percentage_estimate 
                           + vote_prop
                           + tot_crimes_pop
                           + active_287g, 
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T,
                           vartype = "bootstrap",
                           seed = 123,
                           nboots = 200)
# Plot results
plot_figure_s24b_log1_adj_signing <- plot(figure_s24b_log1_adj_signing, 
                                main = "Secure Communities Signing Date - Adjusted", 
                                ylab = "ln(Transfers + 1)", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s24b_log1_adj_signing
ggsave('plot_figure_s24b_log1_adj_signing.jpeg')



### S25
### Enacted
figure_s25a_log1_adj_applied <- fect(departed_log1 ~ treatment_sc_applied
                           + total_pop_estimate 
                           + noncitizen_pop_estimate 
                           + hispanic_or_latino_population_percentage_estimate
                           + households_below_poverty_line_percentage_estimate 
                           + vote_prop
                           + tot_crimes_pop
                           + active_287g, 
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T,
                           vartype = "bootstrap",
                           seed = 123,
                           nboots = 200)
# Plot results
plot_figure_s25a_log1_adj_applied <- plot(figure_s25a_log1_adj_applied, 
                                main = "Secure Communities Enactment Date - Adjusted", 
                                ylab = "ln(Removals + 1)", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s25a_log1_adj_applied
ggsave('plot_figure_s25a_log1_adj_applied.jpeg')


### Signed
figure_s25b_log1_adj_signing <- fect(departed_log1 ~ treatment_sc
                           + total_pop_estimate 
                           + noncitizen_pop_estimate 
                           + hispanic_or_latino_population_percentage_estimate
                           + households_below_poverty_line_percentage_estimate 
                           + vote_prop
                           + tot_crimes_pop
                           + active_287g, 
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T,
                           vartype = "bootstrap",
                           seed = 123,
                           nboots = 200)
# Plot results
plot_figure_s25b_log1_adj_signing <- plot(figure_s25b_log1_adj_signing, 
                                main = "Secure Communities Signing Date - Adjusted", 
                                ylab = "ln(Removals + 1)", 
                                xlab = "Months Before and After Treatment",
                                stats = "F.p",
                                show.count = T,
                                cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s25b_log1_adj_signing
ggsave('plot_figure_s25b_log1_adj_signing.jpeg')


###################################
### Enacted after State Signing ###
###### Pre-Trend Fittings #########
###################################       


# Enactment AFTER signing
figure_s26a_unadj_signing <- fect(detainers_binary ~ treatment_sc, 
                                        data = enacted_after, 
                                        index = c("fips","time"), 
                                        method = "fe", 
                                        force = "two-way",
                                        parallel = T,
                                        se = T,
                                        vartype = "bootstrap",
                                        seed = 123,
                                        nboots = 200)
# Est Avg Effect
print(figure_s26a_unadj_signing$est.avg)

# Plot results
plot_figure_s26a_unadj_signing <- plot(figure_s26a_unadj_signing, 
                                             main = "Enactment After State Signing", 
                                             ylab = "Detainers - Binary",
                                             xlab = "Months Before and After Treatment",
                                             stats = "F.p",
                                             cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s26a_unadj_signing
ggsave('plot_figure_s26a_unadj_signing.jpeg')


#### Transfers
# Enactment AFTER signing
figure_s26b_unadj_signing <- fect(transfer_binary ~ treatment_sc, 
                                        data = enacted_after, 
                                        index = c("fips","time"), 
                                        method = "fe", 
                                        force = "two-way",
                                        parallel = T,
                                        se = T,
                                        vartype = "bootstrap",
                                        seed = 123,
                                        nboots = 200)
# Est Avg Effect
print(figure_s26b_unadj_signing$est.avg)

# Plot results
plot_figure_s26b_unadj_signing <- plot(figure_s26b_unadj_signing, 
                                             main = "Enactment After State Signing", 
                                             ylab = "Transfers - Binary", 
                                             xlab = "Months Before and After Treatment",
                                             stats = "F.p",
                                             cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s26b_unadj_signing
ggsave('plot_figure_s26b_unadj_signing.jpeg')



### Removals
# Enactment AFTER signing
figure_s26c_unadj_signing <- fect(departed_binary ~ treatment_sc, 
                                       data = enacted_after, 
                                       index = c("fips","time"), 
                                       method = "fe", 
                                       force = "two-way",
                                       parallel = T,
                                       se = T,
                                       vartype = "bootstrap",
                                       seed = 123,
                                       nboots = 200)
# Est Avg Effect
print(figure_s26c_unadj_signing$est.avg)

# Plot results
plot_figure_s26c_unadj_signing <- plot(figure_s26c_unadj_signing, 
                                            main = "Enactment After State Signing", 
                                            ylab = "Removals - Binary", 
                                            xlab = "Months Before and After Treatment",
                                            stats = "F.p",
                                            cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s26c_unadj_signing
ggsave('plot_figure_s26c_unadj_signing.jpeg')




#### Pre-Trend Fittings

figure_s27a_unadj_signing <- plot(figure_s26a_unadj_signing, type = "equiv", 
     cex.legend = 0.6, 
     main = "Enacted After State Signing - Testing Pre-Trend", 
     xlab = "Months Before Treatment",
     ylab = "Detainers - Binary",
     cex.text = 0.8)
figure_s27a_unadj_signing
ggsave('figure_s27a_unadj_signing.jpeg')



##### Pre-Trend Fittings
figure_s27b_unadj_signing <- plot(figure_s26b_unadj_signing, type = "equiv", 
     cex.legend = 0.6, 
     main = "Enacted After State Signing - Testing Pre-Trend", 
     xlab = "Months Before Treatment",
     ylab = "Transfers - Binary",
     cex.text = 0.8)
figure_s27b_unadj_signing
ggsave('figure_s27b_unadj_signing.jpeg')


##### Pre-Trend Fittings
figure_s27c_unadj_signing <- plot(figure_s26c_unadj_signing, type = "equiv", 
     cex.legend = 0.6, 
     main = "Enacted After State Signing - Testing Pre-Trend", 
     xlab = "Months Before Treatment",
     ylab = "Removals - Binary",
     cex.text = 0.8)
figure_s27c_unadj_signing
ggsave('figure_s27c_unadj_signing.jpeg')





##########################################
###### Clustered Standard Errors #########
##########################################


### Unadjusted

figure_s28a_unadj_signing <- fect(detainers_binary ~ treatment_sc, 
                            data = data_pnas, 
                            index = c("fips","time"), 
                            method = "fe", 
                            force = "two-way",
                            parallel = T,
                            se = T, 
                            seed = 123,
                            cl = "state_name")
print(figure_s28a_unadj_signing$est.avg)

# Plot results
plot_figure_s28a_unadj_signing <- plot(figure_s28a_unadj_signing, 
                                            main = "Secure Communities Signing Date - Unadjusted", 
                                            ylab = "Detainers Binary", 
                                            xlab = "Months Before and After Treatment",
                                            stats = "F.p",
                                            show.count = T,
                                            cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s28a_unadj_signing
ggsave('plot_figure_s28a_unadj_signing.jpeg')

## Transfers
figure_s28b_unadj_signing <- fect(transfer_binary ~ treatment_sc,  
                                      data = data_pnas, 
                                      index = c("fips","time"), 
                                      method = "fe", 
                                      force = "two-way",
                                      parallel = T,
                                      se = T, 
                                      seed = 123,
                                      cl = "state_name")

print(figure_s28b_unadj_signing$est.avg)

# Plot results
plot_figure_s28b_unadj_signing <- plot(figure_s28b_unadj_signing, 
                                            main = "Secure Communities Signing Date - Unadjusted", 
                                            ylab = "Transfers Binary", 
                                            xlab = "Months Before and After Treatment",
                                            stats = "F.p",
                                            show.count = T,
                                            cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s28b_unadj_signing
ggsave('plot_figure_s28b_unadj_signing.jpeg')

### Removals
figure_s28c_unadj_signing <- fect(departed_binary ~ treatment_sc, 
                                     data = data_pnas, 
                                     index = c("fips","time"), 
                                     method = "fe", 
                                     force = "two-way",
                                     parallel = T,
                                     se = T,
                                     seed = 123,
                                     cl = "state_name")
print(figure_s28c_unadj_signing$est.avg)

# Plot results
plot_figure_s28c_unadj_signing <- plot(figure_s28c_unadj_signing, 
                                           main = "Secure Communities Signing Date - Unadjusted", 
                                           ylab = "Removals Binary", 
                                           xlab = "Months Before and After Treatment",
                                           stats = "F.p",
                                           show.count = T,
                                           cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s28c_unadj_signing
ggsave('plot_figure_s28c_unadj_signing.jpeg')




### Adjusted 

figure_s29a_adj_signing <- fect(detainers_binary ~ treatment_sc 
                 + total_pop_estimate 
                 + noncitizen_pop_estimate 
                 + hispanic_or_latino_population_percentage_estimate
                 + households_below_poverty_line_percentage_estimate 
                 + vote_prop 
                 + tot_crimes_pop, 
                 data = data_pnas, 
                 index = c("fips","time"), 
                 method = "fe", 
                 force = "two-way",
                 parallel = T,
                 se = T, seed = 123,
                 cl = "state_name")

print(figure_s29a_adj_signing$est.avg)

# Plot results
plot_figure_s29a_adj_signing <- plot(figure_s29a_adj_signing, 
                                          main = "Secure Communities Signing Date - Adjusted", 
                                          ylab = "Detainers Binary", 
                                          xlab = "Months Before and After Treatment",
                                          stats = "F.p",
                                          show.count = T,
                                          cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s29a_adj_signing
ggsave("plot_figure_s29a_adj_signing.jpeg")

## Transfers
figure_s29b_adj_signing <- fect(transfer_binary ~ treatment_sc
                           + total_pop_estimate 
                           + noncitizen_pop_estimate 
                           + hispanic_or_latino_population_percentage_estimate
                           + households_below_poverty_line_percentage_estimate 
                           + vote_prop 
                           + tot_crimes_pop,  
                           data = data_pnas, 
                           index = c("fips","time"), 
                           method = "fe", 
                           force = "two-way",
                           parallel = T,
                           se = T, 
                           seed = 123,
                           cl = "state_name")

print(figure_s29b_adj_signing$est.avg)


# Plot results
plot_figure_s29b_adj_signing<- plot(figure_s29b_adj_signing, 
                                          main = "Secure Communities Signing Date - Adjusted", 
                                          ylab = "Transfers Binary", 
                                          xlab = "Months Before and After Treatment",
                                          stats = "F.p",
                                          show.count = T,
                                          cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s29b_adj_signing
ggsave('plot_figure_s29b_adj_signing.jpeg')

### Removals
figure_s29c_adj_signing <- fect(departed_binary ~ treatment_sc
                          + total_pop_estimate 
                          + noncitizen_pop_estimate 
                          + hispanic_or_latino_population_percentage_estimate
                          + households_below_poverty_line_percentage_estimate 
                          + vote_prop 
                          + tot_crimes_pop, 
                          data = data_pnas, 
                          index = c("fips","time"), 
                          method = "fe", 
                          force = "two-way",
                          parallel = T,
                          se = T, 
                          seed = 123,
                          cl = "state_name")

print(figure_s29c_adj_signing$est.avg)


# Plot results
plot_figure_s29c_adj_signing<- plot(figure_s29c_adj_signing, 
                                         main = "Secure Communities Signing Date - Adjusted", 
                                         ylab = "Removals Binary", 
                                         xlab = "Months Before and After Treatment",
                                         stats = "F.p",
                                         show.count = T,
                                         cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure_s29c_adj_signing
ggsave('plot_figure_s29c_adj_signing.jpeg')




##################################################
### Create Table - Unadjusted ####################
#### Clustered Standard Errors - State Level ####
##################################################


# Create names
tables5_columns <- c("ATT", "95% CI Low", "95% CI High")
tables5_rows <- c("Detainers - Unadjusted", 
                  "Transfers - Unadjusted", 
                  "Removals - Unadjusted",
                  "Detainers - Adjusted", 
                  "Transfers - Adjusted", 
                  "Removals - Adjusted")

# Table S3
tables5 <- data.frame(matrix(ncol = length(tables5_columns), 
                             nrow = length(tables5_rows)))
colnames(tables5) <- tables5_columns
rownames(tables5) <- tables5_rows


### Collect Coefficients
# ATT
tables5$ATT[1] <- round((figure_s28a_unadj_signing$est.avg[1]*100), 2)
tables5$ATT[2] <- round((figure_s28b_unadj_signing$est.avg[1]*100), 2)
tables5$ATT[3] <- round((figure_s28c_unadj_signing$est.avg[1]*100), 2)
tables5$ATT[4] <- round((figure_s29a_adj_signing$est.avg[1]*100), 2)
tables5$ATT[5] <- round((figure_s29b_adj_signing$est.avg[1]*100), 2)
tables5$ATT[6] <- round((figure_s29c_adj_signing$est.avg[1]*100), 2)

# 95% CI Low
tables5$`95% CI Low`[1] <- round((figure_s28a_unadj_signing$est.avg[3]*100), 2)
tables5$`95% CI Low`[2] <- round((figure_s28b_unadj_signing$est.avg[3]*100), 2)
tables5$`95% CI Low`[3] <- round((figure_s28c_unadj_signing$est.avg[3]*100), 2)
tables5$`95% CI Low`[4] <- round((figure_s29a_adj_signing$est.avg[3]*100), 2)
tables5$`95% CI Low`[5] <- round((figure_s29b_adj_signing$est.avg[3]*100), 2)
tables5$`95% CI Low`[6] <- round((figure_s29c_adj_signing$est.avg[3]*100), 2)

# 95% CI High
tables5$`95% CI High`[1] <- round((figure_s28a_unadj_signing$est.avg[4]*100), 2)
tables5$`95% CI High`[2] <- round((figure_s28b_unadj_signing$est.avg[4]*100), 2)
tables5$`95% CI High`[3] <- round((figure_s28c_unadj_signing$est.avg[4]*100), 2)
tables5$`95% CI High`[4] <- round((figure_s29a_adj_signing$est.avg[4]*100), 2)
tables5$`95% CI High`[5] <- round((figure_s29b_adj_signing$est.avg[4]*100), 2)
tables5$`95% CI High`[6] <- round((figure_s29c_adj_signing$est.avg[4]*100), 2)


xtable::xtable(tables5)


#################################
###### 287(g) Agreements ########
#################################

data_pnas %>%
  group_by(fips) %>%
  mutate(active_287g_ever = ifelse(any(active_287g == 1), 1, 0)) %>%
  ungroup() -> data_pnas_287g

## No 287(g) counties
data_pnas_287g %>%
  filter(active_287g_ever == 0) -> data_pnas_287g_no287
length(unique(data_test2_287g_no287$fips))

## Only 287(g) counties
data_pnas_287g %>%
  filter(active_287g_ever == 1) -> data_pnas_287g_yes287

length(unique(data_pnas_287g_yes287$fips))

##############################
plot_figure_s30 <- data_pnas_287g %>%
  filter(month_year_running == "2013-12-01") %>%
  mutate(active_287g_ever = as.factor(active_287g_ever)) %>%
  usmap::plot_usmap(data = ., values = "active_287g_ever",
                    exclude = c("AK","HI")) +
  scale_fill_manual(values = c('0' = "white", '1' = "blue"),
                    labels = c("No", "Yes"),
                    name = "287(g)") + 
  theme(legend.position = "right") +
  ggtitle("Past 287(g) Agreements")
plot_figure_s30
ggsave('plot_figure_s30.jpeg')





#################################################
######### Create Table - Adjusted ###############
####### Overall + 287(g) YES + 287(g) NO ########
#################################################

### 287(g) yes
# Detainers
table_s6a_yes287g_adj_signing <- fect(detainers_binary ~ treatment_sc 
                             + total_pop_estimate 
                             + noncitizen_pop_estimate 
                             + hispanic_or_latino_population_percentage_estimate
                             + households_below_poverty_line_percentage_estimate 
                             + vote_prop 
                             + tot_crimes_pop,
                             data = data_pnas_287g_yes287, 
                             index = c("fips","time"), 
                             method = "fe", 
                             force = "two-way",
                             parallel = T,
                             se = T,
                             vartype = "bootstrap",
                             seed = 123,
                             nboots = 200)

# Plot results
plot_table_s6a_yes287g_adj_signing <- plot(table_s6a_yes287g_adj_signing, 
                                        main = "Secure Communities Signing Date - 287(g) Only - Adjusted", 
                                        ylab = "Detainers - Binary", 
                                        xlab = "Months Before and After Treatment",
                                        stats = "F.p",
                                        show.count = T,
                                        cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
plot_table_s6a_yes287g_adj_signing
ggsave('plot_table_s6a_yes287g_adj_signing.jpeg') 

# Transfers
table_s6b_yes287g_adj_signing <- fect(transfer_binary ~ treatment_sc 
                             + total_pop_estimate 
                             + noncitizen_pop_estimate 
                             + hispanic_or_latino_population_percentage_estimate
                             + households_below_poverty_line_percentage_estimate 
                             + vote_prop 
                             + tot_crimes_pop,
                             data = data_pnas_287g_yes287, 
                             index = c("fips","time"), 
                             method = "fe", 
                             force = "two-way",
                             parallel = T,
                             se = T,
                             vartype = "bootstrap",
                             seed = 123,
                             nboots = 200)

# Plot results
plot_table_s6b_yes287g_adj_signing <- plot(table_s6b_yes287g_adj_signing, 
                                        main = "Secure Communities Signing Date - 287(g) Only - Adjusted", 
                                        ylab = "Transfers - Binary", 
                                        xlab = "Months Before and After Treatment",
                                        stats = "F.p",
                                        show.count = T,
                                        cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
plot_table_s6b_yes287g_adj_signing
ggsave('plot_table_s6b_yes287g_adj_signing.jpeg') 

# Removals
table_s6c_yes287g_adj_signing <- fect(departed_binary ~ treatment_sc 
                             + total_pop_estimate 
                             + noncitizen_pop_estimate 
                             + hispanic_or_latino_population_percentage_estimate
                             + households_below_poverty_line_percentage_estimate 
                             + vote_prop 
                             + tot_crimes_pop,
                             data = data_pnas_287g_yes287, 
                             index = c("fips","time"), 
                             method = "fe", 
                             force = "two-way",
                             parallel = T,
                             se = T,
                             vartype = "bootstrap",
                             seed = 123,
                             nboots = 200)

# Plot results
plot_table_s6c_yes287g_adj_signing <- plot(table_s6c_yes287g_adj_signing, 
                                        main = "Secure Communities Signing Date - 287(g) Only - Adjusted", 
                                        ylab = "Removals - Binary", 
                                        xlab = "Months Before and After Treatment",
                                        stats = "F.p",
                                        show.count = T,
                                        cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
plot_table_s6c_yes287g_adj_signing
ggsave('plot_table_s6c_yes287g_adj_signing.jpeg')

### 287(g) NO
# Detainers
table_s6a_no287g_adj_signing <- fect(detainers_binary ~ treatment_sc 
                            + total_pop_estimate
                            + noncitizen_pop_estimate 
                            + hispanic_or_latino_population_percentage_estimate
                            + households_below_poverty_line_percentage_estimate 
                            + vote_prop 
                            + tot_crimes_pop,
                            data = data_pnas_287g_no287, 
                            index = c("fips","time"), 
                            method = "fe", 
                            force = "two-way",
                            parallel = T,
                            se = T,
                            vartype = "bootstrap",
                            seed = 123,
                            nboots = 200)

# Plot results
plot_table_s6a_no287g_adj_signing <- plot(table_s6a_no287g_adj_signing, 
                                       main = "Secure Communities Signing Date - No 287(g) - Adjusted", 
                                       ylab = "Detainers - Binary", 
                                       xlab = "Months Before and After Treatment",
                                       stats = "F.p",
                                       show.count = T,
                                       cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
plot_table_s6a_no287g_adj_signing
# 
ggsave('plot_table_s6a_no287g_adj_signing.jpeg')

# Transfers
table_s6b_no287g_adj_signing <- fect(transfer_binary ~ treatment_sc 
                            + total_pop_estimate 
                            + noncitizen_pop_estimate 
                            + hispanic_or_latino_population_percentage_estimate
                            + households_below_poverty_line_percentage_estimate 
                            + vote_prop 
                            + tot_crimes_pop,
                            data = data_pnas_287g_no287, 
                            index = c("fips","time"), 
                            method = "fe", 
                            force = "two-way",
                            parallel = T,
                            se = T,
                            vartype = "bootstrap",
                            seed = 123,
                            nboots = 200)

# Plot results
plot_table_s6b_no287g_adj_signing <- plot(table_s6b_no287g_adj_signing, 
                                       main = "Secure Communities Signing Date - No 287(g) - Adjusted", 
                                       ylab = "Transfers - Binary", 
                                       xlab = "Months Before and After Treatment",
                                       stats = "F.p",
                                       show.count = T,
                                       cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
plot_table_s6b_no287g_adj_signing
ggsave('plot_table_s6b_no287g_adj_signing.jpeg')

### Removals
table_s6c_no287g_adj_signing <- fect(departed_binary ~ treatment_sc 
                            + total_pop_estimate 
                            + noncitizen_pop_estimate 
                            + hispanic_or_latino_population_percentage_estimate
                            + households_below_poverty_line_percentage_estimate 
                            + vote_prop 
                            + tot_crimes_pop,
                            data = data_pnas_287g_no287, 
                            index = c("fips","time"), 
                            method = "fe", 
                            force = "two-way",
                            parallel = T,
                            se = T,
                            vartype = "bootstrap",
                            seed = 123,
                            nboots = 200)

# Plot results
plot_table_s6c_no287g_adj_signing <- plot(table_s6c_no287g_adj_signing, 
                                       main = "Secure Communities Signing Date - No 287(g) - Adjusted", 
                                       ylab = "Removals - Binary", 
                                       xlab = "Months Before and After Treatment",
                                       stats = "F.p",
                                       show.count = T,
                                       cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
plot_table_s6c_no287g_adj_signing
ggsave('plot_table_s6c_no287g_adj_signing.jpeg')




#########################
### Make Table S6 #######

# Create names
tables6_columns <- c("Overall", "287(g) YES", "287(g) NO")
tables6_rows <- c("Detainers ATT", "Detainers 95% CI Low", "Detainers 95% CI High",
                  "Transfers ATT", "Transfers 95% CI Low", "Transfers 95% CI High", 
                  "Removals ATT", "Removals 95% CI Low", "Removals 95% CI High")

# Table 1
tables6 <- data.frame(matrix(ncol = length(tables6_columns), 
                             nrow = length(tables6_rows)))
colnames(tables6) <- tables6_columns
rownames(tables6) <- tables6_rows

### Collect Coefficients
# Overall Detainers
tables6$Overall[1] <- round((table_s4a_adj_signing$est.avg[1]*100), 2)
tables6$Overall[2] <- round((table_s4a_adj_signing$est.avg[3]*100), 2)
tables6$Overall[3] <- round((table_s4a_adj_signing$est.avg[4]*100), 2)

# Overall Transfers
tables6$Overall[4] <- round((table_s4b_adj_signing$est.avg[1]*100), 2)
tables6$Overall[5] <- round((table_s4b_adj_signing$est.avg[3]*100), 2)
tables6$Overall[6] <- round((table_s4b_adj_signing$est.avg[4]*100), 2)

# Overall Removals
tables6$Overall[7] <- round((table_s4c_adj_signing$est.avg[1]*100), 2)
tables6$Overall[8] <- round((table_s4c_adj_signing$est.avg[3]*100), 2)
tables6$Overall[9] <- round((table_s4c_adj_signing$est.avg[4]*100), 2)

# 287(g) Yes Detainers
tables6$`287(g) YES`[1] <- round((table_s6a_yes287g_adj_signing$est.avg[1]*100), 2)
tables6$`287(g) YES`[2] <- round((table_s6a_yes287g_adj_signing$est.avg[3]*100), 2)
tables6$`287(g) YES`[3] <- round((table_s6a_yes287g_adj_signing$est.avg[4]*100), 2)

# 287(g) Yes Transfers
tables6$`287(g) YES`[4] <- round((table_s6b_yes287g_adj_signing$est.avg[1]*100), 2)
tables6$`287(g) YES`[5] <- round((table_s6b_yes287g_adj_signing$est.avg[3]*100), 2)
tables6$`287(g) YES`[6] <- round((table_s6b_yes287g_adj_signing$est.avg[4]*100), 2)

# 287(g) Yes Removals
tables6$`287(g) YES`[7] <- round((table_s6c_yes287g_adj_signing$est.avg[1]*100), 2)
tables6$`287(g) YES`[8] <- round((table_s6c_yes287g_adj_signing$est.avg[3]*100), 2)
tables6$`287(g) YES`[9] <- round((table_s6c_yes287g_adj_signing$est.avg[4]*100), 2)

# 287(g) No Detainers
tables6$`287(g) NO`[1] <- round((table_s6a_no287g_adj_signing$est.avg[1]*100), 2)
tables6$`287(g) NO`[2] <- round((table_s6a_no287g_adj_signing$est.avg[3]*100), 2)
tables6$`287(g) NO`[3] <- round((table_s6a_no287g_adj_signing$est.avg[4]*100), 2)

# 287(g) No Transfers
tables6$`287(g) NO`[4] <- round((table_s6b_no287g_adj_signing$est.avg[1]*100), 2)
tables6$`287(g) NO`[5] <- round((table_s6b_no287g_adj_signing$est.avg[3]*100), 2)
tables6$`287(g) NO`[6] <- round((table_s6b_no287g_adj_signing$est.avg[4]*100), 2)

# 287(g) No Removals
tables6$`287(g) NO`[7] <- round((table_s6c_no287g_adj_signing$est.avg[1]*100), 2)
tables6$`287(g) NO`[8] <- round((table_s6c_no287g_adj_signing$est.avg[3]*100), 2)
tables6$`287(g) NO`[9] <- round((table_s6c_no287g_adj_signing$est.avg[4]*100), 2)

xtable::xtable(tables6)








