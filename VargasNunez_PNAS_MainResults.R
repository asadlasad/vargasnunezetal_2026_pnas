rm(list = ls() )
### Final Dataset for Analyses
## Runs main results 
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


###################################       
########## Figure 1 ###############       
###################################       

###################################       
########## Detainers ##############
########## Figure 1 ###############

#### Enacting
figure1_unadj_applied <- fect(detainers_binary ~ treatment_sc_applied, 
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
print(figure1_unadj_applied$att.avg)
att_fig1_panelc_unadjusted <- round((figure1_unadj_applied$att.avg*100), 2)

# Plot results
plot_figure1_unadj_applied <- plot(figure1_unadj_applied, 
                                  main = "Secure Communities Enactment Date - Unadjusted", 
                                  ylab = "Detainers - Binary", 
                                  xlab = "Months Before and After Treatment",
                                  show.count = T,
                                  cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure1_unadj_applied <- plot_figure1_unadj_applied +
  annotate("text", x = -35, y = .10, label = "ATT:", 
           color = "black", size = 4.5) +
  annotate("text", x = -31.5, y = .10, label = att_fig1_panelc_unadjusted, 
           color = "black", size = 4.5) +
  annotate("text", x = -25.75, y = .10, label = "(italic(p)<0.001)",
           parse = T,
           color = "black", size = 4.5) 


#### Signed
figure1_unadj_signed <- fect(detainers_binary ~ treatment_sc, 
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
print(figure1_unadj_signed$att.avg)
att_fig1_paneld_unadjusted <- round((figure1_unadj_signed$att.avg*100), 2)

# Plot results
plot_figure1_unadj_signed <- plot(figure1_unadj_signed, 
                                  main = "Secure Communities Signing Date - Unadjusted", 
                                  ylab = "Detainers - Binary", 
                                  xlab = "Months Before and After Treatment",
                                  show.count = T,
                                  cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure1_unadj_signed <- plot_figure1_unadj_signed +
  annotate("text", x = -14, y = .11, label = "ATT:",
           color = "black", size = 4.5) +
  annotate("text", x = -10, y = .11, label = att_fig1_paneld_unadjusted,
           color = "black", size = 4.5) +
  annotate("text", x = -3.5, y = .11, label = "(italic(p)<0.001)",
           parse = T,
           color = "black", size = 4.5)

####### Make Figure 1

fig1 <- ggpubr::ggarrange(plot_figure1_unadj_signed,
                  plot_figure1_unadj_applied,
                  nrow = 2)
fig1
ggsave("fig1_pnas.pdf", fig1)


###################################       
########## Transfers ##############
########## Figure 2 ###############

### Enacted
figure2_unadj_applied <- fect(transfer_binary ~ treatment_sc_applied, 
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
print(figure2_unadj_applied$att.avg)
att_fig2_panelc_unadjusted <- round((figure2_unadj_applied$att.avg*100), 2)

# Plot results
plot_figure2_unadj_applied <- plot(figure2_unadj_applied, 
                                  main = "Secure Communities Enactment Date - Unadjusted", 
                                  ylab = "Transfer - Binary", 
                                  xlab = "Months Before and After Treatment",
                                  show.count = T,
                                  cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure2_unadj_applied <- plot_figure2_unadj_applied +
  annotate("text", x = -35, y = .09, label = "ATT:", 
           color = "black", size = 4.5) +
  annotate("text", x = -31.5, y = .09, label = att_fig2_panelc_unadjusted, 
           color = "black", size = 4.5) +
  annotate("text", x = -26, y = .09, label = "(italic(p)<0.001)", 
           parse = T,
           color = "black", size = 4.5) 


### Signed
figure2_unadj_signed <- fect(transfer_binary ~ treatment_sc, 
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
print(figure2_unadj_signed$att.avg)
att_fig2_paneld_unadjusted <- round((figure2_unadj_signed$att.avg*100), 2)

# Plot results
plot_figure2_unadj_signed <- plot(figure2_unadj_signed, 
                                  main = "Secure Communities Signing Date - Unadjusted", 
                                  ylab = "Transfer -  Binary",
                                  xlab = "Months Before and After Treatment",
                                  show.count = T,
                                  cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure2_unadj_signed <- plot_figure2_unadj_signed +
  annotate("text", x = -14, y = .08, label = "ATT:", 
           color = "black", size = 4.5) +
  annotate("text", x = -10, y = .08, label = att_fig2_paneld_unadjusted, 
           color = "black", size = 4.5) +
  annotate("text", x = -4, y = .08, label = "(italic(p)==0.016)",
           parse = TRUE,
           color = "black", size = 4.5) 


###### Make Figure 2
fig2 <- ggpubr::ggarrange(plot_figure2_unadj_signed,
                  plot_figure2_unadj_applied,
                  nrow = 2)
fig2
ggsave("fig2_pnas.pdf", fig2)



###################################       
########## Removals ###############
########## Figure 3 ###############

### Enacted
figure3_unadj_applied <- fect(departed_binary ~ treatment_sc_applied, 
                             data = data_pnas, 
                             index = c("fips","time"), 
                             method = "fe", 
                             force = "two-way",
                             parallel = T,
                             se = T,
                             vartype = "bootstrap",
                             seed = 123,
                             nboots = 200)
print(figure3_unadj_applied$att.avg)
att_fig3_panelc_unadjusted <- round((figure3_unadj_applied$att.avg*100), 2)

# Plot results
plot_figure3_unadj_applied <- plot(figure3_unadj_applied, 
                                  main = "Secure Communities Enactment Date - Unadjusted", 
                                  ylab = "Removals - Binary", 
                                  xlab = "Months Before and After Treatment",
                                  show.count = T,
                                  cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure3_unadj_applied <- plot_figure3_unadj_applied +
  annotate("text", x = -35, y = .09, label = "ATT:", 
           color = "black", size = 4.5) +
  annotate("text", x = -31.5, y = .09, label = att_fig3_panelc_unadjusted, 
           color = "black", size = 4.5) +
  annotate("text", x = -25.75, y = .09, label = "(italic(p)<0.001)",
           parse = T,
           color = "black", size = 4.5) 


### Signed
figure3_unadj_signed <- fect(departed_binary ~ treatment_sc, 
                             data = data_pnas, 
                             index = c("fips","time"), 
                             method = "fe", 
                             force = "two-way",
                             parallel = T,
                             se = T,
                             vartype = "bootstrap",
                             seed = 123,
                             nboots = 200)
print(figure3_unadj_signed$att.avg)
att_fig3_paneld_unadjusted <- round((figure3_unadj_signed$att.avg*100), 2)

# Plot results
plot_figure3_unadj_signed <- plot(figure3_unadj_signed, 
                                  main = "Secure Communities Signing Date - Unadjusted", 
                                  ylab = "Removals - Binary", 
                                  xlab = "Months Before and After Treatment",
                                  show.count = T,
                                  cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# 
plot_figure3_unadj_signed <- plot_figure3_unadj_signed +
  annotate("text", x = -14, y = .08, label = "ATT:", 
           color = "black", size = 4.5) +
  annotate("text", x = -10, y = .08, label = att_fig3_paneld_unadjusted, 
           color = "black", size = 4.5) +
  annotate("text", x = -3.5, y = .08, label = "(italic(p)==0.0017)", 
           parse = T,
           color = "black", size = 4.5) 

## Est Avg Effect
print(figure3_unadj_signed$att.avg)


###### Make Figure 3
fig3 <- ggpubr::ggarrange(plot_figure3_unadj_signed,
                  plot_figure3_unadj_applied,
                  nrow = 2)
fig3
ggsave("fig3_pnas.pdf", fig3)


###################################       
########### Table 1 ###############       
###################################       

### Separate 287(g) Yes/No counties
data_pnas %>%
  group_by(fips) %>%
  mutate(active_287g_ever = ifelse(any(active_287g == 1), 1, 0)) %>%
  ungroup() -> data_pnas_287g

## No 287(g) counties
data_pnas_287g %>%
  filter(active_287g_ever == 0) -> data_pnas_287g_no287
length(unique(data_pnas_287g_no287$fips))

## Only 287(g) counties
data_pnas_287g %>%
  filter(active_287g_ever == 1) -> data_pnas_287g_yes287
length(unique(data_pnas_287g_yes287$fips))




### Make Table  ####################
#### Overall effects and ###########
#### Yes 287(g) + No 287(g) ########

##### 287(g) Yes 

### Detainers
figure1_yes287g_unadj_signed <- fect(detainers_binary ~ treatment_sc,
                               data = data_pnas_287g_yes287, 
                               index = c("fips","time"), 
                               method = "fe", 
                               force = "two-way",
                               parallel = T,
                               se = T,
                               vartype = "bootstrap",
                               seed = 123,
                               nboots = 200)

### Transfers
figure2_yes287g_unadj_signed <- fect(transfer_binary ~ treatment_sc,
                               data = data_pnas_287g_yes287, 
                               index = c("fips","time"), 
                               method = "fe", 
                               force = "two-way",
                               parallel = T,
                               se = T,
                               vartype = "bootstrap",
                               seed = 123,
                               nboots = 200)

### Removals
figure3_yes287g_unadj_signed <- fect(departed_binary ~ treatment_sc,
                               data = data_pnas_287g_yes287, 
                               index = c("fips","time"), 
                               method = "fe", 
                               force = "two-way",
                               parallel = T,
                               se = T,
                               vartype = "bootstrap",
                               seed = 123,
                               nboots = 200)

##### 287(g) NO 
### Detainers
figure1_no287g_unadj_signed <- fect(detainers_binary ~ treatment_sc,
                              data = data_pnas_287g_no287, 
                              index = c("fips","time"), 
                              method = "fe", 
                              force = "two-way",
                              parallel = T,
                              se = T,
                              vartype = "bootstrap",
                              seed = 123,
                              nboots = 200)

### Transfers
figure2_no287g_unadj_signed <- fect(transfer_binary ~ treatment_sc,
                              data = data_pnas_287g_no287, 
                              index = c("fips","time"), 
                              method = "fe", 
                              force = "two-way",
                              parallel = T,
                              se = T,
                              vartype = "bootstrap",
                              seed = 123,
                              nboots = 200)

### Removals
figure3_no287g_unadj_signed <- fect(departed_binary ~ treatment_sc,
                              data = data_pnas_287g_no287, 
                              index = c("fips","time"), 
                              method = "fe", 
                              force = "two-way",
                              parallel = T,
                              se = T,
                              vartype = "bootstrap",
                              seed = 123,
                              nboots = 200)




# Create names
columns <- c("Overall", "287(g) YES", "287(g) NO")
rows <- c("Detainers ATT", "Detainers 95% CI Low", "Detainers 95% CI High",
          "Transfers ATT", "Transfers 95% CI Low", "Transfers 95% CI High", 
          "Removals ATT", "Removals 95% CI Low", "Removals 95% CI High")

# Table 1
table1 <- data.frame(matrix(ncol = length(columns), 
                            nrow = length(rows)))
colnames(table1) <- columns
rownames(table1) <- rows

### Collect Coefficients
# Overall Detainers
table1$Overall[1] <- round((figure1_unadj_signed$est.avg[1]*100), 2)
table1$Overall[2] <- round((figure1_unadj_signed$est.avg[3]*100), 2)
table1$Overall[3] <- round((figure1_unadj_signed$est.avg[4]*100), 2)

# Overall Transfers
table1$Overall[4] <- round((figure2_unadj_signed$est.avg[1]*100), 2)
table1$Overall[5] <- round((figure2_unadj_signed$est.avg[3]*100), 2)
table1$Overall[6] <- round((figure2_unadj_signed$est.avg[4]*100), 2)

# Overall Removals
table1$Overall[7] <- round((figure3_unadj_signed$est.avg[1]*100), 2)
table1$Overall[8] <- round((figure3_unadj_signed$est.avg[3]*100), 2)
table1$Overall[9] <- round((figure3_unadj_signed$est.avg[4]*100), 2)

# 287(g) Yes Detainers
table1$`287(g) YES`[1] <- round((figure1_yes287g_unadj_signed$est.avg[1]*100), 2)
table1$`287(g) YES`[2] <- round((figure1_yes287g_unadj_signed$est.avg[3]*100), 2)
table1$`287(g) YES`[3] <- round((figure1_yes287g_unadj_signed$est.avg[4]*100), 2)

# 287(g) Yes Transfers
table1$`287(g) YES`[4] <- round((figure2_yes287g_unadj_signed$est.avg[1]*100), 2)
table1$`287(g) YES`[5] <- round((figure2_yes287g_unadj_signed$est.avg[3]*100), 2)
table1$`287(g) YES`[6] <- round((figure2_yes287g_unadj_signed$est.avg[4]*100), 2)

# 287(g) Yes Removals
table1$`287(g) YES`[7] <- round((figure3_yes287g_unadj_signed$est.avg[1]*100), 2)
table1$`287(g) YES`[8] <- round((figure3_yes287g_unadj_signed$est.avg[3]*100), 2)
table1$`287(g) YES`[9] <- round((figure3_yes287g_unadj_signed$est.avg[4]*100), 2)

# 287(g) No Detainers
table1$`287(g) NO`[1] <- round((figure1_no287g_unadj_signed$est.avg[1]*100), 2)
table1$`287(g) NO`[2] <- round((figure1_no287g_unadj_signed$est.avg[3]*100), 2)
table1$`287(g) NO`[3] <- round((figure1_no287g_unadj_signed$est.avg[4]*100), 2)

# 287(g) No Transfers
table1$`287(g) NO`[4] <- round((figure2_no287g_unadj_signed$est.avg[1]*100), 2)
table1$`287(g) NO`[5] <- round((figure2_no287g_unadj_signed$est.avg[3]*100), 2)
table1$`287(g) NO`[6] <- round((figure2_no287g_unadj_signed$est.avg[4]*100), 2)

# 287(g) No Removals
table1$`287(g) NO`[7] <- round((figure3_no287g_unadj_signed$est.avg[1]*100), 2)
table1$`287(g) NO`[8] <- round((figure3_no287g_unadj_signed$est.avg[3]*100), 2)
table1$`287(g) NO`[9] <- round((figure3_no287g_unadj_signed$est.avg[4]*100), 2)

xtable::xtable(table1)


