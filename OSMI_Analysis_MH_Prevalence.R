#setup#

library(tidyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(skimr)
library(janitor)
library(gridExtra)

setwd("C:/Users/ryany/OneDrive/Desktop/Google Data Analytics R Files")

#loading in file and viewing#
osmi_2022 <- read_csv("OSMI_2022.csv")
view(osmi_2022)

#renaming headers#
##disorder questions confusing, consulted osmi website to affirm possible/confirmed differentiation##
colnames(osmi_2022) <- c("id", "self_employed", "company_size", 
                         "tech_company", "tech_role_primary", "mh_coverage", 
                         "mh_options_aware", "mh_employer_discuss", "mh_resource_employer_offer", 
                         "anon_protect", "mh_leave_difficulty", "mh_ph_discuss_coworker",
                         "comfort_supervisor", "mh_employer_discuss_personal", "description_mh_employer_discuss_personal",
                         "comfort_coworker", "mh_coworker_discuss_personal", "description_mh_coworker_discuss_personal",
                         "mh_coworker_discuss_personal_2", "description_mh_coworker_discuss_personal_2", "ph_employer_importance", 
                         "mh_employer_importance", "mh_personal_coverage", "mh_resource_aware", 
                         "mh_reveal_client", "mh_reveal_client_outcome", "mh_reveal_coworkers", 
                         "mh_reveal_coworkers_outcome", "mh_productivity", "mh_productivity_worktime",
                         "prev_employer", "prev_employer_tech_company", "mh_coverage_prev_employer",
                         "mh_options_aware_prev_employer", "mh_prev_employer_discuss", "mh_resource_prev_employer_offer", 
                         "anon_protect_prev_employer", "comfort_prev_employer", "comfort_prev_supervisor", 
                         "mh_prev_employer_discuss_personal", "description_mh_prev_employer_discuss_personal", "comfort_prev_coworker",
                         "mh_prev_coworker_discuss_personal", "description_mh_prev_coworker_discuss_personal", "mh_prev_coworker_discuss_personal_2",
                         "description_mh_prev_coworker_discuss_personal_2", "ph_prev_employer_importance", "mh_prev_employer_importance", 
                         "mh_current_diag", "mh_diag_history","diag_anxiety_disorder", 
                         "diag_mood_disorder", "diag_psychotic_disorder", "diag_eating_disorder", 
                         "diag_adhd_disorder", "diag_personality_disorder", "diag_ocd_disorder", 
                         "diag_ptsd_disorder", "diag_stress_disorder", "diag_did_disorder", 
                         "diag_sud_disorder", "diag_addiction_disorder", "diag_other_disorder", 
                         "possible_anxiety_disorder", "possible_mood_disorder", "possible_psychotic_disorder",
                         "possible_eating_disorder", "possible_adhd_disorder", "possible_personality_disorder", 
                         "possible_ocd_disorder", "possible_ptsd_disorder", "possible_stress_disorder", 
                         "possible_did_disorder","possible_sud_disorder", "possible_addiction_disorder", 
                         "possible_other_disorder", "past_anxiety_disorder", "past_mood_disorder", 
                         "past_psychotic_disorder", "past_eating_disorder", "past_adhd_disorder", 
                         "past_personality_disorder", "past_ocd_disorder", "past_ptsd_disorder", 
                         "past_stress_disorder", "past_did_disorder", "past_sud_disorder", 
                         "past_addiction_disorder", "past_other_disorder", "mh_past_diag", 
                         "mh_disorder_treat","mh_fam_hist", "mh_work_disrupt_treated", 
                         "mh_work_disrupt_untreated", "mh_other_discuss_bad_reveal", "mh_diag_share_fam_friends", 
                         "ph_discuss_potential_employer", "ph_discuss_potential_employer_why", "mh_discuss_potential_employer", 
                         "mh_discuss_potential_employer_why", "mh_openid", "mh_openid_outcome", 
                         "mh_openid_outcome_description", "mh_perceived_rx", "mh_observe_bad_rx", 
                         "mh_observe_bad_rx_description", "mh_observe_good_rx", "mh_observe_good_rx_description", 
                         "mh_perceived_industry_support", "mh_industry_support_suggestion", "add_comments", 
                         "follow_up", "age", "gender", 
                         "country_res", "us_state_res", "race", 
                         "race_other", "country_work", "us_state_work", 
                         "covid_diag", "start_date", "submit_date",
                         "network_id")

#removing irrelevant columns#
osmi_select <- osmi_2022 %>% 
  select(-id, -follow_up, -start_date,
         -submit_date, -network_id)

#combining areas where multiple columns were used for a single question#
osmi_unite <- osmi_select %>% 
  unite("diag_disorder", diag_anxiety_disorder, 
        diag_mood_disorder, diag_psychotic_disorder, diag_eating_disorder, 
        diag_adhd_disorder, diag_personality_disorder, diag_ocd_disorder, 
        diag_ptsd_disorder, diag_stress_disorder, diag_did_disorder, 
        diag_sud_disorder, diag_addiction_disorder, diag_other_disorder,
        sep = "", na.rm = TRUE) %>% 
  unite("possible_disorder",  possible_anxiety_disorder, possible_mood_disorder, possible_psychotic_disorder,
        possible_eating_disorder, possible_adhd_disorder, possible_personality_disorder, 
        possible_ocd_disorder, possible_ptsd_disorder, possible_stress_disorder, 
        possible_did_disorder,possible_sud_disorder, possible_addiction_disorder, 
        possible_other_disorder, sep = "", na.rm = TRUE) %>% 
  unite("past_disorder", past_anxiety_disorder, past_mood_disorder, 
        past_psychotic_disorder, past_eating_disorder, past_adhd_disorder, 
        past_personality_disorder, past_ocd_disorder, past_ptsd_disorder, 
        past_stress_disorder, past_did_disorder, past_sud_disorder, 
        past_addiction_disorder, past_other_disorder, sep = "", na.rm = TRUE) %>% 
  unite("race", race, race_other, na.rm = TRUE)

#correct data types#
str(osmi_unite)
osmi_str <- osmi_unite %>% 
  mutate(self_employed = as.logical(self_employed),
         tech_company = as.logical(tech_company),
         tech_role_primary = as.logical(tech_role_primary),
         mh_employer_discuss_personal = as.logical(mh_employer_discuss_personal),
         mh_coworker_discuss_personal = as.logical(mh_coworker_discuss_personal),
         mh_coworker_discuss_personal_2 = as.logical(mh_coworker_discuss_personal_2),
         mh_personal_coverage = as.logical(mh_personal_coverage),
         prev_employer = as.logical(prev_employer),
         prev_employer_tech_company = as.logical(prev_employer_tech_company),
         mh_prev_employer_discuss_personal = as.logical(mh_prev_employer_discuss_personal),
         mh_prev_coworker_discuss_personal = as.logical(mh_prev_coworker_discuss_personal),
         mh_prev_coworker_discuss_personal_2 = as.logical(mh_prev_coworker_discuss_personal_2),
         mh_disorder_treat = as.logical(mh_disorder_treat),
         mh_openid = as.logical(mh_openid),
         mh_openid_outcome = as.logical(mh_openid_outcome),
         mh_observe_good_rx_description = as.character(mh_observe_good_rx_description))
view(osmi_str)  

#check for discrepancies in country_work vs country_res#
osmi_work_res_diff <- osmi_str %>% 
  filter(country_res != country_work)
view(osmi_work_res_diff) #respondents not in us, can ignore#

#filter for us respondents only#
osmi_us <- osmi_str %>% 
  filter(country_res == "United States of America",
         tech_company == TRUE)
view(osmi_us)

#plot current disorder prevalence#
osmi_diag <- osmi_us %>% 
  drop_na(mh_current_diag) %>% 
  ggplot(aes(x = mh_current_diag, y = after_stat(100*count/sum(count)))) +
  geom_bar(fill = 4) + 
  labs(x = "Diagnosed with MH Disorder", y = "Percentage", title = "Presently Diagnosed with Mental Health Disorder")
osmi_diag

#plot past disorder prevalence#
osmi_diag_past <- osmi_us %>% 
  drop_na(mh_diag_history) %>% 
  ggplot(aes(x = mh_diag_history, y = after_stat(100*count/sum(count)))) +
  geom_bar(fill = 4) +
  labs(x = "Had MH Disorder", y = "Percentage", title = "Previously Diagnosed with Mental Health Disorder")
osmi_diag_past

#plot classification of past diagnoses#
osmi_diag_type <- osmi_us %>% 
  drop_na(past_disorder) %>% 
  distinct(past_disorder) %>% 
  mutate(past_disorder_char = nchar(past_disorder)) %>% 
  arrange(past_disorder_char) 
view(osmi_diag_type)
  
osmi_diag_type_graph <- osmi_us %>%
  drop_na(past_disorder) %>% 
  filter(past_disorder != "") %>% 
  mutate(past_disorder = case_when(
    nchar(past_disorder) > 56 ~ "Comorbid/Multiple Disorders",
    nchar(past_disorder) == 25 ~ "Stress Response Syndromes",
    nchar(past_disorder) == 49 ~ "Mood Disorder (Depression, Bipolar Disorder, etc)",
    nchar(past_disorder) == 56 ~ "Psychotic Disorder (Schizophrenia, Schizoaffective, etc)")) %>% 
  ggplot(aes(x = "", y = after_stat(100*count/sum(count)), fill = past_disorder)) +
  geom_bar() +
  geom_text(aes(label = paste0(round(after_stat(100*count/sum(count)), 2), "%")), 
            position = position_stack(vjust = 0.5), 
            stat = "count") +
  coord_polar(theta = "y") + 
  theme(axis.ticks = element_blank(), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(fill = "", title = "Past Mental Health Disorder History")
osmi_diag_type_graph

#plot whether MH disorder treated#
osmi_treated <- osmi_us %>% 
  drop_na(past_disorder) %>% 
  filter(past_disorder != "") %>% 
  ggplot(aes(x = "", y = after_stat(100*count/sum(count)), fill = mh_disorder_treat)) +
  geom_bar() +
  geom_text(aes(label = paste0(round(after_stat(100*count/sum(count)), 2), "%")), 
            position = position_stack(vjust = 0.5), 
            stat = "count") +
  scale_fill_discrete(label = "Yes") +
  coord_polar(theta = "y") + 
  theme(axis.ticks = element_blank(), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(fill = "", title = "Whether Mental Health Disorder was Treated")
osmi_treated

#plot all graphs together#
grid.arrange(osmi_diag, osmi_diag_past, 
             osmi_diag_type_graph, osmi_treated,
             ncol = 2)
