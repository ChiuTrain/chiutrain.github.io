#setup#

library(tidyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
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

#removing irrelevant columns, filter for tech companies only#
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
str(osmi_unite, list.len = 6)
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
  select(country_res, country_work) %>% 
  filter(country_res != country_work)
view(osmi_work_res_diff) #respondents not in us, can ignore#

#filter for us respondents and tech companies only#
osmi_us <- osmi_str %>% 
  filter(country_res == "United States of America",
         tech_company == TRUE)
view(osmi_us)

#demographic stats#
osmi_demo <- osmi_us %>% 
  select("self_employed", "company_size", "tech_company", 
         "tech_role_primary", "age", "gender", 
         "us_state_res", "race", 
         "us_state_work", "covid_diag")
view(osmi_demo)

#plot self employed status#
osmi_self <- osmi_demo %>% 
  ggplot(aes(x = "", y = after_stat(100*count/sum(count)), fill = self_employed)) +
  geom_bar() +
  geom_text(aes(label = paste0(after_stat(100*count/sum(count)), "%")), 
            position = position_stack(vjust = 0.5), 
            stat = "count") +
  coord_polar(theta = "y") + 
  theme(axis.ticks = element_blank(), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(fill = "", title = "Self-Employed Status") +
  scale_fill_discrete(labels = "No")
osmi_self
  
#plot company size#
osmi_demo$company_size <- replace(osmi_demo$company_size, 
                                  osmi_demo$company_size == "05-Jan", 
                                  "1-5")
osmi_demo$company_size <- replace(osmi_demo$company_size, 
                                  osmi_demo$company_size == "25-Jun", 
                                  "6-25")

osmi_comp_size <- osmi_demo %>% 
  ggplot(aes(x = "", y = after_stat(100*count/sum(count)), fill = company_size)) +
  geom_bar() + 
  geom_text(aes(label = paste0(round(after_stat(100*count/sum(count)), 2), "%")), 
            position = position_stack(vjust = 0.5), 
            stat = "count") +
  coord_polar(theta = "y") + 
  theme(axis.ticks = element_blank(), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(fill = "", title = "Respondent's Company Size") +
  scale_fill_discrete(labels = c("1-5", "100-500", "26-100", 
                                 "500-1000", "6-25", "More than 1000", "No Answer"))
osmi_comp_size

#plot tech role primary#
osmi_tech_role <- osmi_demo %>% 
  ggplot(aes(x = "", y = after_stat(100*count/sum(count)), fill = tech_role_primary)) +
  geom_bar() +
  geom_text(aes(label = paste0(round(after_stat(100*count/sum(count)), 2), "%")), 
            position = position_stack(vjust = 0.5), 
            stat = "count") +
  coord_polar(theta = "y") + 
  theme(axis.ticks = element_blank(), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(fill = "Respondent's Job Type") +
  scale_fill_discrete(labels = c("Non-Tech", "Tech"))
osmi_tech_role

#plot age#
osmi_demo$age <- replace(osmi_demo$age, 
                         osmi_demo$age == 223,
                         23)

osmi_age <- osmi_demo %>% 
  ggplot(aes(x = age, y = after_stat(100*count/sum(count)))) +
  geom_histogram(color = 4, fill = 5, binwidth = 5) +
  labs(x = "Age", y = "Percentage", title = "Age Distribution of Respondents")
osmi_age

#plot gender#
osmi_distinct <- osmi_demo %>% 
  distinct(gender)

osmi_demo$gender <- replace(osmi_demo$gender,
                            osmi_demo$gender %in% c("female", "Female (cis)", "woman", "F"),
                            "Female") 
osmi_demo$gender <- replace(osmi_demo$gender,
                            osmi_demo$gender %in% c("male","male/he/him", "cis-het male", "M"),
                            "Male")
osmi_demo$gender <- replace(osmi_demo$gender,
                            osmi_demo$gender %in% c("afab non-binary","Non-binary/Agender"),
                            "Non-Binary")

osmi_gender <- osmi_demo %>% 
  ggplot(aes(x = "", y = after_stat(100*count/sum(count)), fill = gender)) +
  geom_bar() +
  geom_text(aes(label = paste0(round(after_stat(100*count/sum(count)), 2), "%")), 
            position = position_stack(vjust = 0.5), 
            stat = "count") +
  coord_polar(theta = "y") + 
  theme(axis.ticks = element_blank(), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(fill = "", title = "Gender") +
  scale_fill_discrete(labels = c("Female", "Male", "Non-Binary", "No Answer"))
osmi_gender

#plot race#
osmi_race <- osmi_demo %>% 
  ggplot(aes(x = "", y = after_stat(100*count/sum(count)), fill = race)) +
  geom_bar() +
  geom_text(aes(label = paste0(round(after_stat(100*count/sum(count)), 2), "%")), 
            position = position_stack(vjust = 0.5), 
            stat = "count") +
  coord_polar(theta = "y") + 
  theme(axis.ticks = element_blank(), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(fill = "", title = "Race") +
  scale_fill_discrete(labels = c("Asian", "Multi-racial (unspecified)", "White"))
osmi_race

#plot state of res#
osmi_state <- osmi_demo %>% 
  ggplot(aes(x = us_state_res, y = after_stat(100*count/sum(count)))) +
  geom_bar(fill = 4) +
  coord_flip() +
  labs(x = "State", y = "Percentage", title = "Respondents' State of Residence")
osmi_state
  
#plot all graphs together#
grid.arrange(osmi_self, osmi_comp_size, osmi_tech_role, 
             osmi_age, osmi_gender, osmi_race, 
             osmi_state,
             ncol = 3)

view(osmi_us)
