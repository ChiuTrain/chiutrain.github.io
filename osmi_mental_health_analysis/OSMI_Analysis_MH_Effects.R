#setup#

library(tidyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(skimr)
library(janitor)
library(gridExtra)
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

#investigate perceived impacts of mh on work performance etc#
osmi_mh_effects <- osmi_us %>% 
  select(21, 56:58, 62, 64:68, 70, 72)
view(osmi_mh_effects)

#plot disruption when treated mh disorder#
osmi_dis_treat <- osmi_mh_effects %>% 
  filter(mh_work_disrupt_treated != "Not applicable to me") %>% 
  ggplot(aes(x = "", y = after_stat(100*count/sum(count)), fill = mh_work_disrupt_treated)) +
  geom_bar() +
  geom_text(aes(label = paste0(round(after_stat(100*count/sum(count)), 2), "%")), 
            position = position_stack(vjust = 0.5), 
            stat = "count") +
  coord_polar(theta = "y") +
  theme(axis.ticks = element_blank(), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(fill = "", title = "Mental Health Disorder Perceived to Interfere with Work", subtitle = "(When Under Treatment)")
osmi_dis_treat

#plot disruption when untreated mh disorder#
osmi_dis_untreat <- osmi_mh_effects %>% 
  filter(mh_work_disrupt_untreated != "Not applicable to me") %>% 
  ggplot(aes(x = "", y = after_stat(100*count/sum(count)), fill = mh_work_disrupt_untreated)) +
  geom_bar() +
  geom_text(aes(label = paste0(round(after_stat(100*count/sum(count)), 2), "%")), 
            position = position_stack(vjust = 0.5), 
            stat = "count") +
  coord_polar(theta = "y") +
  theme(axis.ticks = element_blank(), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(fill = "", title = "Mental Health Disorder Perceived to Interfere with Work", 
       subtitle = "(When Not Treated)")
osmi_dis_untreat

#plot openid and outcome#
osmi_openid <- osmi_mh_effects %>% 
  ggplot(aes(x = mh_openid, y = after_stat(100*count/sum(count)), fill = mh_openid_outcome)) +
  geom_bar() +
  labs(fill = "", x = "Openly Identified with MH Disorder", y = "Percentage", 
       title = "Being Publicly Identified with Mental Health Disorder Impacts Career") +
  scale_x_discrete(labels = c("No", "Yes")) +
  scale_fill_discrete(labels = c("No", "Yes", "Not Applicable"))
osmi_openid

#plot perceived rx to openid#
osmi_per_rx <- osmi_mh_effects %>% 
  drop_na(mh_perceived_rx) %>% 
  mutate(mh_perceived_rx = as.integer(mh_perceived_rx)) %>% 
  ggplot(aes(x = mh_perceived_rx, y = after_stat(100*count/sum(count)))) +
  geom_bar(fill = 4) +
  labs(x = "Perceived Hypothetical Reaction", y = "Percentage", 
       subtitle = "Higher Values Indicate More Positive Reaction", 
       title = "Coworker's Reaction to Respondent Having Mental Health Disorder") +
  scale_x_continuous(breaks = c(0, 5, 10)) +
  expand_limits(x = 0)
osmi_per_rx

#plot perceived industry support#
osmi_per_ind_sup <- osmi_mh_effects %>% 
  drop_na(mh_perceived_industry_support) %>% 
  ggplot(aes(x = mh_perceived_industry_support, y = after_stat(100*count/sum(count)))) +
  geom_bar(fill = 4) +
  labs(x = "Perceived Support", y = "Percentage", 
       title = "Perceived Mental Health Support in Industry", 
       subtitle = "Higher Values Indicate Greater Support")
osmi_per_ind_sup

#plot instances of badly handled mh situ#
osmi_bad_rx <- osmi_mh_effects %>% 
  drop_na(mh_observe_bad_rx) %>% 
  ggplot(aes(x = "", y = after_stat(100*count/sum(count)), fill = mh_observe_bad_rx)) +
  geom_bar() +
  geom_text(aes(label = paste0(round(after_stat(100*count/sum(count)), 2), "%")), 
            position = position_stack(vjust = 0.5), 
            stat = "count") +
  coord_polar(theta = "y") +
  theme(axis.ticks = element_blank(), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(fill = "", title = "Respondent Witnessed Badly Handled Mental Health Issue")
osmi_bad_rx

#plot instances of well handled mh situ#
osmi_good_rx <- osmi_mh_effects %>% 
  drop_na(mh_observe_good_rx) %>% 
  ggplot(aes(x = "", y = after_stat(100*count/sum(count)), fill = mh_observe_good_rx)) +
  geom_bar() +
  geom_text(aes(label = paste0(round(after_stat(100*count/sum(count)), 2), "%")), 
            position = position_stack(vjust = 0.5), 
            stat = "count") +
  coord_polar(theta = "y") +
  theme(axis.ticks = element_blank(), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(fill = "", title = "Respondent Witnessed Well Handled Mental Health Issue")
osmi_good_rx

#plot perceived employer mh importance#
osmi_emp_mh_imp <- osmi_mh_effects %>% 
  drop_na(mh_employer_importance) %>% 
  ggplot(aes(x = mh_employer_importance, y = after_stat(100*count/sum(count)))) +
  geom_bar(fill = 4) +
  labs(x = "Employer Values MH", y = "Percentage", 
       subtitle = "Higher Values Indicate Greater Importance", title = "Employer Perceived to Value Mental Health") +
  scale_x_continuous(breaks = c(0, 5, 10)) +
  expand_limits(x = c(0, 10))
osmi_emp_mh_imp

#plot all graphs together#
grid.arrange(osmi_dis_treat, osmi_dis_untreat, osmi_openid, 
             osmi_per_rx, osmi_bad_rx, osmi_good_rx, 
             osmi_emp_mh_imp, osmi_per_ind_sup,
             ncol = 2)