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

##investigate relationships between variables##
#investigate workplace conditions vs comfort discussing (supervisor)#
osmi_comfort <- osmi_us %>% 
  select(5:13, 15:16, 18) %>% 
  drop_na(comfort_supervisor, comfort_coworker)

#coverage vs com_sup#
osmi_comfort$mh_coverage <- factor(osmi_comfort$mh_coverage, order = TRUE, 
                                   levels = c("Not eligible for coverage / NA", "No", "I don't know", "Yes"))

osmi_cov_sup <- osmi_comfort %>% 
  drop_na(mh_coverage) %>% 
  ggplot(aes(x = mh_coverage, y = after_stat(100*count/sum(count)), fill = comfort_supervisor)) +
  geom_bar() +
  labs(x = "MH Coverage", y = "Percentage", fill = "", title = "Comfort Discussing MH with Supervisor")
osmi_cov_sup

#mh options vs com_sup#
osmi_opt_sup <- osmi_comfort %>% 
  drop_na(mh_options_aware) %>% 
  ggplot(aes(x = mh_options_aware, y = after_stat(100*count/sum(count)), fill = comfort_supervisor)) +
  geom_bar() +
  labs(x = "Aware of MH Options", y = "Percentage", fill = "", 
       title = "Comfort Discussing MH with Supervisor")
osmi_opt_sup

#mh employer discussed mh vs com_sup#
osmi_empdis_sup <- osmi_comfort %>% 
  drop_na(mh_employer_discuss) %>% 
  ggplot(aes(x = mh_employer_discuss, y = after_stat(100*count/sum(count)), fill = comfort_supervisor)) +
  geom_bar() +
  labs(x = "Employer Discussed MH Formally", y = "Percentage", fill = "", 
       title = "Comfort Discussing MH with Supervisor")
osmi_empdis_sup

#mh resource offered vs com_sup#
osmi_resoff_sup <- osmi_comfort %>% 
  drop_na(mh_resource_employer_offer) %>% 
  ggplot(aes(x = mh_resource_employer_offer, y = after_stat(100*count/sum(count)), fill = comfort_supervisor)) +
  geom_bar() +
  labs(x = "Employer Offered MH Resources", y = "Percentage", fill = "", title = "Comfort Discussing MH with Supervisor")
osmi_resoff_sup

#anon protection vs com_sup#
osmi_anon_sup <- osmi_comfort %>% 
  drop_na(anon_protect) %>% 
  ggplot(aes(x = anon_protect, y = after_stat(100*count/sum(count)), fill = comfort_supervisor)) +
  geom_bar() +
  labs(x = "MH Resources Anonymity Protected", y = "Percentage", fill = "", 
       title = "Comfort Discussing MH with Supervisor")
osmi_anon_sup

#mh leave diff vs com_sup#
osmi_comfort$mh_leave_difficulty <- factor(osmi_comfort$mh_leave_difficulty, order = TRUE, 
                                           levels = c("Very easy", "Somewhat easy", "I don't know", 
                                                      "Neither easy nor difficult", "Somewhat difficult", "Difficult"))
osmi_lea_sup <- osmi_comfort %>% 
  drop_na(mh_leave_difficulty) %>% 
  ggplot(aes(x = mh_leave_difficulty, y = after_stat(100*count/sum(count)), fill = comfort_supervisor)) +
  geom_bar() +
  labs(x = "Difficulty Taking MH Leave", y = "Percentage", fill = "", title = "Comfort Discussing MH with Supervisor")
osmi_lea_sup

#plot all graphs together#
grid.arrange(osmi_cov_sup, osmi_opt_sup, osmi_empdis_sup, 
             osmi_resoff_sup, osmi_anon_sup, osmi_lea_sup,
             ncol = 2)