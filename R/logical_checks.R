
library(tidyverse)
library(lubridate)
library(glue)
library(openxlsx)

source("R/list_function.R")

# Load data

df_ipe_logical_data <- readxl::read_excel("inputs/Individual_Profiling_Exercise_-_Questionnaire_for_Sampled_Households_shared_2022-06-15.xlsx") %>% 
  mutate(m.uuid = '_uuid',
         m.start_date = as_date(start),
         m.enumerator_id = Enumerator,
         m.settlement_name = settlement,
         start = as_datetime(start),
         end = as_datetime(end)
         
  )

# Load survey questions sheet
df_ipe_survey <- readxl::read_excel("inputs/Individual_Profiling_Exercise_Tool.xlsx", sheet = "survey")

# Load survey choices sheet
df_ipe_choices <- readxl::read_excel("inputs/Individual_Profiling_Exercise_Tool.xlsx", sheet = "choices")

# Logical flow

logic_output <- list()

# If "hh_size" = 1 and response to relation to household head "relation_to_hoh" is not "head_of_household", survey needs to be checked

df_relation_to_hoh <- df_ipe_logical_data %>% 
  filter(relation_to_hoh %in% c("husband", "wife", "son", "daughter", "sister", "brother", "mother", "father", "grandmother", 
  "grandmother", "uncle", "aunt", "grandson", "mother_in_law", "father_in_law", "son_in_law", "bother_in_law", "sister_in_law", 
  "niece", "nephew", "granddaughter", "cousin_female", "cousin_male", "half_brother", "half_sister", "foster_child",
  "no_blood_relation", "other_blood_relation") , hh_size == 1) %>%
  mutate(m.type = "change_response",
         m.name = "relation_to_hoh",
         m.current_value = relation_to_hoh,
         m.value = "",
         m.issue_id = "un_expected_response",
         m.issue = "hh_size = 1 but relation_to_hoh is not: head_of_household",
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = "",
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_relation_to_hoh")


# If response to "live_in_house" is no, survey needs to be checked

df_live_in_house <- df_ipe_logical_data %>% 
  filter(live_in_house == "no") %>%
  mutate(m.type = NA,
         m.name = "live_in_house",
         m.current_value = live_in_house,
         m.value = "",
         m.issue_id = "un_expected_response",
         m.issue = "If respondent does not live in the house, then should not answer for household",
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = "",
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_live_in_house")


# If hh_size = 1 and respondent does not live in the house i.e. live_in_house = no, survey needs to be checked

df_live_in_house_and_hh_size <- df_ipe_logical_data %>% 
  filter(live_in_house %in% c("no") , hh_size == 1) %>%
  mutate(m.type = "change_response",
         m.name = "live_in_house",
         m.current_value = live_in_house,
         m.value = "",
         m.issue_id = "un_expected_response",
         m.issue = "hh_size = 1 but respondent does not live in the house i.e. live_in_house = no",
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = "",
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_live_in_house_and_hh_size")


# If respondent answered to "How long have you lived in the house?" i.e  long_live_in_house = ‘more_than_two_years’ or ‘one_to_two_years’ and 
# food_aid_assistance = ‘no’, survey should be checked

df_food_aid_assistance <- df_ipe_logical_data %>% 
  filter(long_live_in_house %in% c("more_than_two_years", "one_to_two_years"), food_aid_assistance == "no") %>%
  mutate(m.type = "change_response",
         m.name = "food_aid_assistance",
         m.current_value = food_aid_assistance,
         m.value = "",
         m.issue_id = "logic_issue_food_aid_assistance",
         m.issue = glue("long_live_in_house: {long_live_in_house}, but food_aid_assistance: {food_aid_assistance}"),
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "Needs confirmation from the field", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = "",
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_food_aid_assistance")


# If respondent answered to "How long have you lived in the house?" i.e  long_live_in_house = ‘more_than_two_years’ or ‘one_to_two_years’ and 
# receive_nfi = ‘no_i_have_never_received_nfis’, survey should be checked

df_receive_nfi <- df_ipe_logical_data %>% 
  filter(long_live_in_house %in% c("more_than_two_years", "one_to_two_years"), receive_nfi == "no_i_have_never_received_nfis") %>%
  mutate(m.type = "change_response",
         m.name = "receive_nfi",
         m.current_value = receive_nfi,
         m.value = "",
         m.issue_id = "logic_issue_receive_nfi",
         m.issue = glue("long_live_in_house: {long_live_in_house}, but receive_nfi: {receive_nfi}"),
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "Needs confirmation from the field", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = "",
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_receive_nfi")


# If condiments = 0 i.e. household has not eaten salt, spices, tea, or coffee in the past seven days, surveys should be checked
df_condiments_fcs <- df_ipe_logical_data %>% 
  filter(condiments_fcs == 0) %>%
  mutate(m.type = NA,
         m.name = "condiments_fcs",
         m.current_value = condiments_fcs,
         m.value = "",
         m.issue_id = "un_expected_response",
         m.issue = "It's unlikely that a household will spend 7 days eating food without salt",
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = "",
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_condiments_fcs")


# If respondent answered to "What is your current main water source for drinking/cooking?" i.e  main_water_source = water_piped_into_the_dwellingplot and 
# walking_dist_drinking_water_source = 'btn_201_and_500m' or 'btn_501m_and_1km' or 'greater_than_1km', survey should be checked

df_walking_dist_drinking_water_source <- df_ipe_logical_data %>% 
  filter(walking_dist_drinking_water_source %in% c("btn_201_and_500m", "btn_501m_and_1km", "greater_than_1km"), 
         main_water_source == "water_piped_into_the_dwellingplot") %>%
  mutate(m.type = "change_response",
         m.name = "walking_dist_drinking_water_source",
         m.current_value = walking_dist_drinking_water_source,
         m.value = "",
         m.issue_id = "logic_issue_walking_dist_drinking_water_source",
         m.issue = glue("main_water_source: {main_water_source}, but walking_dist_drinking_water_source: {walking_dist_drinking_water_source}"),
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "Needs confirmation from the field", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = "",
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_walking_dist_drinking_water_source")

view(df_walking_dist_drinking_water_source)


# If respondent answered to "How many trips did you make using the containers?" i.e. number_of_trips_for_each_container > '0' 
# and total amount of water collected i.e. calc_total_volume = '0', survey should be checked

df_calc_total_volume <- df_ipe_logical_data %>% 
  filter(calc_total_volume == 0 , number_of_trips_for_each_container > 0) %>%
  mutate(m.type = NA,
         m.name = "calc_total_volume",
         m.current_value = calc_total_volume,
         m.value = "",
         m.issue_id = "un_expected_response",
         m.issue = glue("number_of_trips_for_each_container: {number_of_trips_for_each_container}, but calc_total_volume: {calc_total_volume}"),
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "Enumerator should pay attention", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = "",
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_calc_total_volume")


# If most_important_sources_of_earnings = 'none' and process_and_sell_any_agricultural_by_products or own_a_trading_business or 
# own_a_professional_office or  drive_a_household_owned_taxi_bodaboda or own_a_bar_or_restaurant or 
# own_any_other_non_agricultural_business = 'yes', survey should be checked

df_most_important_sources_of_earnings <- df_ipe_logical_data %>% 
  filter(process_and_sell_any_agricultural_by_products %in% c("yes") | own_a_trading_business %in% c("yes") | 
         own_a_professional_office %in% c("yes") | drive_a_household_owned_taxi_bodaboda %in% c("yes") |  own_a_bar_or_restaurant %in% c("yes")|
         own_any_other_non_agricultural_business %in% c("yes"), most_important_sources_of_earnings == "none") %>%
  mutate(m.type = "change_response",
         m.name = "most_important_sources_of_earnings",
         m.current_value = most_important_sources_of_earnings,
         m.value = "yes",
         m.issue_id = "logic_issue_most_important_sources_of_earnings",
         m.issue = "Household member has an economic activity yet source of earning is none",
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "Enumerator should pay attention", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = "",
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_most_important_sources_of_earnings")

















