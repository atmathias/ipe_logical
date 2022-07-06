
library(tidyverse)
library(lubridate)
library(glue)
library(openxlsx)

source("R/list_function.R")

# Load data

df_ipe_logical_data <- readxl::read_excel("inputs/Individual_Profiling_Exercise_-_Questionnaire_for_Sampled_Households_shared_2022-06-27.xlsx") %>%
    mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = enumerator_id,
         i.check.settlement_name = settlement,
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
  mutate(i.check.type = "change_response",
         i.check.name = "relation_to_hoh",
         i.check.current_value = relation_to_hoh,
         i.check.value = "",
         i.check.issue_id = "logic_issue_relation_to_hoh",
         i.check.issue = glue("relation_to_hoh: {relation_to_hoh}, but respondent stays alone"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_relation_to_hoh")


# If response to "live_in_house" is no, survey needs to be checked

df_live_in_house <- df_ipe_logical_data %>% 
  filter(live_in_house == "no") %>%
  mutate(i.check.type = NA,
         i.check.name = "live_in_house",
         i.check.current_value = live_in_house,
         i.check.value = "",
         i.check.issue_id = "living_in_house",
         i.check.issue = "If respondent does not live in the house, then he/she should not answer for the household",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_live_in_house")


# If hh_size = 1 and respondent does not live in the house i.e. live_in_house = no, survey needs to be checked

df_live_in_house_and_hh_size <- df_ipe_logical_data %>% 
  filter(live_in_house %in% c("no") , hh_size == 1) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "live_in_house",
         i.check.current_value = live_in_house,
         i.check.value = "",
         i.check.issue_id = "hh_size_and_live_in_house",
         i.check.issue = glue("hh_size: {hh_size}, but respondent does not live in the house i.e. live_in_house = no"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_live_in_house_and_hh_size")


# If respondent answered to "How long have you lived in the house?" i.e  long_live_in_house = ‘more_than_two_years’ or ‘one_to_two_years’ and 
# food_aid_assistance = ‘no’, survey should be checked

df_food_aid_assistance <- df_ipe_logical_data %>% 
  filter(long_live_in_house %in% c("more_than_two_years", "one_to_two_years"), food_aid_assistance == "no") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "food_aid_assistance",
         i.check.current_value = food_aid_assistance,
         i.check.value = "",
         i.check.issue_id = "logic_issue_food_aid_assistance",
         i.check.issue = glue("long_live_in_house: {long_live_in_house}, but food_aid_assistance: {food_aid_assistance}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Needs confirmation from the field", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_food_aid_assistance")


# If respondent answered to "How long have you lived in the house?" i.e  long_live_in_house = ‘more_than_two_years’ or ‘one_to_two_years’ and 
# receive_nfi = ‘no_i_have_never_received_nfis’, survey should be checked

df_receive_nfi <- df_ipe_logical_data %>% 
  filter(long_live_in_house %in% c("more_than_two_years", "one_to_two_years"), receive_nfi == "no_i_have_never_received_nfis") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "receive_nfi",
         i.check.current_value = receive_nfi,
         i.check.value = "",
         i.check.issue_id = "logic_issue_receive_nfi",
         i.check.issue = glue("long_live_in_house: {long_live_in_house}, but receive_nfi: {receive_nfi}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Needs confirmation from the field", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_receive_nfi")


# If condiments = 0 i.e. household has not eaten salt, spices, tea, or coffee in the past seven days, surveys should be checked
df_condiments_fcs <- df_ipe_logical_data %>% 
  filter(condiments_fcs == 0) %>%
  mutate(i.check.type = NA,
         i.check.name = "condiments_fcs",
         i.check.current_value = condiments_fcs,
         i.check.value = "",
         i.check.issue_id = "eating_condiments",
         i.check.issue = glue("condiments_fcs: {condiments_fcs}, it's unlikely that a household will spend 7 days eating food without salt"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Enumerators should pay attention to salt in particular", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))


add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_condiments_fcs")


# If respondent answered to "What is your current main water source for drinking/cooking?" i.e  main_water_source = water_piped_into_the_dwellingplot and 
# walking_dist_drinking_water_source = 'btn_201_and_500m' or 'btn_501m_and_1km' or 'greater_than_1km', survey should be checked

df_walking_dist_drinking_water_source <- df_ipe_logical_data %>% 
  filter(walking_dist_drinking_water_source %in% c("btn_201_and_500m", "btn_501m_and_1km", "greater_than_1km"), 
         main_water_source == "water_piped_into_the_dwellingplot") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "walking_dist_drinking_water_source",
         i.check.current_value = walking_dist_drinking_water_source,
         i.check.value = "",
         i.check.issue_id = "logic_issue_walking_dist_drinking_water_source",
         i.check.issue = glue("main_water_source: {main_water_source}, but walking_dist_drinking_water_source: {walking_dist_drinking_water_source}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Needs confirmation from the field", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_walking_dist_drinking_water_source")


# If respondent answered to "How many trips did you make using the containers?" i.e. number_of_trips_for_each_container > '0' 
# and total amount of water collected i.e. calc_total_volume = '0', survey should be checked

df_calc_total_volume <- df_ipe_logical_data %>% 
  filter(calc_total_volume == 0 , number_of_trips_for_each_container > 0) %>%
  mutate(i.check.type = NA,
         i.check.name = "calc_total_volume",
         i.check.current_value = calc_total_volume,
         i.check.value = "",
         i.check.issue_id = "water_amount_collected",
         i.check.issue = glue("calc_total_volume: {calc_total_volume}, but number_of_trips_for_each_container: {number_of_trips_for_each_container}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Enumerator should pay attention to the number of trips made for all water containers", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_calc_total_volume")


# If most_important_sources_of_earnings = 'none' and process_and_sell_any_agricultural_by_products or own_a_trading_business or 
# own_a_professional_office or  drive_a_household_owned_taxi_bodaboda or own_a_bar_or_restaurant or 
# own_any_other_non_agricultural_business = 'yes', survey should be checked

df_most_important_sources_of_earnings <- df_ipe_logical_data %>% 
  filter(process_and_sell_any_agricultural_by_products %in% c("yes") | own_a_trading_business %in% c("yes") | 
         own_a_professional_office %in% c("yes") | drive_a_household_owned_taxi_bodaboda %in% c("yes") |  own_a_bar_or_restaurant %in% c("yes")|
         own_any_other_non_agricultural_business %in% c("yes"), most_important_sources_of_earnings == "none") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "most_important_sources_of_earnings",
         i.check.current_value = most_important_sources_of_earnings,
         i.check.value = "",
         i.check.issue_id = "logic_issue_most_important_sources_of_earnings",
         i.check.issue = "Household member has an economic activity yet source of earning is none",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Enumerator should pay attention to the economic activities done by hh members", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_most_important_sources_of_earnings")


# If Respondent stays alone i.e. hh_size = 1, spends (exp_food) 50,000 shillings or more on food 
# but for more than 5 days (no_fd_frequency) in a month has no food

# df_no_fd_frequency <- df_ipe_logical_data %>% 
  # filter(exp_food > 50000, no_fd_frequency %in% c("often"), hh_size == 1) %>%
  # mutate(i.check.type = NA,
         # i.check.name = "no_fd_frequency",
         # i.check.current_value = no_fd_frequency,
         # i.check.value = "",
         # i.check.issue_id = "logic_issue_no_fd_frequency",
         # i.check.issue = glue("hh_size: {hh_size}, exp_food: {exp_food}, but no_fd_frequency: {no_fd_frequency} has no food to eat at all" ),
         # i.check.other_text = "",
         # i.check.checked_by = "",
         # i.check.checked_date = as_date(today()),
         # i.check.comment = "Enumerator should pay attention", 
         # i.check.reviewed = "",
         # i.check.adjust_log = "",
         # i.check.uuid_cl = "",
         # i.check.so_sm_choices = "") %>% 
  # dplyr::select(starts_with("i.check.")) %>% 
  # rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_no_fd_frequency")


# If pulses_fcs = 0 i.e. household has not eaten beans in the past seven days, surveys should be checked
df_pulses_fcs <- df_ipe_logical_data %>% 
  filter(pulses_fcs == 0) %>%
  mutate(i.check.type = NA,
         i.check.name = "pulses_fcs",
         i.check.current_value = pulses_fcs,
         i.check.value = "",
         i.check.issue_id = "eating_pulses",
         i.check.issue = "It's unlikely that hh members will spend 7 days without eating any beans/peas/sim sim/g.nuts etc.",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Enumerators should not only look at beans but also other nuts", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_pulses_fcs")

ipe_combined_checks <- bind_rows(logic_output) %>% 
  mutate(int.name = ifelse(str_detect(string = name, pattern = "_rank_.*"), str_replace(string = name, pattern = "_rank_.*", replacement = ""), name)) %>% 
  left_join(df_ipe_survey %>% select(name, label), by = c("int.name" = "name")) %>% 
  select(-int.name) %>% 
  relocate(label, .after = name)

write_csv(x = ipe_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_logical_checks.csv"), na = "")











