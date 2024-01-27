
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
  filter(!relation_to_hoh %in% c("head_of_household"), hh_size == 1) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "relation_to_hoh",
         i.check.current_value = as.character(relation_to_hoh),
         i.check.value = "head_of_household",
         i.check.issue_id = "logic_issue_relation_to_hoh",
         i.check.issue = glue("relation_to_hoh: {relation_to_hoh}, but respondent stays alone"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Response to change to 'head_of_household' since respondent lives alone", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_relation_to_hoh")

# If "hh_size" < (num_hh_age_6_12 + num_hh_age_13_16 + num_hh_age_to_18), survey needs to be checked
df_hh_size_less_than_child_hh_size <- df_ipe_logical_data %>% 
  mutate_at(c('num_hh_age_6_12', 'num_hh_age_13_16', 'num_hh_age_to_18'), as.numeric) %>% 
  rowwise() %>% 
  mutate(child_hh_size = sum(num_hh_age_6_12, num_hh_age_13_16, num_hh_age_to_18)) %>% 
  filter(hh_size <  child_hh_size) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "hh_size",
         i.check.current_value = as.character(hh_size),
         i.check.value = "",
         i.check.issue_id = "logic_issue_hh_size_less_than_child_hh_size",
         i.check.issue = glue("hh_size: {hh_size}, child_hh_size: {child_hh_size}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Check that all children in the progress hh roster are still part of the hh", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_size_less_than_child_hh_size")


# If response to "live_in_house" is no, survey needs to be checked
df_live_in_house <- df_ipe_logical_data %>% 
  filter(live_in_house %in% c("no")) %>%
  mutate(i.check.type = "remove_survey",
         i.check.name = "live_in_house",
         i.check.current_value = as.character(live_in_house),
         i.check.value = "",
         i.check.issue_id = "living_in_house",
         i.check.issue = "If respondent does not live in the house, then he/she should not answer for the household",
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Management to decide", 
         i.check.reviewed = "",
         i.check.adjust_log = ""
         ) %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_live_in_house")


# If hh_size = 1 and respondent does not live in the house i.e. live_in_house = no, survey needs to be checked
df_live_in_house_and_hh_size <- df_ipe_logical_data %>% 
  filter(live_in_house %in% c("no") , hh_size == 1) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "live_in_house",
         i.check.current_value = as.character(live_in_house),
         i.check.value = "",
         i.check.issue_id = "hh_size_and_live_in_house",
         i.check.issue = glue("hh_size: {hh_size}, but respondent does not live in the house i.e. live_in_house = no"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Keep data as it is", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_live_in_house_and_hh_size")


# If respondent answered to "How long have you lived in the house?" i.e  long_live_in_house = ‘more_than_two_years’ or ‘one_to_two_years’ and 
# food_aid_assistance = ‘no’, survey should be checked

df_food_aid_assistance <- df_ipe_logical_data %>% 
  filter(long_live_in_house %in% c("more_than_two_years", "one_to_two_years"), food_aid_assistance == "no") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "food_aid_assistance",
         i.check.current_value = as.character(food_aid_assistance),
         i.check.value = "",
         i.check.issue_id = "logic_issue_food_aid_assistance",
         i.check.issue = glue("long_live_in_house: {long_live_in_house}, but food_aid_assistance: {food_aid_assistance}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Keep data as it is, needs confirmation from the field", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_food_aid_assistance")


# If respondent answered to "How long have you lived in the house?" i.e  long_live_in_house = ‘more_than_two_years’ or ‘one_to_two_years’ and 
# receive_nfi = ‘no_i_have_never_received_nfis’, survey should be checked

df_receive_nfi <- df_ipe_logical_data %>% 
  filter(long_live_in_house %in% c("more_than_two_years", "one_to_two_years"), receive_nfi == "no_i_have_never_received_nfis") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "receive_nfi",
         i.check.current_value = as.character(receive_nfi),
         i.check.value = "",
         i.check.issue_id = "logic_issue_receive_nfi",
         i.check.issue = glue("long_live_in_house: {long_live_in_house}, but receive_nfi: {receive_nfi}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Keep data as it is, needs confirmation from the field", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_receive_nfi")

# If member_fell_sick = 'no' but child_diarrhoea = 'yes' or severe_diarrhoea = 'yes' or 
# child_cough = 'yes' or respiratory_infection = 'yes' or child_fever = 'yes' or child_skin_disease = 'yes'
# child_sick_symptoms = 'yes', surveys should be checked
df_hh_member_fell_sick <- df_ipe_logical_data %>% 
  mutate(child_fell_sick = ifelse(if_any(c(child_diarrhoea, severe_diarrhoea, child_cough, 
          respiratory_infection, child_fever, child_skin_disease, child_sick_symptoms), ~ .x %in% c("yes")),
          "yes", "no")) %>% 
  filter(member_fell_sick %in% c("no") &  child_fell_sick %in% c("yes")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "member_fell_sick",
         i.check.current_value =as.character(member_fell_sick),
         i.check.value = "yes",
         i.check.issue_id = "hh_member_fell_sick_but_not_reported",
         i.check.issue = glue("member_fell_sick: {member_fell_sick}, but child fell sick in past two weeks"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "change member_fell_sick to yes since child fell sick", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_member_fell_sick")


# If condiments = 0 i.e. household has not eaten salt, spices, tea, or coffee in the past seven days, surveys should be checked
df_condiments_fcs <- df_ipe_logical_data %>% 
  filter(condiments_fcs == 0) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "condiments_fcs",
         i.check.current_value =as.character(condiments_fcs),
         i.check.value = "",
         i.check.issue_id = "eating_condiments",
         i.check.issue = glue("condiments_fcs: {condiments_fcs}, it's unlikely that a household spends 7 days eating without salt"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "blank variable, most enumerators misinterpreted this question", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_condiments_fcs")


# If respondent answered to "What is your current main water source for drinking/cooking?" i.e  main_water_source = water_piped_into_the_dwellingplot and 
# walking_dist_drinking_water_source = 'btn_201_and_500m' or 'btn_501m_and_1km' or 'greater_than_1km', survey should be checked

df_walking_dist_drinking_water_source <- df_ipe_logical_data %>% 
  filter(walking_dist_drinking_water_source %in% c("btn_201_and_500m", "btn_501m_and_1km", "greater_than_1km"), 
         main_water_source %in% c("water_piped_into_the_dwellingplot")) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "walking_dist_drinking_water_source",
         i.check.current_value =as.character(walking_dist_drinking_water_source),
         i.check.value = "",
         i.check.issue_id = "logic_issue_walking_dist_drinking_water_source",
         i.check.issue = glue("main_water_source: {main_water_source}, but walking_dist_drinking_water_source: {walking_dist_drinking_water_source}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Keep data as it is", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_walking_dist_drinking_water_source")


# If respondent answered to "How many trips did you make using the containers?" i.e. number_of_trips_for_each_container > '0' 
# and total amount of water collected i.e. calc_total_volume = '0', survey should be checked

df_calc_total_volume <- df_ipe_logical_data %>% 
  filter(calc_total_volume == 0 , number_of_trips_for_each_container > 0) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "calc_total_volume",
         i.check.current_value =as.character(calc_total_volume),
         i.check.value = "",
         i.check.issue_id = "water_amount_collected",
         i.check.issue = glue("calc_total_volume: {calc_total_volume}, but number_of_trips_for_each_container: {number_of_trips_for_each_container}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Keep data as it is.", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_calc_total_volume")


# If most_important_sources_of_earnings = 'none' and process_and_sell_any_agricultural_by_products or own_a_trading_business or 
# own_a_professional_office or  drive_a_household_owned_taxi_bodaboda or own_a_bar_or_restaurant or 
# own_any_other_non_agricultural_business = 'yes', survey should be checked

df_most_important_sources_of_earnings <- df_ipe_logical_data %>% 
  filter(if_any(c(process_and_sell_any_agricultural_by_products, own_a_trading_business , own_a_professional_office ,
          drive_a_household_owned_taxi_bodaboda ,  own_a_bar_or_restaurant, 
          own_any_other_non_agricultural_business ), ~ .x %in% c("yes")) & most_important_sources_of_earnings %in% c("none")) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "most_important_sources_of_earnings",
         i.check.current_value = as.character(most_important_sources_of_earnings),
         i.check.value = "yes",
         i.check.issue_id = "logic_issue_most_important_sources_of_earnings",
         i.check.issue = glue("most_important_sources_of_earnings: {most_important_sources_of_earnings}, but hh has an economic activity"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "change most_important_sources_of_earnings to yes", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_most_important_sources_of_earnings")


# If exp_food = 0 and cereals_cash > 500 or tubers_cash > 500 or pulses_cash > 500 or fruits_vegetables_cash > 500
#  or fish_meat_poultry_cash > 500 oil_cash >= 500 or milk_products_cash >= 500 or sugar_salt_cash > = 500
#  or tea_coffee_cash >= 100 or matooke_cash => 500 or other_cash => 500, survey should be checked

df_exp_food_zero <- df_ipe_logical_data %>% 
  mutate_at(c("cereals_cash", "tubers_cash", "pulses_cash", "fruits_vegetables_cash", "fish_meat_poultry_cash",
              "oil_cash", "milk_products_cash", "sugar_salt_cash", "matooke_cash", "tea_coffee_cash", "other_cash"), as.numeric) %>% 
  mutate(total_exp_food_items = rowSums(dplyr::select(., cereals_cash, tubers_cash, pulses_cash, fruits_vegetables_cash, fish_meat_poultry_cash,
        oil_cash, milk_products_cash, sugar_salt_cash, matooke_cash, tea_coffee_cash, other_cash))) %>%
  relocate(total_exp_food_items, .after = other_cash) %>% 
  filter(exp_food == 0 & total_exp_food_items >= 300) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "exp_food",
         i.check.current_value =  as.character(exp_food),
         i.check.value =  as.character(total_exp_food_items),
         i.check.issue_id = "logic_issue_exp_food_zero",
         i.check.issue = glue("exp_food: {exp_food}, but hh bought food in food breakdown items"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "sum food type amount spent and replace with zero on exp_food", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_exp_food_zero")


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
         # i.check.checked_by = "MT",
         # i.check.checked_date = as_date(today()),
         # i.check.comment = "Enumerator should pay attention", 
         # i.check.reviewed = "",
         # i.check.adjust_log = "",
         # 
         # ) %>% 
  # dplyr::select(starts_with("i.check.")) %>% 
  # rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_no_fd_frequency")


# If pulses_fcs = 0 i.e. household has not eaten beans in the past seven days, surveys should be checked
df_pulses_fcs <- df_ipe_logical_data %>% 
  filter(pulses_fcs == 0) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "pulses_fcs",
         i.check.current_value = as.character(pulses_fcs),
         i.check.value = "",
         i.check.issue_id = "eating_pulses",
         i.check.issue = "It's unlikely that hh members will spend 7 days without eating any beans/peas/sim sim/g.nuts etc.",
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Keep data as it is", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

 add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_pulses_fcs")

 
 # If no_fd_frequency = 'never' and insufficient_fd = 'yes', surveys should be checked
 df_no_fd_frequency <- df_ipe_logical_data %>% 
   filter(no_fd_frequency %in% c("never") & insufficient_fd %in% c("yes")) %>%
   mutate(i.check.type = "change_response",
          i.check.name = "no_fd_frequency",
          i.check.current_value = as.character(no_fd_frequency),
          i.check.value = ifelse((no_fd_frequency %in% c("never") & days_insufficient_fd > 2), "often", "sometimes"),
          i.check.issue_id = "logic_issue_no_fd_frequency",
          i.check.issue = glue("no_fd_frequency: {no_fd_frequency}, but insufficient_fd: {insufficient_fd}"),
          i.check.other_text = "",
          i.check.checked_by = "MT",
          i.check.checked_date = as_date(today()),
          i.check.comment = "Change no_fd_frequency to often or rarely accordingly", 
          i.check.reviewed = "",
          i.check.adjust_log = "") %>% 
   dplyr::select(starts_with("i.check.")) %>% 
   rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
 
 add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_no_fd_frequency")
 
 ipe_combined_checks <- bind_rows(logic_output) %>% 
  mutate(int.name = ifelse(str_detect(string = name, pattern = "_rank_.*"), str_replace(string = name, pattern = "_rank_.*", replacement = ""), name)) %>% 
  left_join(df_ipe_survey %>% select(name, label), by = c("int.name" = "name")) %>% 
  select(-int.name) %>% 
  relocate(label, .after = name)

write_csv(x = ipe_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_logical_checks.csv"), na = "")











