# AUTHOR:   B. Betz | USAID
# PURPOSE:  Analyze young and old KP cascades versus middle aged
# REF ID:   0a230334 
# LICENSE:  MIT
# DATE:     2024-03-21
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
library(tidyverse)
library(gagglr)
library(stringr)
library(janitor)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "0a230334"

# IMPORT ------------------------------------------------------------------
  
## ID file paths
  
files <-  list.files(path = "Data") %>% 
  str_c("Data/", ., sep="") %>% #note that new pipes don't allow this function to run
  print()
#
## Read data  

df <- map_dfr(.x = files, read_csv) |> clean_names() |> 
  rename(population = all_target_populations,
         prev_disaggs = kp_pp_gp_prev_disaggs,
         fy = fiscal_year_short_name,
         q = fiscal_quarter ) |> 
  mutate(
    indicator = str_extract(data_element_short_name, ".+(?=\\s\\([k,K,G,P])|.+"),  
      indicator = recode(indicator,
                         "PrEP_CT:" = "PrEP_CT",
                         "PrEP_NEW (N)" = "PrEP_NEW"),
    pop = str_to_upper(
      str_extract(str_to_lower(data_element_short_name), "(?<=\\()pp|gp|k.+(?=\\))|^.{1}p")
                        ),
      pop = recode(pop, "KEYPOP" = "KP"),
    population_abr = case_match(population, "Female Sex Workers" ~ "FSW",
                                    "People in prisons and other closed settings" ~ "People in prison",
                                    .default = population),
    population = case_match(population,
                              "MSM" ~ "Men who have sex with men",
                              "PWID" ~ "People who inject drugs",
                              "Other KP" ~ population,
                            .default = str_to_sentence(population)),
    age_group_order = case_when(age_group_order_for_datim >= 914 ~ 913, #collapse 50+
                                age_group_order_for_datim == 1 ~ 2000, #reorder unknown to end
                                 .default = age_group_order_for_datim),
    age_bins_30_50 = case_when(
      age_group_order >  1 & age_group_order < 9 ~ "Young",
      age_group_order >= 9 & age_group_order < 913 ~ "Middle Age",
      age_group_order == 913 ~ "Older",
      age_group_order == 1 | age_group_order == 2000 ~ "Unknown Age",
      ),
    age_bins_25_50 = ifelse(age_group_order_for_datim == 8, "Middle Age", age_bins_30_50),
    
    age_group_y = case_when(all_age_groups == "<20 Years" ~ "18-20 Years",
                            all_age_groups == "Age Unknown" ~ "Unknown Age",
                            age_group_order == 913 ~ "50+ Years",
                            .default = all_age_groups),
    age_group = str_remove_all(age_group_y, " Years"),
    
        ) |> 
  relocate(pop, .before = population) 
  # filter(pop == "KP") |> 
  # filter(is.na(indicator)) |> 
  # count(age_group_order_for_datim , all_age_groups, age)
  # count(indicator) |> print(n=21)




hts <- df |> filter(indicator=="HTS_TST") |> 
  mutate(indicator = str_c(indicator, 
                           str_to_upper(
                             str_extract(hiv_result_status, "^.{3}")
                                        ),
                           sep = "_")) 

kp_prev <- df |> filter(indicator=="KP_PREV") |> 
  mutate(indicator = str_c(indicator, str_extract(prev_disaggs, "(?<=\\:\\s).+"), sep = ": "))

## establish cascades
prevention_cascade <- c("KP_PREV", "KP_PREV: Tested or Referred", "HTS_SELF", "HTS_TST", "HTS_TST_NEG", "PrEP_OFFER", "PrEP_NEW", "PrEP_NEW_VERIFY", "PrEP_CT", "PrEP_CT_VERIFY")
treatment_cascade <- c( "HTS_TST_POS", "TX_NEW", "TX_NEW_VERIFY", "TX_RTT", "TX_RTT_VERIFY", "TX_CURR", "TX_CURR_VERIFY", "TX_PVLS_ELIGIBLE", "TX_PVLS_ELIGIBLE_VERIFY", "TX_PVLS (D)", "TX_PVLS_VERIFY (D)", "TX_PVLS (N)", "TX_PVLS_VERIFY (N)")
tx_link <- c("HTS_TST_POS", "TX_NEW", "TX_NEW_VERIFY")
tx_vl <- c("TX_CURR", "TX_CURR_VERIFY", "TX_PVLS_ELIGIBLE", "TX_PVLS_ELIGIBLE_VERIFY", "TX_PVLS (D)", "TX_PVLS_VERIFY (D)", "TX_PVLS (N)", "TX_PVLS_VERIFY (N)")
snapshot_indicators <- c("TX_CURR", "TX_CURR_VERIFY", "TX_PVLS_ELIGIBLE", "TX_PVLS_ELIGIBLE_VERIFY", "TX_PVLS (D)", "TX_PVLS_VERIFY (D)", "TX_PVLS (N)", "TX_PVLS_VERIFY (N)", "PrEP_CT", "PrEP_CT_VERIFY")
indicator_order <- c("KP_PREV", "KP_PREV: Tested or Referred",  "KP_PREV: Recently Tested", "KP_PREV: Known Positive", "KP_PREV: Declined Testing",  "HTS_SELF", "HTS_TST", "HTS_TST_NEG", "PrEP_OFFER", "PrEP_NEW", "PrEP_NEW_VERIFY", "PrEP_CT", "PrEP_CT_VERIFY", "HTS_TST_POS", "TX_NEW", "TX_NEW_VERIFY", "TX_RTT", "TX_RTT_VERIFY", "TX_CURR", "TX_CURR_VERIFY", "TX_PVLS_ELIGIBLE", "TX_PVLS_ELIGIBLE_VERIFY", "TX_PVLS (D)", "TX_PVLS_VERIFY (D)", "TX_PVLS (N)", "TX_PVLS_VERIFY (N)")

## append test result indicators, keep KP

kp <- df |> rbind(hts) |> rbind(kp_prev) |> 
  filter(pop == "KP") |>
  mutate(fyyy.q_curr = lubridate::quarter(today(), fiscal_start = 1, with_year = TRUE),
         fyyy_currr = floor(fyyy.q_curr),
         fyyy = 2000 + as.integer(str_extract(fy, "[0-9]{2}")),
         fyyy.q = fyyy + q/10,
         fy_curr = str_c("FY", as.character(floor(fyyy.q_curr)-2000)),
         q_curr = round(10*(fyyy.q_curr-floor(fyyy.q_curr))),
         exclude_except_for_quarterly_analysis = indicator %in% snapshot_indicators & ((fyyy == fyyy_currr & q_curr < q) | (fyyy < fyyy_currr & q < 4)),
         exclude_future_quarters = fyyy.q_curr < fyyy + q/10,
        type = if_else(str_detect(indicator, "VERIFY|OFFER|ELIGIBLE"), "Custom", "MER"),
        cascade = case_when(
          indicator %in% prevention_cascade ~ "Prevention",
          indicator %in% tx_link ~ "Treatment: Linkage",
          indicator %in% tx_vl ~ "Treatment: VL"
        ),
        indicator_parent = if_else(str_detect(indicator, "KP_PREV: "), 
                                     "KP_PREV_disaggs", str_remove_all(indicator, "_VERIFY")),
      # indicator = reorder(indicator, match(indicator, indicator_order)),
      # indicator = fct_relevel(indicator, indicator_order),
      indicator = factor(indicator, levels = indicator_order),
      indicator_encoded = match(indicator, indicator_order),
  ) |> 
  select(
        -age_group_order_for_datim, -all_age_groups,
         -data_element_short_name,
         -hiv_result_status) |> 
  relocate(indicator, indicator_parent, .after = country) |> 
  # relocate(age, .before = sex) |> 
  relocate(hivst_type, prev_disaggs, .after = support_type) |> 
  relocate(value, .after = q) |> 
  arrange(indicator)


# Mechanism Info ----------------------------------------------------------
mech <- readxl::read_xlsx(path = "Data/EpiC KP mechanism types and approaches.xlsx") |> 
  janitor::clean_names() |> 
  rename(model_dic_oss = epi_c_report_does_mechanism_feature_drop_in_centers_or_one_stop_shops,
         model_other_community = epi_c_report_other_community_model,
         model_moh = epi_c_report_mechanism_run_entirely_through_the_public_moh_system
         ) |> 
  select(country, model_dic_oss, model_other_community, model_moh) |>
  filter(model_dic_oss == "Yes", model_moh == "Yes") |> 
  print()

kp_model <- kp |> left_join(mech) |> glimpse()
# Write data for Tableau --------------------------------------------------


write_csv(kp_model, "Dataout/young_kp_for_tableau.csv")



kp |> filter(str_detect(indicator, "PREV")) |> count(fyyy, q, exclude_except_for_quarterly_analysis, exclude_future_quarters)  
kp |> count(indicator_parent, indicator, indicator_encoded)  
# count(pop)
glimpse(kp)
kp


## Check for HTS duplication
# no HTS duplication in original data
df |> filter(indicator == "HTS_TST",
             pop == "KP",
             population != "Other KP") |> 
  group_by(country, fy) |> 
  summarise(val = sum(value, na.rm = TRUE)) |> pivot_wider(values_from = "val", names_from = "fy") |> 
  print(n=27)

#no issues below with the HTS_status indicators
hts |> count(indicator)

# no HTS duplication in output data
kp |> filter(indicator == "HTS_TST",
             pop == "KP",
             population != "Other KP") |> 
  group_by(country, fy) |> 
  summarise(val = sum(value, na.rm = TRUE)) |> pivot_wider(values_from = "val", names_from = "fy") |> 
  print(n=27)
