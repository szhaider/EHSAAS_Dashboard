#Tehsil Data Prep
rm(list=ls())
library(janitor)
library(rio)
library(tidyverse)
library(sf)
#Reading in Shape files

teh_shp <- read_sf("Ehsaas_dashboard/Main_data/Tehsils_shp_UNOCHA/pak_admbnda_adm3_ocha_pco_gaul_20181218.shp") %>% 
  st_as_sf() %>%
  clean_names() %>% 
  select(province = adm1_en ,tehsil = adm3_en, district = adm2_en, geometry) %>% 
   arrange(tehsil) %>% 
  st_transform(crs = "+init=epsg:4326") %>%
  write_rds("Ehsaas_dashboard/Ehsaas/data/pak_shp_tehsil.rds")


# tehsil_names_shp <- teh_shp$tehsil %>% 
#   write.csv("Ehsaas_dashboard/Main_data/tehsil_names.csv")

#433 Tehsils in Pakistan

x <- unique(teh_shp$tehsil)
y <- unique()


################################################################################
#491 Tehsils in Census 2017
# census <- readxl::read_xlsx("Ehsaas_dashboard/Main_data/tehsil_census_data.xlsx") %>% 
#   clean_names() %>% 
#   select(admin_3, tehsil_shp, 
#          population_total,
#          avg_hh_size = average_household_size, 
#          pop_avg_growth_rate) %>% 
#   arrange(admin_3) %>% 
#   write.csv("Ehsaas_dashboard/Main_data/tehsil_census_arranged.csv")

census <- read.csv("Ehsaas_dashboard/Main_data/tehsil_census_arranged.csv") %>% 
  rename(tehsil= tehsil_shp, population = population_total, hhsize = avg_hh_size, pop_gr = pop_avg_growth_rate) %>%
  as_tibble() %>% 
  mutate(population = as.numeric(population)) 
  

################################################################################

#Creating a tibble for units 
units <- tibble(indicator = 
                  c("No Of Eligible Beneficiaries", 
                    "Amount Deposited In Accounts",
                    "No Of Beneficiaries Paid",
                    "Amount Withdrawn",
                    "Share Of Actual To Eligible Beneficiaries",
                    "POPULATION",
                    "Eligible Beneficiaries Relative To District Population",
                    "Actual Beneficiaries Relative To District Population"),
                unit =c( "",
                         "(million Rs)",
                         "",
                         "(million Rs)",
                         "(%)",
                         "(millions)",
                         "(%)",
                         "(%)"))

################################################################################

#Reading in Ehsaas Data
ehsaas_data <- rio::import_list("Ehsaas_dashboard/Main_data/Ehsaas_Web_Data.xlsx")
#Using only category 1 and 2 for Tehsil level since Ehsaas total has NAs ans Unavailable entries

#Alot of tehsils repeating because of spelling errors
#I'm going to treat [tehsil]-sub as [tehsil]

#In district data, 2-30 changes were needed. At tehsil level, much more so did by hand initially

#Category 1 cleaning
ehsaas_data$Category_1_edited <- 
  ehsaas_data$Category_1_edited %>% 
  janitor::clean_names() %>% 
  rename(tehsil = tehsil_area) %>% 
  filter(!is.na(amount_withdrawn)) %>% 
  mutate(tehsil = str_to_title(tehsil)) %>% 
  arrange(tehsil) %>% 
  group_by(tehsil) %>% 
  summarise(
    no_of_eligible_beneficiaries = sum(no_of_eligible_beneficiaries),
    amount_deposited_in_accounts = sum(amount_deposited_in_accounts),
    no_of_beneficiaries_paid = sum(no_of_beneficiaries_paid),
            amount_withdrawn = sum(amount_withdrawn)) %>% 
  mutate(share_of_actual_to_eligible_beneficiaries= no_of_beneficiaries_paid/no_of_eligible_beneficiaries * 100 ) %>% 
  left_join(census, by= "tehsil") %>% 
  arrange(tehsil) %>% 
  filter(!is.na(pop_gr)) %>% 
  mutate(eligible_beneficiaries_relative_to_district_population = (no_of_eligible_beneficiaries  * (hhsize / 1.3)) / (population * (1+pop_gr^5)) * 100 ,
         actual_beneficiaries_relative_to_district_population = (no_of_beneficiaries_paid * (hhsize / 1.3)) / (population * (1+pop_gr^5))* 100  ) %>% 
  mutate(
    amount_deposited_in_accounts = amount_deposited_in_accounts / 1000000,
    amount_withdrawn = amount_withdrawn / 1000000,
    population = population / 1000000) %>% 
  pivot_longer(cols =no_of_eligible_beneficiaries:actual_beneficiaries_relative_to_district_population,
               names_to = "indicator", values_to = "value") %>% 
  mutate(category = "Category 1") %>% 
  mutate(indicator = str_to_title(str_replace_all(indicator, "_", " ")))
  # left_join(units, by="indicator") %>% 
  # filter(indicator!= "Household Size",
         # indicator!= "Pop",
         # indicator!=  "Hhsize",
         # indicator!= "Pop Gr",
         # indicator!= "Population") %>% 

  View()
# filter(
#   tehsil != "HAVELIAN",
#   tehsil != "GISHKORE",
#   tehsil != "GOLARCHI",
#   tehsil != "GOLARCHI (S.F.RAHU)",
#   tehsil !=
#     tehsil != ""
#     tehsil != ""
#     tehsil != ""
#     tehsil != ""
#     tehsil != ""
#     tehsil != ""
#     tehsil != ""
#     tehsil != ""
#     tehsil != ""
#     tehsil !=
#     tehsil !=
#     tehsil !=
#     tehsil !=
#     tehsil !=
#     tehsil !=
#     tehsil !=
#     tehsil !=
#     tehsil !=
# )  Doing this by hand. Need to start from Faisalabad
  
  
  
  
  
  