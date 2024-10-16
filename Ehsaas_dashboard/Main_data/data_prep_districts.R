#Data Cleaning Script
library(rio)
library(sf)
library(tidyverse)
library(janitor)
library(readxl)
library(dplyr)


#Reading in data from census for HHsize and POp growth rate
census <- read_csv("Ehsaas_dashboard/Main_data/district_census_arranged.csv") %>% 
  clean_names() %>% 
  select(district, pop= population_tot, hhsize = avg_hhsize, pop_gr =pop_avg_growth_rate) %>% 
  filter(!is.na(district)) %>% 
  arrange(district) %>% 
  mutate(pop_gr= pop_gr/100)


#Reading in district Population and preparing for merge
pak_ind <- readRDS("Ehsaas_dashboard/Main_data/pak_ind.RDS")

pak_ind <- pak_ind %>% 
  filter(indicator_1== "Population Census",
         year == 2018) %>%
  select(district, value) %>% 
  arrange(district) %>% 
  rename(population = value)

#Creating a tibble for units 
units <- 
  tibble(indicator = 
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
       

#Reading in District level hhsize (from PAKISTAN INdicators -PSLM data which Ramiro shared)
pakistan_indicators <- import_list("Ehsaas_dashboard/Main_data/Pakistan_Indicators_1920.xlsx")
pakistan_indicators<- pakistan_indicators$PSLM2019 %>% 
  filter(year == 2019,
         indicator == "Household size") %>% 
  select(district, indicator , value) %>% 
  pivot_wider(district, names_from = "indicator", values_from = "value") %>% 
  clean_names() %>% 
  mutate(district =
           case_when(   #taking avg of hhsize of Karachi distircts
             district == "Karachi Central" ~ "Karachi City",
             district == "Karachi East" ~ "Karachi City",
             district == "Karachi Malir" ~ "Karachi City",
             district == "Karachi South" ~ "Karachi City",
             district == "Karachi West" ~ "Karachi City",
             district == "Kachhi/ Bolan" ~ "Kachhi",
             district == "Kech/Turbat" ~ "Kech",
             
             TRUE ~ district
           )) %>% 
  group_by(district) %>% 
  summarize(household_size = mean(household_size)) %>% 
  filter(!district == "Korangi",
          !district == "Malir",
          !district == "Duki",
          !district == "Shaheed Sikandarabad") 

#match(pakistan_indicators$district, ehsaas_data$Emergency_Cash_Total$district)

#Reading in Ehsaas data  (Shinfting to new web scraped data as on 19/09/2022)<
# ehsaas_data <- import_list("Ehsaas_dashboard/Main_data/Ehsaas_Web_Data.xlsx")
ehsaas_data <- readRDS("Ehsaas_dashboard/Main_data/bisp_data.rds") #<<<<<<<<<<<<-------

ehsaas_data$Emergency_Cash_Total <- 
  ehsaas_data$Emergency_Cash_Total %>% 
  clean_names() %>% #In the Ehsaas Portal, FR Districts are names as T A ADJ...
  
  rename(province = province_total,
    district = district_total,
    no_of_eligible_beneficiaries = no_of_eligible_beneficiaries_total,
    amount_deposited_in_accounts = amount_deposited_in_accounts_total,
    no_of_beneficiaries_paid = no_of_beneficiaries_served_total,
    amount_withdrawn = amount_withdrawl_total
    ) %>% # <--
  filter(province != "AJK",
         province != "GB") %>% 
  
  mutate(district = str_trim(district)) %>% 
           arrange(district) %>% 
  mutate(district = str_to_title(district)) %>% 
  mutate(province = str_to_title(province)) %>% 
  # mutate(tehsil_area = str_to_title(tehsil_area)) %>%
  mutate(province = if_else(province == "Kp", "KP", province)) %>% 
  mutate(district = if_else(district == "Shaheed Benazir Abad", "Shaheed Benazirabad", district)) %>% 
  mutate(district = case_when( 
    district == "T A Adj Bannu" ~ "FR Bannu",
    district == "T A Adj D.i.khan" ~ "FR Dera Ismail Khan",
    district == "T A Adj Kohat" ~ "FR Kohat",
    district == "T A Adj Peshawar" ~ "FR Peshawar",
    district == "T A Adj Tank" ~ "FR Tank",
    district == "T.a.adj.lakki Marwat" ~ "FR Lakki Marwat",
    district == "D. I. Khan" ~  "Dera Ismail Khan",
    district == "Karachi Central" ~ "Karachi City",
    district == "Karachi East" ~ "Karachi City",
    district == "Karachi Malir" ~ "Karachi City",
    district == "Karachi South" ~ "Karachi City",
    district == "Karachi West" ~ "Karachi City",
    district == "Malakand P Area" ~ "Malakand PA",
    district == "Sajawal" ~ "Sujawal",
    district == "S Waziristan Agency" ~ "South Waziristan Agency",
    district == "N Waziristan Agency" ~ "North Waziristan Agency",
    district == "Sherani" ~ "Sheerani",
    district == "Kambar Shahdad Kot" ~ "Qambar Shahdadkot",
    district == "Lasbela" ~ "Las Bela",
    district == "Leiah" ~ "Layyah",
    district == "Sibbi" ~ "Sibi",
    district == "Tando Allahyar" ~ "Tando Allah Yar",
    district == "Tor Garh" ~ "Tor Ghar",
    district == "Umer Kot" ~ "Umerkot",
      TRUE ~ district)) %>% 
    group_by(district) %>% 
    summarise(no_of_eligible_beneficiaries = sum(no_of_eligible_beneficiaries),
              amount_deposited_in_accounts = sum(amount_deposited_in_accounts),
              no_of_beneficiaries_paid = sum(no_of_beneficiaries_paid),
              amount_withdrawn = sum(amount_withdrawn)) %>% 
  arrange(district) %>% 
  filter(!district == "Korangi",
         !district == "Malir",
         !district == "Duki",
         !district == "Shaheed Sikandarabad") %>%   #Dropping Kambar Shahadat Kot since not available in shapfile
  mutate(share_of_actual_to_eligible_beneficiaries= no_of_beneficiaries_paid/no_of_eligible_beneficiaries * 100 ) %>% 
  left_join(pak_ind, by ="district") %>% 
  left_join(pakistan_indicators, by= "district") %>% 
  left_join(census, by= "district") %>% 
  mutate(eligible_beneficiaries_relative_to_district_population = (no_of_eligible_beneficiaries  * (hhsize / 1.3)) / (population * (1+pop_gr^5)) * 100 ,
         actual_beneficiaries_relative_to_district_population = (no_of_beneficiaries_paid * (hhsize / 1.3)) / (population * (1+pop_gr^5))* 100  ) %>% 
  mutate(
         amount_deposited_in_accounts = amount_deposited_in_accounts / 1000000,
         amount_withdrawn = amount_withdrawn / 1000000,
         population = pop / 1000000) %>% 
   pivot_longer(cols =no_of_eligible_beneficiaries:actual_beneficiaries_relative_to_district_population,
                 names_to = "indicator", values_to = "value") %>% 
  mutate(category = "Ehsaas Cash Total") %>% 
  mutate(indicator = str_to_title(str_replace_all(indicator, "_", " "))) %>% 
  left_join(units, by="indicator") %>% 
  filter(indicator!= "Household Size",
         indicator!= "Pop",
         indicator!=  "Hhsize",
         indicator!= "Pop Gr",
         indicator!= "Population")
# write.csv("data/ehsaas_total_cash.csv")

ehsaas_data$Category_1 <- 
  ehsaas_data$Category_1 %>% 
  clean_names()  %>%   #In the Ehsaas Portal, FR Districts are names as T A ADJ...
  
  rename(province = province_cat1,
         district = district_cat1,
         no_of_eligible_beneficiaries = no_of_eligible_beneficiaries_cat1,
         amount_deposited_in_accounts = amount_deposited_in_accounts_cat1,
         no_of_beneficiaries_paid = no_of_beneficiaries_served_cat1,
         amount_withdrawn = amount_withdrawl_cat1
  ) %>% # <--
  filter(province != "AJK",
         province != "GB") %>% 
  
  arrange(district) %>% 
  mutate(district = str_to_title(district)) %>% 
  mutate(province = str_to_title(province)) %>% 
  # mutate(tehsil_area = str_to_title(tehsil_area)) %>%
  mutate(province = if_else(province== "Kp", "KP", province)) %>%  
  mutate(district = if_else(district== "Shaheed Benazir Abad", "Shaheed Benazirabad", district)) %>% 
  mutate(district = case_when( 
    district == "T A Adj Bannu" ~ "FR Bannu",
    district == "T A Adj D.i.khan" ~ "FR Dera Ismail Khan",
    district == "T A Adj Kohat" ~ "FR Kohat",
    district == "T A Adj Peshawar" ~ "FR Peshawar",
    district == "T A Adj Tank" ~ "FR Tank",
    district == "T.a.adj.lakki Marwat" ~ "FR Lakki Marwat",
    district == "D. I. Khan" ~  "Dera Ismail Khan",
    district == "Karachi Central" ~ "Karachi City",
    district == "Karachi East" ~ "Karachi City",
    district == "Karachi Malir" ~ "Karachi City",
    district == "Karachi South" ~ "Karachi City",
    district == "Karachi West" ~ "Karachi City",
    district == "Malakand P Area" ~ "Malakand PA",
    district == "Sajawal" ~ "Sujawal",
    district == "S Waziristan Agency" ~ "South Waziristan Agency",
    district == "N Waziristan Agency" ~ "North Waziristan Agency",
    district == "Sherani" ~ "Sheerani",
    district == "Kambar Shahdad Kot" ~ "Qambar Shahdadkot",
    district == "Lasbela" ~ "Las Bela",
    district == "Leiah" ~ "Layyah",
    district == "Sibbi" ~ "Sibi",
    district == "Tando Allahyar" ~ "Tando Allah Yar",
    district == "Tor Garh" ~ "Tor Ghar",
    district == "Umer Kot" ~ "Umerkot",
    TRUE ~ district)) %>% 
  arrange(district) %>% 
  group_by(district) %>% 
  summarise(no_of_eligible_beneficiaries = sum(no_of_eligible_beneficiaries),
            amount_deposited_in_accounts = sum(amount_deposited_in_accounts),
            no_of_beneficiaries_paid = sum(no_of_beneficiaries_paid),
            amount_withdrawn = sum(amount_withdrawn)) %>%
  # left_join(d, by=c("district" = "d"))
  
  filter(!district == "Korangi",
         !district == "Malir",
         !district == "Duki",
         !district == "Shaheed Sikandarabad"
  ) %>%  #Dropping Kambar Shahadat Kot since not available in shapfile
  mutate(share_of_actual_to_eligible_beneficiaries= no_of_beneficiaries_paid/no_of_eligible_beneficiaries * 100 ) %>% 
  left_join(pak_ind, by ="district") %>% 
  left_join(census, by= "district") %>%
  left_join(pakistan_indicators, by= "district") %>% 
  mutate(eligible_beneficiaries_relative_to_district_population = 
           (no_of_eligible_beneficiaries  * (hhsize / 1.3)) / (pop * (1+pop_gr^5)) * 100 ,
         actual_beneficiaries_relative_to_district_population = 
           (no_of_beneficiaries_paid * (hhsize / 1.3)) / (pop * (1+pop_gr^5))* 100  ) %>% 
  
  # mutate(eligible_beneficiaries_relative_to_district_population=no_of_eligible_beneficiaries/population *100,
  #        actual_beneficiaries_relative_to_district_population= no_of_beneficiaries_paid/population* 100) %>% 
  mutate(
         amount_deposited_in_accounts = amount_deposited_in_accounts / 1000000,
         amount_withdrawn = amount_withdrawn / 1000000,
         population = pop / 1000000) %>%
  
  # #Since North Waziristan and Torgar are missing
  # add_row(district = "North Waziristan Agency",
  #         no_of_eligible_beneficiaries = NA,
  #         amount_deposited_in_accounts = NA,
  #         no_of_beneficiaries_paid = NA,
  #         amount_withdrawn = NA,
  #         
          # )
  
  pivot_longer(cols =no_of_eligible_beneficiaries:actual_beneficiaries_relative_to_district_population,
               names_to = "indicator", values_to = "value")%>% 
  mutate(category = "Category 1") %>% 
  mutate(indicator = str_to_title(str_replace_all(indicator, "_", " "))) %>% 
  left_join(units, by="indicator") %>% 
  filter(indicator!= "Household Size",
         indicator!= "Pop",
         indicator!=  "Hhsize",
         indicator!= "Pop Gr",
         indicator!= "Population") %>% 
  pivot_wider(names_from = district, values_from = value) %>% 
  mutate(`North Waziristan Agency` = NA,
         `Tor Ghar` = NA) %>% 
  pivot_longer(cols = -c("indicator", "category", "unit") ,
               names_to = "district", 
               values_to = "value")%>% 
  arrange(district)

# d %>%
#   anti_join(ehsaas_data$Category_1, by=c("d" = "district")) %>% 
#   View()

# North Waziristan Agency
# Tor Ghar

  # write.csv("data/category_1.csv")

ehsaas_data$Category_2 <- 
  ehsaas_data$Category_2 %>% 
  clean_names() %>%   
  
  rename(province = province_cat2,
         district = district_cat2,
         no_of_eligible_beneficiaries = no_of_eligible_beneficiaries_cat2,
         amount_deposited_in_accounts = amount_deposited_in_accounts_cat2,
         no_of_beneficiaries_paid = no_of_beneficiaries_served_cat2,
         amount_withdrawn = amount_withdrawl_cat2
  ) %>% # <--
  filter(province != "AJK",
         province != "GB") %>% 

  arrange(district) %>% 
  mutate(district = str_to_title(district)) %>% 
  mutate(province = str_to_title(province)) %>% 
  # mutate(tehsil_area = str_to_title(tehsil_area)) %>%
  mutate(province = if_else(province== "Kp", "KP", province)) %>% 
  mutate(district = if_else(district== "Shaheed Benazir Abad", "Shaheed Benazirabad", district)) %>% 
  group_by(district) %>% 
  summarise(no_of_eligible_beneficiaries = sum(no_of_eligible_beneficiaries),
            amount_deposited_in_accounts = sum(amount_deposited_in_accounts),
            no_of_beneficiaries_paid = sum(no_of_beneficiaries_paid),
            amount_withdrawn = sum(amount_withdrawn)) %>% 
    mutate(district = case_when( 
      district == "T A Adj Bannu" ~ "FR Bannu",
      district == "T A Adj D.i.khan" ~ "FR Dera Ismail Khan",
      district == "T A Adj Kohat" ~ "FR Kohat",
      district == "T A Adj Peshawar" ~ "FR Peshawar",
      district == "T A Adj Tank" ~ "FR Tank",
      district == "T.a.adj.lakki Marwat" ~ "FR Lakki Marwat",
      district == "D. I. Khan" ~  "Dera Ismail Khan",
      district == "Karachi Central" ~ "Karachi City",
      district == "Karachi East" ~ "Karachi City",
      district == "Karachi Malir" ~ "Karachi City",
      district == "Karachi South" ~ "Karachi City",
      district == "Karachi West" ~ "Karachi City",
      district == "Malakand P Area" ~ "Malakand PA",
      district == "Sajawal" ~ "Sujawal",
      district == "S Waziristan Agency" ~ "South Waziristan Agency",
      district == "N Waziristan Agency" ~ "North Waziristan Agency",
      district == "Sherani" ~ "Sheerani",
      district == "Kambar Shahdad Kot" ~ "Qambar Shahdadkot",
      district == "Lasbela" ~ "Las Bela",
      district == "Leiah" ~ "Layyah",
      district == "Sibbi" ~ "Sibi",
      district == "Tando Allahyar" ~ "Tando Allah Yar",
      district == "Tor Garh" ~ "Tor Ghar",
      district == "Umer Kot" ~ "Umerkot",
      TRUE ~ district)) %>% 
  arrange(district) %>% 
  group_by(district) %>% 
  summarise(no_of_eligible_beneficiaries = sum(no_of_eligible_beneficiaries),
            amount_deposited_in_accounts = sum(amount_deposited_in_accounts),
            no_of_beneficiaries_paid = sum(no_of_beneficiaries_paid),
            amount_withdrawn = sum(amount_withdrawn)) %>%
  filter(
         !district == "Korangi",
         !district == "Malir",
          !district == "Duki",
         !district == "Shaheed Sikandarabad"
    ) %>%   #Dropping Kambar Shahadat Kot, Korangi since not available in shapfile
  #Also aggregating all Karachi dsitricts into Karachi City
  mutate(share_of_actual_to_eligible_beneficiaries= no_of_beneficiaries_paid/no_of_eligible_beneficiaries * 100) %>% 
  left_join(pak_ind, by ="district") %>% 
  left_join(pakistan_indicators, by= "district") %>% 
  left_join(census, by= "district") %>% 
  
  mutate(eligible_beneficiaries_relative_to_district_population = (no_of_eligible_beneficiaries  * (hhsize / 1.3)) / (pop * (1+pop_gr^5)) * 100 ,
         actual_beneficiaries_relative_to_district_population = (no_of_beneficiaries_paid * (hhsize / 1.3)) / (pop * (1+pop_gr^5))* 100  ) %>% 
  
  # mutate(eligible_beneficiaries_relative_to_district_population=no_of_eligible_beneficiaries/population *100,
  #        actual_beneficiaries_relative_to_district_population= no_of_beneficiaries_paid/population* 100) %>% 
  mutate(
         amount_deposited_in_accounts = amount_deposited_in_accounts / 1000000,
         amount_withdrawn = amount_withdrawn / 1000000,
         population = pop / 1000000) %>%
   pivot_longer(cols =no_of_eligible_beneficiaries:actual_beneficiaries_relative_to_district_population,
               names_to = "indicator", values_to = "value") %>% 
  mutate(category = "Category 2") %>% 
  mutate(indicator = str_to_title(str_replace_all(indicator, "_", " "))) %>% 
  left_join(units, by="indicator") %>% 
  filter(indicator!= "Household Size",
         indicator!= "Pop",
         indicator!=  "Hhsize",
         indicator!= "Pop Gr",
         indicator!= "Population") %>% 
  pivot_wider(names_from = district, values_from = value) %>% 
  mutate(`North Waziristan Agency` = NA,
         `Tor Ghar` = NA) %>% 
  pivot_longer(cols = -c("indicator", "category", "unit") ,
               names_to = "district", 
               values_to = "value") %>% 
  arrange(district)


  # write.csv("data/category_2.csv")

#I have inserted TOR GHAR and North Waziristan by hand in the excel file category1 and 2, Since its missing in Ehsaas, but polygon is there in shape file

data <- 
  ehsaas_data$Emergency_Cash_Total %>%
  bind_rows(ehsaas_data$Category_1, ehsaas_data$Category_2) %>% 
  filter(!is.na(district))

#For adding category 1 and 2
data1 <- data %>% 
  filter(category != "Ehsaas Cash Total", 
         indicator != "Actual Beneficiaries Relative To District Population", 
         indicator != "Eligible Beneficiaries Relative To District Population", 
         indicator !="Share Of Actual To Eligible Beneficiaries") %>% 
  group_by(district, indicator) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  pivot_wider(district ,names_from = indicator , values_from = value) %>% 
  clean_names() %>%
  left_join(census, by= "district") %>% 
  mutate(share_of_actual_to_eligible_beneficiaries= no_of_beneficiaries_paid/no_of_eligible_beneficiaries * 100,
         eligible_beneficiaries_relative_to_district_population = (no_of_eligible_beneficiaries  * (hhsize / 1.3)) / (pop * (1+pop_gr^5)) * 100 ,
         actual_beneficiaries_relative_to_district_population = (no_of_beneficiaries_paid * (hhsize / 1.3)) / (pop * (1+pop_gr^5))* 100  ) %>% 
  pivot_longer(cols =amount_deposited_in_accounts:actual_beneficiaries_relative_to_district_population,
               names_to = "indicator", values_to = "value") %>% 
  mutate(category = "Category 1+2") %>% 
  mutate(indicator = str_to_title(str_replace_all(indicator, "_", " "))) %>% 
  left_join(units, by="indicator") %>% 
    filter(indicator!= "Household Size",
           indicator!= "Pop",
           indicator!=  "Hhsize",
           indicator!= "Pop Gr",
           indicator!= "Population")
  


data <- data %>% 
  bind_rows(data1)

#SOurce
source <- import_list("Ehsaas_dashboard/Main_data/source.xlsx")
source1 <- source$source1
source2 <- source$source2

data <- data %>% 
  left_join(source1, by= "indicator") %>% 
  left_join(source2, by= "category") 
 
 data %>% 
     # write.csv("Ehsaas_dashboard/Main_data/data/ehsaas_clean.csv")  #To be shared
    write_rds("Ehsaas_dashboard/Ehsaas/data/ehsaas_clean.rds")
  
#Reading in Shape files
dis_shp <- read_sf("Ehsaas_dashboard/Main_data/Districts_shp_usual/pakistan_indicators.shp") %>% 
  st_as_sf() %>%
  clean_names() %>% 
  filter(year == 2018) %>% 
  select(district ,geometry) %>% 
  arrange(district) %>% 
  st_transform(crs = "+init=epsg:4326") %>%
  write_rds("Ehsaas_dashboard/Ehsaas/data/pak_shp.rds")


#Reading in 2017 - census data for HOusehold size and pop-growth rate
# census <- read_excel("Ehsaas_dashboard/Main_data/district_census.xlsx")
# View(census)
# census %>% arrange(District) %>% write_csv("Ehsaas_dashboard/Main_data/district_census_arranged.csv")





