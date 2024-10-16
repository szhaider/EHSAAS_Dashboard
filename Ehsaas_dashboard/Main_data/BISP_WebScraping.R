#BISP Web Scarping Script
# ------------------------------------------------------------------------------
rm(list = ls())
library(rvest)
library(dplyr)
library(stringr)
# ------------------------------------------------------------------------------
# BISP URL for Total transfers Jan-June 2022
# url_total <- 'https://www.pass.gov.pk/ecs/uct_2022_Jan_Jun.html'
url_total <- 'https://www.pass.gov.pk/ecs/uct_all.html'

# BISP URL for Category 1 transfers Jan-June 2022
url_cat1 <- 'https://www.pass.gov.pk/ecs/uct_cat1.html'

# BISP URL for Category 2 transfers Jan-June 2022
url_cat2 <- 'https://www.pass.gov.pk/ecs/uct_cat2.html'  
# ------------------------------------------------------------------------------
#Reading the HTML code from the website (USing CSS selector Gadget)
webpage_total <- read_html(url_total)
webpage_cat1 <- read_html(url_cat1)
webpage_cat2 <- read_html(url_cat2)

# ------------------------------------------------------------------------------

#Using CSS selector to scrape the BISP Data Table section

# for(i in 1:7){
#   char <- 'tr :nth-child(5)'
#   row <- html_nodes(webpage, char)
#   row_i <- row
# }
# ------------------------------------------------------------------------------
#Total UCTs
province_html_total <- html_nodes(webpage_total,'tr :nth-child(1)')
district_html_total <- html_nodes(webpage_total,'tr :nth-child(2)')
tehsil_html_total   <- html_nodes(webpage_total,'tr :nth-child(3)')
no_of_eligible_beneficiaries_html_total <- html_nodes(webpage_total,'tr :nth-child(4)')
amount_deposited_in_accounts_html_total <- html_nodes(webpage_total,'tr :nth-child(5)')
no_of_beneficiaries_served_html_total <- html_nodes(webpage_total,'tr :nth-child(6)')
amount_withdrawl_html_total <- html_nodes(webpage_total,'tr :nth-child(7)')

head(province_html_total)

#Cleaning into text
province_total <- html_text(province_html_total)
head(province_total)

district_total <- html_text(district_html_total)
head(district_total)

tehsil_total <- html_text(tehsil_html_total)
head(tehsil_total)

no_of_eligible_beneficiaries_total <- (html_text(no_of_eligible_beneficiaries_html_total))
head(no_of_eligible_beneficiaries_total)

amount_deposited_in_accounts_total <- html_text(amount_deposited_in_accounts_html_total)
head(amount_deposited_in_accounts_total)

no_of_beneficiaries_served_total <- html_text(no_of_beneficiaries_served_html_total)
head(no_of_beneficiaries_served_total)

amount_withdrawl_total <- html_text(amount_withdrawl_html_total)
head(amount_withdrawl_total)

bisp_web_data_total <- data.frame(province_total, 
                                  district_total, 
                                  tehsil_total, 
                                  no_of_eligible_beneficiaries_total,
                                  amount_deposited_in_accounts_total, 
                                  no_of_beneficiaries_served_total,
                                  amount_withdrawl_total) 


bisp_web_data_total <- slice_tail(bisp_web_data_total, n=-1)

bisp_web_data_total <- 
  bisp_web_data_total %>% 
  mutate(no_of_eligible_beneficiaries_total = str_remove_all(no_of_eligible_beneficiaries_total, ","),
         amount_deposited_in_accounts_total = str_remove_all(amount_deposited_in_accounts_total, ","),
         no_of_beneficiaries_served_total = str_remove_all(no_of_beneficiaries_served_total, ","), 
         amount_withdrawl_total= str_remove_all(amount_withdrawl_total, ","))

#final BISP Data
bisp_web_data_total_final <- 
  bisp_web_data_total %>% 
  mutate(no_of_eligible_beneficiaries_total = as.numeric(no_of_eligible_beneficiaries_total),
         amount_deposited_in_accounts_total = as.numeric(amount_deposited_in_accounts_total) ,
         no_of_beneficiaries_served_total = as.numeric(no_of_beneficiaries_served_total), 
         amount_withdrawl_total= as.numeric(amount_withdrawl_total)) %>% 
  arrange(district_total)

# BISP District aggregated
bisp_data_total <- 
  bisp_web_data_total_final %>% 
  group_by(province_total, district_total) %>% 
  summarise(
    no_of_eligible_beneficiaries_total = sum(no_of_eligible_beneficiaries_total, na.rm = T),
    amount_deposited_in_accounts_total = sum(amount_deposited_in_accounts_total, na.rm = T) ,
    no_of_beneficiaries_served_total = sum(no_of_beneficiaries_served_total, na.rm = T),
    amount_withdrawl_total = sum(amount_withdrawl_total, na.rm = T) 
  ) %>% 
  mutate(province_total = str_trim(province_total),
         district_total = str_trim(district_total),
         district_total = str_to_title(district_total)) %>% 
  mutate(share_of_actual_to_eligible_beneficiaries_total = 
           no_of_beneficiaries_served_total / no_of_eligible_beneficiaries_total,
  ) %>% 
  ungroup()
# ------------------------------------------------------------------------------

#Category 1
province_html_cat1 <- html_nodes(webpage_cat1,'tr :nth-child(1)')
district_html_cat1 <- html_nodes(webpage_cat1,'tr :nth-child(2)')
tehsil_html_cat1   <- html_nodes(webpage_cat1,'tr :nth-child(3)')
no_of_eligible_beneficiaries_html_cat1 <- html_nodes(webpage_cat1,'tr :nth-child(4)')
amount_deposited_in_accounts_html_cat1 <- html_nodes(webpage_cat1,'tr :nth-child(5)')
no_of_beneficiaries_served_html_cat1 <- html_nodes(webpage_cat1,'tr :nth-child(6)')
amount_withdrawl_html_cat1 <- html_nodes(webpage_cat1,'tr :nth-child(7)')

head(province_html_cat1)

#Cleaning into text
province_cat1 <- html_text(province_html_cat1)
head(province_cat1)

district_cat1 <- html_text(district_html_cat1)
head(district_cat1)

tehsil_cat1 <- html_text(tehsil_html_cat1)
head(tehsil_cat1)

no_of_eligible_beneficiaries_cat1 <- (html_text(no_of_eligible_beneficiaries_html_cat1))
head(no_of_eligible_beneficiaries_cat1)

amount_deposited_in_accounts_cat1 <- html_text(amount_deposited_in_accounts_html_cat1)
head(amount_deposited_in_accounts_cat1)

no_of_beneficiaries_served_cat1 <- html_text(no_of_beneficiaries_served_html_cat1)
head(no_of_beneficiaries_served_cat1)

amount_withdrawl_cat1 <- html_text(amount_withdrawl_html_cat1)
head(amount_withdrawl_cat1)

bisp_web_data_cat1 <- data.frame(province_cat1, 
                                 district_cat1, 
                                 tehsil_cat1, 
                                 no_of_eligible_beneficiaries_cat1,
                                 amount_deposited_in_accounts_cat1, 
                                 no_of_beneficiaries_served_cat1,
                                 amount_withdrawl_cat1) 


bisp_web_data_cat1 <- slice_tail(bisp_web_data_cat1, n=-1)

bisp_web_data_cat1 <- 
  bisp_web_data_cat1 %>% 
  mutate(no_of_eligible_beneficiaries_cat1 = str_remove_all(no_of_eligible_beneficiaries_cat1, ","),
         amount_deposited_in_accounts_cat1 = str_remove_all(amount_deposited_in_accounts_cat1, ","),
         no_of_beneficiaries_served_cat1 = str_remove_all(no_of_beneficiaries_served_cat1, ","), 
         amount_withdrawl_cat1 = str_remove_all(amount_withdrawl_cat1, ","))

#final BISP Data
bisp_web_data_cat1_final <- 
  bisp_web_data_cat1 %>% 
  mutate(no_of_eligible_beneficiaries_cat1 = as.numeric(no_of_eligible_beneficiaries_cat1),
         amount_deposited_in_accounts_cat1 = as.numeric(amount_deposited_in_accounts_cat1) ,
         no_of_beneficiaries_served_cat1 = as.numeric(no_of_beneficiaries_served_cat1), 
         amount_withdrawl_cat1 = as.numeric(amount_withdrawl_cat1)) %>% 
  arrange(district_cat1)

# BISP District aggregated
bisp_data_cat1 <- 
  bisp_web_data_cat1_final %>% 
  group_by(province_cat1, district_cat1) %>% 
  summarise(
    no_of_eligible_beneficiaries_cat1 = sum(no_of_eligible_beneficiaries_cat1, na.rm = T),
    amount_deposited_in_accounts_cat1 = sum(amount_deposited_in_accounts_cat1, na.rm = T) ,
    no_of_beneficiaries_served_cat1 = sum(no_of_beneficiaries_served_cat1, na.rm = T),
    amount_withdrawl_cat1 = sum(amount_withdrawl_cat1, na.rm = T) 
  ) %>% 
  mutate(province_cat1 = str_trim(province_cat1),
         district_cat1 = str_trim(district_cat1),
         district_cat1 = str_to_title(district_cat1)) %>% 
  mutate(share_of_actual_to_eligible_beneficiaries_cat1 = 
           no_of_beneficiaries_served_cat1 / no_of_eligible_beneficiaries_cat1,
  ) %>% 
  ungroup()

# ------------------------------------------------------------------------------

#Category 2
province_html_cat2 <- html_nodes(webpage_cat2,'tr :nth-child(1)')
district_html_cat2 <- html_nodes(webpage_cat2,'tr :nth-child(2)')
tehsil_html_cat2 <- html_nodes(webpage_cat2,'tr :nth-child(3)')
no_of_eligible_beneficiaries_html_cat2 <- html_nodes(webpage_cat2,'tr :nth-child(4)')
amount_deposited_in_accounts_html_cat2 <- html_nodes(webpage_cat2,'tr :nth-child(5)')
no_of_beneficiaries_served_html_cat2 <- html_nodes(webpage_cat2,'tr :nth-child(6)')
amount_withdrawl_html_cat2 <- html_nodes(webpage_cat2,'tr :nth-child(7)')

head(province_html_cat2)

#Cleaning into text
province_cat2 <- html_text(province_html_cat2)
head(province_cat2)

district_cat2 <- html_text(district_html_cat2)
head(district_cat2)

tehsil_cat2 <- html_text(tehsil_html_cat2)
head(tehsil_cat2)

no_of_eligible_beneficiaries_cat2 <- (html_text(no_of_eligible_beneficiaries_html_cat2))
head(no_of_eligible_beneficiaries_cat2)

amount_deposited_in_accounts_cat2 <- html_text(amount_deposited_in_accounts_html_cat2)
head(amount_deposited_in_accounts_cat2)

no_of_beneficiaries_served_cat2 <- html_text(no_of_beneficiaries_served_html_cat2)
head(no_of_beneficiaries_served_cat2)

amount_withdrawl_cat2 <- html_text(amount_withdrawl_html_cat2)
head(amount_withdrawl_cat2)

bisp_web_data_cat2 <- data.frame(province_cat2, 
                                 district_cat2, 
                                 tehsil_cat2, 
                                 no_of_eligible_beneficiaries_cat2,
                                 amount_deposited_in_accounts_cat2, 
                                 no_of_beneficiaries_served_cat2,
                                 amount_withdrawl_cat2) 


bisp_web_data_cat2 <- slice_tail(bisp_web_data_cat2, n=-1)

bisp_web_data_cat2 <- 
  bisp_web_data_cat2 %>% 
  mutate(no_of_eligible_beneficiaries_cat2 = str_remove_all(no_of_eligible_beneficiaries_cat2, ","),
         amount_deposited_in_accounts_cat2 = str_remove_all(amount_deposited_in_accounts_cat2, ","),
         no_of_beneficiaries_served_cat2 = str_remove_all(no_of_beneficiaries_served_cat2, ","), 
         amount_withdrawl_cat2 = str_remove_all(amount_withdrawl_cat2, ","))

#final BISP Data
bisp_web_data_cat2_final <- 
  bisp_web_data_cat2 %>% 
  mutate(no_of_eligible_beneficiaries_cat2 = as.numeric(no_of_eligible_beneficiaries_cat2),
         amount_deposited_in_accounts_cat2 = as.numeric(amount_deposited_in_accounts_cat2) ,
         no_of_beneficiaries_served_cat2 = as.numeric(no_of_beneficiaries_served_cat2), 
         amount_withdrawl_cat2 = as.numeric(amount_withdrawl_cat2)) %>% 
  arrange(district_cat2)

# BISP District aggregated
bisp_data_cat2 <- 
  bisp_web_data_cat2_final %>% 
  group_by(province_cat2, district_cat2) %>% 
  summarise(
    no_of_eligible_beneficiaries_cat2 = sum(no_of_eligible_beneficiaries_cat2, na.rm = T),
    amount_deposited_in_accounts_cat2 = sum(amount_deposited_in_accounts_cat2, na.rm = T) ,
    no_of_beneficiaries_served_cat2 = sum(no_of_beneficiaries_served_cat2, na.rm = T),
    amount_withdrawl_cat2 = sum(amount_withdrawl_cat2, na.rm = T) 
  ) %>% 
  mutate(province_cat2 = str_trim(province_cat2),
         district_cat2 = str_trim(district_cat2),
         district_cat2 = str_to_title(district_cat2)) %>% 
  mutate(share_of_actual_to_eligible_beneficiaries_cat2 = 
           no_of_beneficiaries_served_cat2 / no_of_eligible_beneficiaries_cat2,
  ) %>% 
  ungroup()


bisp_data <- 
  list(Emergency_Cash_Total = bisp_data_total, 
       Category_1 = bisp_data_cat1, 
       Category_2 = bisp_data_cat2)

bisp_data %>% 
  saveRDS("Ehsaas_dashboard/Main_data/bisp_data.rds")

