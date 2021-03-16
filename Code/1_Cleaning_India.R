# setwd("")

# Loading Data ####

# Confidential Microdata

Level1_68 <- read_dta("Identification of Sample Household - Block 1 and 2 - Level 1 -  68.dta") # information on survey
Level2_68 <- read_dta("Level2_68.dta") # information on household characteristics a
Level3_68 <- read_dta("Level3_68.dta") # information on household characteristics b
Level4_68 <- read_dta("Level4_68.dta") # information on household members
Level5_68 <- read_dta("Level5_68.dta") # expenditures on food and energy
Level6_68 <- read_dta("Level6_68.dta") # expenditures on clothing, bedding, footwear
Level7_68 <- read_dta("Level7_68.dta") # expenditures on education and services
Level8_68 <- read_dta("Level8_68.dta") # miscellaneous goods
Level9_68 <- read_dta("Level9_68.dta") # information on appliances
# Level10_68 <- read_dta("Level10_68.dta") # information on Ayurveda and Indian traditional medicine
# Level11_68 <- read_dta("Level11_68.dta") # information on a condensed level

# Transforming Data ####

level_1 <- Level1_68 %>%
  select(HHID, Combined_multiplier, Sector, MLT)%>%
  rename(hh_id = HHID, urban = Sector, hh_weights = Combined_multiplier, hh_weights_question = MLT)%>%
  mutate(Urban = ifelse(urban == 1, "rural", "urban"))%>%
  remove_all_labels()%>%
  select(-hh_weights_question) 

level_2 <- Level2_68 %>%
  select(HHID, HH_Size, Religion, Social_Group)%>%
  rename(hh_id = HHID, hh_size = HH_Size, ethnicity = Religion, ethnicity.1 = Social_Group)%>%
  remove_all_labels()

Level_12 <- left_join(level_1, level_2, by = "hh_id")%>%
  mutate(hh_size = as.numeric(hh_size))

# Population: 1.108.970.933

level_3 <- Level3_68%>%
  select(HHID, Cooking_Code, Lighting_Code)%>%
  rename(hh_id = HHID, lighting.fuel = Lighting_Code, cooking.fuel = Cooking_Code)%>%
  remove_all_labels()%>%
  mutate(cooking.fuel = as.numeric(cooking.fuel))%>%
  mutate(lighting.fuel = as.numeric(lighting.fuel))

level_3$cooking.fuel[is.na(level_3$cooking.fuel)] <- 10
level_3$lighting.fuel[is.na(level_3$lighting.fuel)] <- 6

Level_123 <- left_join(Level_12, level_3, by = "hh_id")%>%
  mutate(electricity.access = ifelse(lighting.fuel == 5, 1, 0))# Attention: Imputed

level_4 <- Level4_68%>%
  select(HHID, Person_sr_no,Age, Relation, Education)

level_4$Age[is.na(level_4$Age)] <- 0

level_4 <- level_4%>%
  mutate(adults = ifelse(Age >= 18, 1, 0))%>%
  mutate(children = ifelse(Age < 18, 1, 0))%>%
  group_by(HHID)%>%
  mutate(adults = sum(adults))%>%
  mutate(children = sum(children))%>%
  ungroup()

level_41 <- level_4 %>%
  filter(Relation == 1)%>%
  select(HHID, Education, adults, children)%>%
  rename(hh_id = HHID, education = Education)%>%
  remove_all_labels()%>%
  mutate(education = as.numeric(education))

level_41$education[is.na(level_41$education)] <- 1

Level_1234 <- left_join(Level_123, level_41, by = "hh_id")

# write_csv(Level_1234, "household_information_India.csv")

level_5 <- Level5_68%>%
  select(HHID, Item_Code, Home_Produce_Value, Total_Consumption_Value, Source_Code)%>%
  rename(hh_id = HHID, item_code = Item_Code)

level_5 <- level_5 %>%
  mutate(expenditures_selfproduced = ifelse(Source_Code !=1 & Source_Code != "", Total_Consumption_Value, 0))%>%
  mutate(expenditures = ifelse(Source_Code == 1 | Source_Code == "", Total_Consumption_Value, 0))%>%
  remove_labels()%>%
  mutate(expenditures = expenditures*365/30)%>%
  mutate(expenditures_selfproduced = expenditures_selfproduced*365/30)%>%
  select(hh_id, item_code, expenditures, expenditures_selfproduced)

test_5 <- level_5 %>%
  group_by(hh_id)%>%
  mutate(expenditures_hh = sum(expenditures))%>%
  ungroup()%>%
  mutate(expenditures_hh_USD = expenditures_hh*0.1871)%>%
  left_join(level_2, by = "hh_id")%>%
  mutate(hh_size = as.numeric(hh_size))%>%
  mutate(hh_expenditures_hh_USD_pc = expenditures_hh_USD/hh_size)

test_5.1 <- test_5 %>%
  select(hh_id, hh_expenditures_hh_USD_pc)%>%
  filter(!duplicated(hh_id))

# write.csv(level_5, "expenditures_food_India.csv")

level_6 <- Level6_68%>%
  select(HHID, Item_Code, Last_365days_Value)%>%
  rename(hh_id = HHID, item_code = Item_Code, expenditures = Last_365days_Value)%>%
  remove_labels()%>%
  mutate(expenditures_selfproduced = 0)

#write.csv(level_6, "expenditures_clothes_India.csv")

level_7 <- Level7_68 %>%
  select(HHID, Item_Code, Expenditure_in_Rs_last_365_days)%>%
  rename(hh_id = HHID, item_code = Item_Code, expenditures = Expenditure_in_Rs_last_365_days)%>%
  remove_labels()%>%
  mutate(expenditures_selfproduced = 0)

#write.csv(level_7, "expenditures_medical_India.csv")

level_8 <- Level8_68 %>%
  select(HHID, Item_code, Value)%>%
  rename(hh_id = HHID, item_code = Item_code, expenditures = Value)%>%
  remove_labels()%>%
  mutate(expenditures_selfproduced = 0)%>%
  mutate(expenditures = expenditures*365/30)

# write.csv(level_8, "expenditures_miscellaneous_India.csv")

level_9_exp <- Level9_68 %>%
  select(HHID, Item_Code, Total_expenditure_365_days)%>%
  rename(hh_id = HHID, item_code = Item_Code, expenditures = Total_expenditure_365_days)%>%
  remove_labels()%>%
  mutate(expenditures_selfproduced = 0)%>%
  filter(!is.na(expenditures))%>%
  filter(expenditures != 0)

level_9 <- Level9_68 %>%
  select(HHID, Item_Code, Whether_Possesses)%>%
  rename(hh_id = HHID, item_code = Item_Code, YN = Whether_Possesses)%>%
  remove_labels()%>%
  mutate(YN = ifelse(YN == 1, 1, 0))%>%
  spread(key = item_code, value = YN)%>%
  select(hh_id, "560", "561", "562", "580", "581", "582", "583", "584", "585", "586", "587", "588", "601", "602", "622", "623")
  
level_91 <- level_9 %>%
  rename(radio.01 = "560", TV.01 = "561", video.01 = "562", fan.01 = "580", ac.01 = "581", generator.01 = "582", light_bulb.01 = "583", sewing.machine.01 = "584", washing_machine.01 = "585", stove.g.01 = "586", cooker.01 = "587", refrigerator.01 = "588", motorcycle.01 = "601", car.01 = "602", computer.no = "622", mobile.01 = "623")

# write.csv(level_91, "appliances_0_1_India.csv")

# Codes  ####

Urban.Code <- data.frame("urban" = c(1,2), "Urban" = c("rural", "urban"))
Cooking.Code <- data.frame("cooking.fuel" = c(seq(1,10, by = 1)), "Label" = c("coke, coal", "firewood and chips", "LPG", "gobar gas", "dung cake", "charcoal", "kerosene", "electricity", "others", "no cooking arrangement"))
Lighting.Code <- data.frame("lighting.fuel" = c(1,2,3,4,5,6,9), "Label" = c("kerosene", "other oil", "gas", "candle", "electricity", "others", "no lighting"))
Education.Code <- data.frame("education" = c(seq(1,8, by = 1), 10, 11, 12, 13), "Label" = c("not literate", "literate without formal schooling", "literatre through TLC", "literate - others", "below primary school", "primary school", "middle school", "secondary school", "higher secondary school", "diploma/certificate course", "graduate", "postgraduate and above"))
Ethnicity.Code <- data.frame("ethnicity" = c(1,2,3,4,5,6,7,9), "Label" = c("Hinduism", "Islam", "Christianity", "Sikhism", "Jainism", "Buddhism", "Zoroastrianism", "Others"))
Ethnicity.Code.1 <- data.frame("ethnicity.1" = c(1,2,3,9), "Label" = c("Scheduled Tribes", "Scheduled Castes", "Other Backward Castes", "Others"))
# Saving Data ####

# setwd("")
# write_csv(Ethnicity.Code, "Ethnicity.Code.csv")
# write_csv(Ethnicity.Code.1, "Ethnicity.Code.1.csv")
# write_csv(Urban.Code, "Urban.Code.csv")
# write_csv(Cooking.Code, "Cooking.Code.csv")
# write_csv(Lighting.Code, "Lighting.Code.csv")
# write_csv(Education.Code, "Education.Code.csv")

# Output for Distributional Paper Coal in Asia ####

Household_Information <- level_1234%>%
  select(-X1)%>%
  select(hh_id, hh_size, hh_weights, urban, Urban, adults, children)

level_6789 <- level_6%>%
  rbind(level_7)%>%
  rbind(level_8)%>%
  rbind(level_9_exp)%>%
  rbind(level_5)%>%
  arrange(hh_id, item_code)%>%
  select(hh_id, item_code, expenditures, expenditures_selfproduced)

Appended <- level_6789 %>%
  mutate(hh_id = as.numeric(hh_id))%>%
  left_join(Household_Information, by = "hh_id")%>%
  select(hh_id, hh_size, hh_weights, urban, Urban, adults, children, item_code, expenditures, expenditures_selfproduced)

#setwd("")

write_csv(Appended, "all_appended_INDIA_68_JAN_2020.csv")

