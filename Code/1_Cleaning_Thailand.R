
# setwd(")

# Loading Data ####

# Confidential Microdata

rec_01 <- read_sav("ses56 for general user_record 01.sav")
rec_02 <- read_sav("ses56 for general user_record 02.sav")
rec_03 <- read_sav("ses56 for general user_record 03.sav") 
rec_04 <- read_sav("ses56 for general user_record 04.sav")
rec_05 <- read_sav("ses56 for general user_record 05.sav")

rec_06 <- read_sav("ses56 for general user_record 06.sav")
rec_07 <- read_sav("ses56 for general user_record 07.sav")
rec_08 <- read_sav("ses56 for general user_record 08.sav")
rec_09 <- read_sav("ses56 for general user_record 09.sav")
rec_10 <- read_sav("ses56 for general user_record 10.sav")

rec_11 <- read_sav("ses56 for general user_record 11.sav")
rec_12 <- read_sav("ses56 for general user_record 12_monetary.sav")

# Assumably, from here on the Data is redundant

# rec_13 <- read.csv2("ses56 for general user_record 13.csv", stringsAsFactors = FALSE) # information on occupation
# rec_14 <- read.csv2("ses56 for general user_record 14.csv", stringsAsFactors = FALSE)
# rec_15 <- read.csv2("ses56 for general user_record 15.csv", stringsAsFactors = FALSE)
# 
# rec_16 <- read.csv2("ses56 for general user_record 16.csv", stringsAsFactors = FALSE)
# rec_17 <- read.csv2("ses56 for general user_record 17.csv", stringsAsFactors = FALSE)
# rec_18_1 <- read.csv2("ses56 for general user_record 18 part 1.csv", stringsAsFactors = FALSE)
# rec_18_2 <- read.csv2("ses56 for general user_record 18 part 2.csv", stringsAsFactors = FALSE)
# rec_18_3 <- read.csv2("ses56 for general user_record 18 part 3.csv", stringsAsFactors = FALSE)
# 
# rec_25 <- read.csv2("ses56 for general user_record 25.csv", stringsAsFactors = FALSE) # information on depts
# rec_30 <- read_sav("ses56 for general user_record 30.sav)

# Transforming Data ####

Urban.Code <- data.frame(urban = c(1,2), Urban = c("Urban", "Rural"))

rec_01.1 <- rec_01 %>%
  select(NEW_HH_N, AREA, C04, A04_1, A07, A13, A52, C05 )%>%
  rename(hh_id = NEW_HH_N, education = C04, urban = AREA, hh_size = A04_1, expenditures_survey = A07, income_survey = A13, hh_weights = A52, children = C05)%>%
  mutate(income_survey       = as.integer(income_survey))%>%
  mutate(adults              = hh_size - children)%>%
  mutate(population          = hh_size*hh_weights)%>%
  mutate(income_survey       = income_survey*12)%>% # income comes monthly --> include seasonal variation
  mutate(expenditures_survey = expenditures_survey*12)%>%
  mutate(urban = as.character(urban))%>%
  select(hh_id, hh_size, hh_weights, population, adults, children, urban, education, expenditures_survey, income_survey)%>%
  remove_all_labels()

rec_02.1 <- rec_02 %>%
  select(NEW_HH_N, HM01, HM05, HM06)%>%
  rename(hh_id = NEW_HH_N)%>%
  filter(HM01 == 1)%>%
  rename(ethnicity = HM05, ethnicity.1 = HM06)%>%
  select(hh_id, ethnicity, ethnicity.1)%>%
  remove_all_labels()

rec_03.1 <- rec_03 %>%
  select(NEW_HH_N, HH09, HH10, HH11, HH15)%>%
  rename(hh_id = NEW_HH_N, electricity.source = HH09, cooking.fuel = HH10, drinking.water = HH11, toilet.type = HH15)%>%
  remove_all_labels()

rec_123 <- left_join(rec_01.1, rec_02.1, by = "hh_id")%>%
  left_join(rec_03.1, by = "hh_id")

rec_123$education[is.na(rec_123$education)] <- 980

write_csv(rec_123, "household_information_Thailand.csv")

Electricity.Code  <- data.frame(electricity.source = as.integer(c(0,1)), "Label" = c("No", "Yes"))
Cooking.Code      <- data.frame(cooking.fuel = as.integer(c(0,1,2,3,4,5,6)), "Label" = c("No cooking", "Charcoal", "Wood", "Kerosene", "Gas", "Electricity", "Others"))
Toilet.Code       <- data.frame(toilet.type = as.integer(c(0,1,2,3,4)), "Label" = c("No facility nearby", "Flush latrine", "Squat", "Bath flush and squat", "Pit/bucket/discharge into water/others"))
Water.Code        <- data.frame("drinking.water" = c(seq(0,8)), "Labels" = c("Bottled", "Inside Piped Water Supply", "Inside piped underground water", "Outside piped or public tab", "Well or underground water", "River, stream etc", "Rain water", "Treated Water Supply", "Others"))
Ethnicity.Code    <- data.frame("ethnicity" = c(1,2,3,4), "Label" = c("Buddhist", "Islam", "Christ", "Other"))
Ethnicity.1.Code  <- data.frame("ethnicity.1" = c(1,2,3,4,5,6,7), "Label" = c("Thai", "Malay/Yawi", "Chinese", "Mon/Burmese", "Cambodian/Souy", "Karen", "Other"))


# Adjusting the remaining data

rec_04 <- rec_04 %>%
  select(NEW_HH_N, EG011:EG122)%>%
  rename(hh_id = NEW_HH_N)

rec_05 <- rec_05 %>%
  select(NEW_HH_N, EG131:EG272)%>%
  rename(hh_id = NEW_HH_N)

rec_06 <- rec_06 %>%
  select(NEW_HH_N, EG281:EG462)%>%
  rename(hh_id = NEW_HH_N)

rec_07 <- rec_07 %>%
  select(NEW_HH_N, EG471:EG602)%>%
  rename(hh_id = NEW_HH_N)

rec_08 <- rec_08 %>%
  select(NEW_HH_N, EG611:EG7922)%>%
  rename(hh_id = NEW_HH_N)

rec_09 <- rec_09 %>%
  select(NEW_HH_N, EG801:EG932)%>%
  rename(hh_id = NEW_HH_N)

rec_10 <- rec_10 %>%
  select(NEW_HH_N, EG941:EG1122)%>%
  rename(hh_id = NEW_HH_N)

rec_11 <- rec_11 %>%
  select(NEW_HH_N, EG1131:EG1212)%>%
  rename(hh_id = NEW_HH_N)

rec_12 <- rec_12 %>%
  select(NEW_HH_N, EF01A, EF02A, EF03A, EF04A, EF05A, EF06A, EF07A, EF08A, EF09A, EF10A, EF11A, EF12A, EF13A, EF14A, EF15A, EF16A, EF17A)%>%
  rename(hh_id = NEW_HH_N)

# We exclude the "value" data --> instead we keep on working with the "money" data

twelve <- function(x)(x = x*12)

rec_rest <- rec_04 %>%
  left_join(rec_05, by = "hh_id")%>%
  left_join(rec_06, by = "hh_id")%>%
  left_join(rec_07, by = "hh_id")%>%
  left_join(rec_08, by = "hh_id")%>%
  left_join(rec_09, by = "hh_id")%>%
  left_join(rec_10, by = "hh_id")%>%
  left_join(rec_11, by = "hh_id")%>%
  left_join(rec_12, by = "hh_id")%>%
  mutate_at(vars(-hh_id), twelve)

colnames(rec_rest) <- sub("EG", "", colnames(rec_rest))
colnames(rec_rest) <- sub("EF", "", colnames(rec_rest))
colnames(rec_rest) <- sub("A", "", colnames(rec_rest))

#write_csv(rec_rest, "expenditures_items_Thailand.csv")

# Shaping Data ####

# Shaping the data for distributional paper

exp <- read_csv("expenditures_items_Thailand.csv", col_types = cols(.default = col_double()))
thai <- read_csv("household_information_Thailand.csv")

exp_thai <- right_join(thai, exp, by = "hh_id")%>%
  select(-X1)%>%
  gather(key = item_code, value = expenditures, "011":"17")%>%
  filter(!is.na(expenditures))%>%
  arrange(hh_id, item_code)

write_csv(exp_thai, "Data_aggregated_January_2020.csv")

item_thai <- exp_thai %>%
  select(item_code)%>%
  filter(!duplicated(item_code))%>%
  arrange(item_code)

# saving data ####

setwd("H:/OwnCloud/Action/1_Thailand_data/Codierungen")
write_csv(Electricity.Code, "Electricity.Code.csv")
write_csv(Urban.Code, "Urban.Code.csv")
write_csv(Toilet.Code, "Toilet.Code.csv")
write_csv(Cooking.Code, "Cooking.Code.csv")
write_csv(Water.Code, "Water.Code.csv")
write_csv(Ethnicity.1.Code, "Ethnicity.1.Code.csv")
write_csv(Ethnicity.Code, "Ethnicity.Code.csv")

Education.Code <- count(rec_123, education)%>%
  select(-n)

write.xlsx(Education.Code, "Education.Code.xlsx")
Education.Code <- read.xlsx("Education.Code.xlsx", sheetName = "Sheet1")

colnames(Education.Code) <- c("education", "Label")

write_csv(Education.Code, "Education.Code.csv")
