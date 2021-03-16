# 1        Packages ####

library("cowplot")
library("data.table")
library("foreign")
library("ggthemes")
# library("ggExtra")
library("ggsci")
library("Hmisc")
library("janitor")
library("officer")
library("openxlsx")
library("patchwork") # patchwork requires installation process: install.packages("devtools"), devtools::install_github("thomasp85/patchwork")
library("quantreg")
library("rattle")
library("readr")
library("reshape2")
library("scales") 
library("tidyverse")
library("utils")
library("wesanderson")
library("weights")
options(scipen=999)


# 2        Set Working Directory ####

 #setwd("")

# 3        Loading Data ####

# Confidential Microdata

kor_a_0    <- read.dbf("Data_Kor_2018~/Susenas 2018 Maret Kor/kor18rt_diseminasi.dbf")
kor_b_0    <- read.dbf("Data_Kor_2018~/Susenas 2018 Maret Kor/kor18ind_revisi_diseminasi.dbf")

mod_41_a_0 <- read.dbf("Data_Mod_2018~/blok41_gab_prop11-33_diseminasi.dbf")
mod_41_b_0 <- read.dbf("Data_Mod_2018~/blok41_gab_prop34-94_diseminasi.dbf")
mod_42_0   <- read.dbf("Data_Mod_2018~/blok42.dbf")

# 4        Transforming Data ####

# setwd("..")

# nrow(count(kor_a_0, URUT))    # 295,155
# nrow(count(kor_b_0, URUT))    # 295,155
# nrow(count(mod_41_a_0, URUT)) # 140,683
# nrow(count(mod_41_b_0, URUT)) # 154,472
# nrow(count(mod_42_0, URUT))   # 295,155
 
# 4.1      Household Information ####

kor_a_1 <- kor_a_0 %>%
  select(URUT, R101, R102, KABU, R105, R1510A, R1510B, R1511A, R1518A, R1519, starts_with("R1801"), FWT, R301)%>%
  rename(hh_id = URUT, urban = R105, hh_weights = FWT, hh_size = R301)%>%
  rename(province = R101, district = R102, kabupaten = KABU)

kor_a_2 <- kor_a_1 %>%
  select(hh_id, urban, hh_weights, hh_size)%>%
  mutate(urban_01 = ifelse(urban == 1, 1, 0))

# Detailed household information

kor_a_3 <- kor_a_1 %>%
  rename(toilet_a = R1510A, toilet_b = R1510B, water = R1511A, lighting = R1518A, cooking = R1519)%>%
  select(-starts_with("R1801"), - hh_size, - hh_weights, - urban)

# lighting source is electricity indicator

# see codes below

appliances_01 <- kor_a_1 %>%
  select(hh_id, starts_with("R1801"))%>%
  mutate_at(vars(-hh_id), list(function(x) x = ifelse(x == 5, 0, x)))%>%
  rename(refrigerator_01 = R1801B,
          ac_01          = R1801C,
          boiler_01      = R1801D,
          telephone_01   = R1801E,
          computer_01    = R1801F,
          motorcycle_01  = R1801H,
          car_01         = R1801K,
          tv_01          = R1801L)%>%
  select(-starts_with("R1801"))

# Information on household members 

kor_b_1 <- kor_b_0 %>%
  select(URUT, R401, R403, R405, R407, R613, R619)%>%
  rename(hh_id = URUT, ID = R401, household.head = R403, gender = R405, age = R407, education = R613, education_B = R619)

kor_b_2 <- kor_b_1 %>%
  filter(household.head == 1)%>%
  select(hh_id, education, gender, age)

kor_b_3 <- kor_b_1 %>%
  select(hh_id, age)%>%
  mutate(adult = ifelse(age >= 18, 1, 0),
         child = ifelse(age < 18, 1, 0))%>%
  group_by(hh_id)%>%
  summarise(adults = sum(adult),
            children = sum(child))%>%
  ungroup()

kor_a_2 <- kor_a_2 %>%
  left_join(kor_b_2, by = "hh_id")%>%
  left_join(kor_b_3, by = "hh_id")%>%
  left_join(kor_a_3, by = "hh_id")

kor_a_x <- kor_a_2 %>%
  select(hh_id, hh_size, hh_weights, province, district, kabupaten, everything())%>%
  arrange(hh_id)

# write_csv(kor_a_x, "household_information_0.csv")

rm(kor_b_3, kor_b_1, kor_a_1, kor_a_3, appliances_01, kor_a_0, kor_b_0, kor_b_2)

# 264.230.759 people

# 4.2      Information on consumption ####

# Weekly consumption in Rps (excluding self-production)

mod_41_a_1 <- mod_41_a_0 %>%
  select(URUT, KODE, B41K5, B41K6, B41K7, B41K8)%>%
  rename(hh_id = URUT, item_code = KODE, expenditures_weekly = B41K6, expenditures_sp_weekly = B41K8)%>%
  mutate(expenditures_yearly = expenditures_weekly*52,
         expenditures_sp_yearly = expenditures_sp_weekly*52)

mod_41_b_1 <- mod_41_b_0 %>%
  select(URUT, KODE, B41K5, B41K6, B41K7, B41K8)%>%
  rename(hh_id = URUT, item_code = KODE, expenditures_weekly = B41K6, expenditures_sp_weekly = B41K8)%>%
  mutate(expenditures_yearly = expenditures_weekly*52,
         expenditures_sp_yearly = expenditures_sp_weekly*52)

mod_42_1 <- mod_42_0 %>%
  select(URUT, KODE, B42K3, B42K4, B42K5)%>%
  rename(hh_id = URUT, item_code = KODE, expenditures_monthly = B42K4, expenditures_year = B42K5)%>%
  mutate(expenditures_yearly = expenditures_year + expenditures_monthly*12)

expenditures_0 <- bind_rows(mod_41_a_1, mod_41_b_1)%>%
  bind_rows(mod_42_1)%>%
  arrange(hh_id, item_code)

# For the distributional analysis, we should use kor_a_2 and expenditures_0

rm(mod_41_a_0, mod_41_a_1, mod_41_b_0, mod_41_b_1, mod_42_1, mod_42_0)

# 4.3      Codes ####

Education.Code <- count(kor_a_x, education)%>%
  select(-n)
Education.Code$Label <- c("Paket A","SDLB","SD","MI","Paket B","SMPLB","SMP","MTs","Paket C","SMLB","SMA","MA","SMK","MAK","D1/D2","D3","D4","S1","S2","S3",NA)

Toilet.Code <- count(kor_a_x, toilet_a)%>%
  select(-n)
Toilet.Code$Label <- c("Used Alone", "Used with Household Members", "Public Toilet", "No Toilet", "No Facilities")

Water.Code <- count(kor_a_x, water)%>%
  select(-n)
Water.Code$Label <- c("Branded bottled water","Refill water","Plumbing","Borehole / pump","Protected well","An unprotected well","Protected spring","Unprotected spring","Surface water (rivers, lakes, reservoirs, ponds, irrigation)","Rainwater","Others")

Cooking.Code <- count(kor_a_x, cooking)%>%
  select(-n)
Cooking.Code$Label <- c("Don't cook at home","Electricity","Elpiji 5.5 kg / blue gaz","LPG 12 kg","LPG 3 kg","City gas","biogas","Kerosene","Briquettes","Charcoal","Firewood","Others")

Lighting.Code <- count(kor_a_x, lighting)%>%
  select(-n)
Lighting.Code$Label <- c("PLN electricity with meter", "PLN electricity without meter", "Non PLN electricity","Not electricity" )

Province.Code <- count(kor_a_x, province)%>%
  select(-n)

Province.Code$Label <- c("Aceh","Sumatera Utara","Sumatera Barat","Riau","Jambi","Sumatera Selatan","Bengkulu","Lampung","Kepulauan Bangka Belitung","Kepulauan Riau","DKI Jakarta","Jawa Barat","Jawa Tengah","DI Yogyakarta","Jawa Timur","Banten","Bali","Nusa Tenggara Barat","Nusa Tenggara Timur","Kalimantan Barat","Kalimantan Tengah","Kalimantan Selatan","Kalimantan Timur","Kalimantan Utara","Sulawesi Utara","Sulawesi Tengah","Sulawesi Selatan","Sulawesi Tenggara","Gorontalo","Sulawesi Barat","Maluku","Maluku Utara","Papua Barat","Papua")

# setwd("")

# write_csv(Education.Code, "Education.Code.csv")
# write_csv(Toilet.Code,    "Toilet.Code.csv")
# write_csv(Water.Code,     "Water.Code.csv")
# write_csv(Cooking.Code,   "Cooking.Code.csv")
# write_csv(Lighting.Code,  "Lighting.Code.csv")
# write_csv(Province.Code,  "Province.Code.csv")