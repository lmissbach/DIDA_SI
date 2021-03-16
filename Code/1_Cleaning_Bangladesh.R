# setwd("")

# Loading Data ####

# Confidential Microdata

rt001 <- read_dta("rt001.dta")
rt002 <- read_dta("rt002.dta")
# rt003 <- read_dta("rt003.dta") # Includes Information on Work and Payment, Wages etc.
# rt004 <- read_dta("rt004.dta") # Includes Information on Enterprises, Customers and Revenues
# rt005 <- read_dta("rt005.dta") # Includes Information on Shocks (maybe interesting: Droughts, Floods, Landslides, Erosion)
# rt006 <- read_dta("rt006.dta") # Includes Information on Self-Produced Agricultural Products --> Excluded because of comparability
# rt007 <- read_dta("rt007.dta") # Includes Information on Livestock
# rt008 <- read_dta("rt008.dta") # Includes Information on Products from Livestock
# rt009 <- read_dta("rt009.dta") # Includes Information on Fishery Products
# rt010 <- read_dta("rt010.dta") # Includes Information on Forestry
# rt011 <- read_dta("rt011.dta") # Includes Information on agricultural assets
# rt012 <- read_dta("rt012.dta") # Includes Information on agricultural assets
# rt013 <- read_dta("rt013.dta") # Includes Information on Remittances
# rt014 <- read_dta("rt014.dta") # Includes Information on Micro Credits
rt015 <- read_dta("rt015.dta")
rt016 <- read_dta("rt016.dta")
rt017 <- read_dta("rt017.dta")
rt018 <- read_dta("rt018.dta")
rt019 <- read_dta("rt019.dta")
rt020 <- read_dta("rt020.dta")

# Transforming Data ####

#rt001

rt001 <- as_tibble(rt001)

Urban.New <- data.frame("urbanrur" = c(1,2,3,4), "urban" = c(1,2,1,2), "Urban" = c("Rural", "Urban", "Rural", "Urban"))
Urban.Code <- Urban.New %>%
  select(urban, Urban)%>%
  filter(!duplicated(urban))

Electricity.Code <- data.frame("electricity.source" = c(0,1,2), "Electricity.Source" = c("No", "Yes", "No"))
Toilet.Type.Code <- data.frame("toilet.type" = c(1,2,3,4,5,6), "Toilet.Type" = c("Sanitary", "Pacca Latrine (Water Seal)", "Pacca Latrine (Pit)", "Kacha Latrine (perm)", "Kacha Latrine (temp)", "Other, specify"))

rt001.1 <- remove_all_labels(rt001)%>%
  unite("hh_id", c("psu","hhold"), sep = "")%>%
  left_join(Urban.New, by = "urbanrur")%>%
  select(hh_id, urban, wgt, s06a_q08, s06a_q14, s06a_q09)%>%
  rename(hh_weigths = wgt, toilet.type = s06a_q08, electricity.source = s06a_q14, drinking.water = s06a_q09)

Water.Code <- data.frame("drinking.water" = c(1:6), Label = c("Supply Water", "Tubewell", "Pond/river", "Well", "Waterfall/Spring", "Other, specify"))

rt002.1 <- remove_all_labels(rt002)%>%
  unite("hh_id", c("psu", "hhold"), sep = "")%>%
  select(hh_id, idcode, s01a_q04, s01a_q05, s02a_q05)

rt002.1.1 <- rt002.1%>%
  group_by(hh_id)%>%
  mutate(hh_size = n())%>%
  mutate(adults = ifelse(s01a_q04 > 18, 1, 0))%>%
  mutate(children = ifelse(s01a_q04 < 18, 1, 0))%>%
  summarise(
    hh_size = first(hh_size),
    children = sum(children),
    adults = sum(adults)
  )%>%
  ungroup()

rt002.1.2 <- rt002.1 %>%
  filter(idcode == "01")%>%
  rename(ethnicity = s01a_q05, education = s02a_q05)%>%
  select(hh_id, ethnicity, education)

rt002.1 <- left_join(rt002.1.1, rt002.1.2, by = "hh_id")

rt001.12 <- left_join(rt001.1, rt002.1, by = "hh_id")

Education.Code <- data.frame("education" = c(seq(0,19)), "Label" = c("No class passed", "Class 1", "Class 2", "Class 3", "Class 4", "Class 5", "Class 6", "Class 7", "Class 8", "Class 9", "SSC equivalent", "HSV equivalent", "Graduate equivalent", "Post graduate equivalent", "Medical", "Engineering", "Vocational", "Technical Education", "Nursing", "Other specify"))
Ethnicity.Code <- data.frame("ethnicity" = c(1,2,3,4,5), "Label" = c("Islam", "Hinduism", "Buddhism", "Christianity", "Other, specify"))

# write_csv(rt001.12, "household_information_Bangladesh.csv")

rt015.1 <- remove_all_labels(rt015) %>%
  filter(!is.na(hhold))%>%
  mutate(psu = formatC(psu, flag = "0", width = 3), sep = "")%>%
  mutate(hhold = formatC(hhold, flag = "0", width = 3), sep = "")%>%
  unite("hh_id", c("psu", "hhold"), sep = "")%>%
  rename(D.1 = s09a1d_2, D.2 = s09a1d_5, D.3 = s09a1d_8, D.4 = s09a1_11, D.5 = s09a1_14, D.6 = s09a1_17, D.7 = s09a1_20, D.8 = s09a1_23, D.9 = s09a1_26, D.10 =  s09a1_29, D.11 =  s09a1_32, D.12 =  s09a1_35, D.13 =  s09a1_38,  D.14 = s09a1_41)%>%
  rename(T.1 = s09a1d_3, T.2 = s09a1d_6, T.3 = s09a1d_9, T.4 = s09a1_12, T.5 = s09a1_15, T.6 = s09a1_18, T.7 = s09a1_21, T.8 = s09a1_24, T.9 = s09a1_27, T.10 =  s09a1_30, T.11 =  s09a1_33, T.12 =  s09a1_36, T.13 =  s09a1_39,  T.14 = s09a1_42)%>%
  mutate(   exp.1 = ifelse(T.1 == 1, D.1, 0))%>%
  mutate(exp.sp.1 = ifelse(T.1 != 1, D.1, 0))%>%
  mutate(   exp.2 = ifelse(T.2 == 1, D.2, 0))%>%
  mutate(exp.sp.2 = ifelse(T.2 != 1, D.2, 0))%>%
  mutate(   exp.3 = ifelse(T.3 == 1, D.3, 0))%>%
  mutate(exp.sp.3 = ifelse(T.3 != 1, D.3, 0))%>%
  mutate(   exp.4 = ifelse(T.4 == 1, D.4, 0))%>%
  mutate(exp.sp.4 = ifelse(T.4 != 1, D.4, 0))%>%
  mutate(   exp.5 = ifelse(T.5 == 1, D.5, 0))%>%
  mutate(exp.sp.5 = ifelse(T.5 != 1, D.5, 0))%>%
  mutate(   exp.6 = ifelse(T.6 == 1, D.6, 0))%>%
  mutate(exp.sp.6 = ifelse(T.6 != 1, D.6, 0))%>%
  mutate(   exp.7 = ifelse(T.7 == 1, D.7, 0))%>%
  mutate(exp.sp.7 = ifelse(T.7 != 1, D.7, 0))%>%
  mutate(   exp.8 = ifelse(T.8 == 1, D.8, 0))%>%
  mutate(exp.sp.8 = ifelse(T.8 != 1, D.8, 0))%>%
  mutate(   exp.9 = ifelse(T.9 == 1, D.9, 0))%>%
  mutate(exp.sp.9 = ifelse(T.9 != 1, D.9, 0))%>%
  mutate(   exp.10 = ifelse(T.10 == 1, D.10, 0))%>%
  mutate(exp.sp.10 = ifelse(T.10 != 1, D.10, 0))%>%
  mutate(   exp.11 = ifelse(T.11 == 1, D.11, 0))%>%
  mutate(exp.sp.11 = ifelse(T.11 != 1, D.11, 0))%>%
  mutate(   exp.12 = ifelse(T.12 == 1, D.12, 0))%>%
  mutate(exp.sp.12 = ifelse(T.12 != 1, D.12, 0))%>%
  mutate(   exp.13 = ifelse(T.13 == 1, D.13, 0))%>%
  mutate(exp.sp.13 = ifelse(T.13 != 1, D.13, 0))%>%
  mutate(   exp.14 = ifelse(T.14 == 1, D.14, 0))%>%
  mutate(exp.sp.14 = ifelse(T.14 != 1, D.14, 0))%>%
  select(hh_id, item, exp.1, exp.sp.1, exp.2, exp.sp.2, exp.3, exp.sp.3, exp.4, exp.sp.4, exp.5, exp.sp.5, exp.6, exp.sp.6, exp.7, exp.sp.7, exp.8, exp.sp.8, exp.9, exp.sp.9, exp.10, exp.sp.10, exp.11, exp.sp.11, exp.12, exp.sp.12, exp.13, exp.sp.13, exp.14, exp.sp.14)

rt015.1[is.na(rt015.1)] <- 0
  
rt015.2 <- rt015.1 %>%
  mutate(expenditures = exp.1 + exp.2 + exp.3 + exp.4 + exp.5 + exp.6 + exp.7 + exp.8 + exp.9 + exp.10 + exp.11 + exp.12 + exp.13 + exp.14)%>%
  mutate(expenditures.sp = exp.sp.1 + exp.sp.2 + exp.sp.3 + exp.sp.4 + exp.sp.5 + exp.sp.6 + exp.sp.7 + exp.sp.8 + exp.sp.9 + exp.sp.10 + exp.sp.11 + exp.sp.12 + exp.sp.13 + exp.sp.14)%>%
  select(hh_id, item, expenditures, expenditures.sp)%>%
  mutate(expenditures    = (expenditures/14)*365)%>%
  mutate(expenditures.sp = (expenditures.sp/14)*365)%>%
  mutate(expenditures    = expenditures / 100)%>%
  mutate(expenditures.sp = expenditures.sp / 100)

# purchased items, wide-format

rt015.3 <- rt015.2 %>%
  select(-expenditures.sp)%>%
  filter(!(hh_id == 238119 & item == 81 & expenditures > 300))%>%
  spread(key = item, value = expenditures, fill = 0)

# self-produced items, wide-format

rt015.4 <- rt015.2 %>%
  select(-expenditures)%>%
  filter(!(hh_id == 238119 & item == 81))%>%
  spread(key = item, value = expenditures.sp, fill = 0) 

# rt016

rt016.1 <- remove_all_labels(rt016) %>%
  mutate(psu = formatC(psu, flag = "0", width = 3), sep = "")%>%
  mutate(hhold = formatC(hhold, flag = "0", width = 3), sep = "")%>%
  unite("hh_id", c("psu", "hhold"), sep = "" )%>%
  select(hh_id, item, s09b1w_2, s09b1w_3, s09b1w_5, s09b1w_6)%>%
  rename(W.1 = s09b1w_2, W.2 = s09b1w_5, T.1 = s09b1w_3, T.2 = s09b1w_6)%>%
  mutate(expenditures.1 = ifelse(T.1 == 1, W.1, 0))%>%
  mutate(expenditures.2 = ifelse(T.2 == 1, W.2, 0))%>%
  mutate(expenditures.sp.1 = ifelse(T.1 != 1, W.1, 0))%>%
  mutate(expenditures.sp.2 = ifelse(T.1 != 1, W.2, 0))%>%
  mutate(expenditures = expenditures.1 + expenditures.2)%>%
  mutate(expenditures.sp = expenditures.sp.1 + expenditures.sp.2)%>%
  select(hh_id, item, expenditures, expenditures.sp)%>%
  mutate(expenditures = expenditures*52/(2*100))%>%
  mutate(expenditures.sp = expenditures.sp*52/(2*100))

rt016.2 <- rt016.1 %>%
  select(-expenditures.sp)%>%
  spread(key = item, value = expenditures, fill = 0)

rt016.3 <- rt016.1 %>%
  select(-expenditures)%>%
  spread(key = item, value = expenditures.sp, fill = 0)

rt017.1 <- remove_all_labels(rt017) %>%
  mutate(psu = formatC(psu, flag = "0", width = 3), sep = "")%>%
  mutate(hhold = formatC(hhold, flag = "0", width = 3), sep = "")%>%
  unite("hh_id", c("psu", "hhold"), sep = "" )%>%
  select(hh_id, item, s09c1_q0, s09c1__1)%>%
  rename(expenditures = s09c1_q0, expenditures.sp = s09c1__1)%>%
  mutate(expenditures = expenditures*12)%>%
  mutate(expenditures.sp = expenditures.sp*12)

rt017.2 <- rt017.1 %>%
  select(-expenditures.sp)%>%
  spread(key = item, value = expenditures, fill = 0)

rt017.3 <- rt017.1 %>%
  select(-expenditures)%>%
  spread(key = item, value = expenditures.sp, fill = 0)

rt018.1 <- remove_all_labels(rt018) %>%
  mutate(psu = formatC(psu, flag = "0", width = 3), sep = "")%>%
  mutate(hhold = formatC(hhold, flag = "0", width = 3), sep = "")%>%
  unite("hh_id", c("psu", "hhold"), sep = "" )%>%
  select(hh_id, item, s09d1__1)%>%
  rename(expenditures = s09d1__1)%>%
  spread(key = item, value = expenditures, fill = 0)

rt019.1 <- remove_all_labels(rt019) %>%
  mutate(psu = formatC(psu, flag = "0", width = 3), sep = "")%>%
  mutate(hhold = formatC(hhold, flag = "0", width = 3), sep = "")%>%
  unite("hh_id", c("psu", "hhold"), sep = "" )%>%
  select(hh_id, item, s09d2_q0)%>%
  rename(expenditures = s09d2_q0)%>%
  filter(!(item == 384 & expenditures == 0))%>%
  spread(key = item, value = expenditures, fill = 0)%>%
  select(-"718")  #obviously a bug

# Merging Purchased and Self-Produced Items

Bangladesh.Purchased <- rt015.3 %>%
  left_join(rt016.2, by = "hh_id")%>%
  left_join(rt017.2, by = "hh_id")%>%
  left_join(rt018.1, by = "hh_id")%>%
  left_join(rt019.1, by = "hh_id")

# write_csv(Bangladesh.Purchased, "H:/OwnCloud/Action/1_Bangladesh_data/Data_Step_1/expenditures_items_Bangladesh.csv")

Bangladesh.Self.Produced <- rt015.4 %>%
  left_join(rt016.3, by = "hh_id") %>%
  left_join(rt017.3, by = "hh_id")

# write.csv(Bangladesh.Self.Produced, "expenditures_self_produced_Bangladesh.csv")

# Saving Data ####

#setwd("..")

# write_csv(Water.Code, "Water.Code.csv")
# write_csv(Education.Code, "Education.Code.csv")
# write_csv(Ethnicity.Code, "Ethnicity.Code.csv")
# write_csv(Electricity.Code, "Electricity.Code.csv")
# write_csv(Urban.Code, "Urban.Code.csv")
# write_csv(Toilet.Type.Code, "Toilet.Type.Code.csv")

# Output for distributional paper ####


appended <- left_join(rt001.12, Bangladesh.Purchased, by = "hh_id")%>%
  select(-X1.x, -X1.y)%>%
  gather(key = "item_code", value = "expenditures", "10":"553")%>%
  filter(expenditures != 0)

#setwd("")

# write_csv(appended, "appended_Bangladesh_January_2020.csv")


