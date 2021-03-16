# !diagnostics off

# First of all: datasets are already transformed ####

# Confidential Microdata

total     <- read_csv("totals.csv") 
household <- read_csv("household.raw.csv")
summary   <- read_csv("summary.hhold.csv") 
nonfood   <- read_csv("NFOODEXP_1.csv")
food      <- read_csv("FOODEXP_1.csv")

# Transforming Data ####

# setwd("")

h_1 <- household %>%
  select(w_id, hgc, members, ageless5, age5_17, toilet, electric, water)%>%
  rename(hh_id = w_id, education = hgc, hh_size = members, children.1 =ageless5, children.2 = age5_17, toilet.type = toilet, electricity.source = electric, water.access = water)
h_1$children.1[is.na(h_1$children.1)] <- 0
h_1$children.2[is.na(h_1$children.2)] <- 0

h_1 <- h_1 %>%
  mutate(children = children.1 + children.2)%>%
  mutate(children = ifelse(children > hh_size, hh_size, children))%>% # two households have to be adjusted, because otherwise there would be more children than people
  select(-children.1, -children.2)%>%
  mutate(adults = hh_size - children)%>% # adults as imputed
  select(hh_id, hh_size, adults, children, everything())%>%
  mutate(electricity.source = ifelse(electricity.source == 1, 1, 0))

# sum(t_1$pop1) # 104.578.989 people
# sum(t_1$pop2) # 104.981.331 people

s_1 <- summary %>% 
  select(w_id, urb, rfact)%>%
  rename(hh_id = w_id, urban = urb, hh_weights = rfact)%>%
  left_join(h_1, by = "hh_id")

# 104.578.989 people, 43 % rural, 57 % urban

s_1 <- s_1 %>%
  select(hh_id, hh_size, hh_weights, urban, everything())

# write_csv(s_1, "household_information_Philippines.csv")

nonfood_1 <- nonfood %>%
  select(W_ID, starts_with("C"), TOTHERDISB:TODISBOTHER, -starts_with("G"), - starts_with("K"))%>%
  rename(otherdisb = TOTHERDISB, realprop = TODISBREALPROP, cashloan = TODISBCASHLOAN, appliance = TODISBAPPLIANCE, perstransport = TODISBTRANSPORT,
         loans.outside = TODISBLOANSOUTSIDE, deposits = TODISBDEPOSITS, repair = TODISBMAJREPAIR, construction = TODISBCONSTRUCTION, otherdisbu = TODISBOTHER)%>%
  select(- starts_with("T"))%>%
  rename_at(vars(starts_with("C")), funs(sub("C", "", .)))

colnames(nonfood_1) <- tolower(colnames(nonfood_1))

nonfood_11 <- nonfood_1 %>%
  mutate_at(vars(c(alcohol:otherdisbu)), function(x){x = as.numeric(x)})%>%
  rename(hh_id = w_id)

nonfood_names <- as.data.frame(colnames(nonfood_11))

colnames(nonfood_names) <- "item"
nonfood_names <- nonfood_names %>%
  filter(item != "hh_id")%>%
  mutate(no = 1000:1296)

# write.xlsx(nonfood_names, "item_code.Philippines.xlsx")

colnames(nonfood_11) <- c("hh_id", seq(1000, 1296, by = 1))

food_1 <- food %>%
  select(W_ID, starts_with("C"))%>%
  rename_at(vars(starts_with("C")), funs(sub("C", "", .)))%>%
  mutate_at(vars(c(CEREAL:CANTEENMILITARY)),function(x){x = as.numeric(x)})%>%
  rename(hh_id = W_ID)

colnames(food_1) <- tolower(colnames(food_1))

food_names <- as.data.frame(colnames(food_1))

colnames(food_names) <- "item"
food_names <- food_names %>%
  filter(item != "hh_id")%>%
  mutate(no = 1:n())

# write.xlsx(food_names, "item.code.food.Philippines.xlsx")

colnames(food_1) <- c("hh_id", seq(1, 273, by = 1))

expenditures <- left_join(food_1, nonfood_11, by = "hh_id")

# write_csv(expenditures, "expenditures_items_Philippines.csv")

# Codierungen ####

setwd("")

Toilet.Code <- data.frame("toilet.type" = c(seq(0,7, by = 1)), "Label" = c("None", "Water-sealed, sewer septic tank, used exclusively", "Water-sealed, sewer septic tank, shared use", "Water-sealed, other depository, used exclusively", "Water-sealed, other depository, shared use", "closed pit", "open pit", "others"))
Water.Code <- data.frame("water.access" = c(seq(1,11, by = 1)), "Label" = c("Own use, faucet, community water system", "Shared, faucet, community water system", "Own use, tubed/piped deep well", "Shared, tubed/piped deep well", "tubed/piped shallow well", "dug well", "protected spting, river, stream", "unprotected spring, river, stream", "lake, river, rain and others", "Peddler", "Others"))
Urban.Code <- data.frame("urban" = c(1,2), "Urban" = c("Rural", "Urban"))
# write.csv(Toilet.Code, "Toilet.Code.Philippines.csv")
# write.csv(Water.Code, "Water.Code.Philippines.csv")
# write.csv(Urban.Code, "Urban.Code.Philippines.csv")
