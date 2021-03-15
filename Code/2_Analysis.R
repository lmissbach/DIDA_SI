# 0          General ####

# Authors: L. Missbach - missbach@mcc-berlin.net, L. Montrone - montrone@mcc-berlin.net, I. Dorband - dorband@mcc-berlin.net
# This Script is designed to homogenize the analysis of the household survey for different countries

# 1          Basic Information ####

Country.Name               <- "Indonesia"  # insert country name here
carbon.price               <- 40.00        # insert price in US-$ per ton of CO2
electricity.price.increase <- 0.25         # insert price increase here
decomposition.part         <- TRUE        # TRUE if you want the decomposition to happen

COUNTRY.NAME               <- toupper(Country.Name)
country.name               <- tolower(Country.Name)

# 1.0        Packages ####

library("foreign")
library("ggthemes")
library("ggsci")
library("Hmisc")
library("janitor")
library("labelled")
library("officer")
library("openxlsx")
library("quantreg")
library("rattle")
library("reshape2")
library("scales")
library("sjlabelled")
library("tidyverse")
library("utils")
library("wesanderson")
library("weights")
options(scipen=999)

# 1.01       Working-Directory ####

wd <- sprintf("../%s_analysis", Country.Name) # ADD Working Directory
setwd(wd)

# 1.1        Country Information ####

# We collect supplementary data in an additional excel sheet and import those information

information.ex                <- read.xlsx("../Exchange_Rates.xlsx", sheet = "exchange_rate_2014") # from World Bank

exchange.rate      <- as.numeric(information.ex[Country.Name]) # not ppp-adjusted

# 1.2        CPI-Adjustment (Inflation/Deflation) ####

cpis <- read.xlsx("../IMF_Consumer_Price_Index_Inflation_Average.xlsx")

cpis_0 <- cpis %>%
  select(Country, starts_with("2"))%>%
  filter(Country == "Bangladesh" | Country == "India" | Country == "Indonesia" | Country == "Pakistan" | Country == "Philippines" | Country == "Turkey" | Country == "Thailand" | Country == "Vietnam")

years <- data.frame(Country = c("Bangladesh", "India", "Indonesia", "Pakistan", "Philippines", "Thailand", "Turkey", "Vietnam"), Year = c(2010,2012,2018, 2015, 2015,2013, 2013, 2012))

cpis_1 <- cpis_0 %>%
  left_join(years, by = "Country")%>%
  mutate_at(vars('2010':'2018'), function(x) x = 1 + x/100)%>%
  rename_at(vars(starts_with("2")), list(~ str_replace(., "^", "Year_")))%>%
  rename(Value = Year)%>%
  mutate(inflation_factor = ifelse(Value == 2010, Year_2011*Year_2012*Year_2013*Year_2014, 
                                   ifelse(Value == 2012, Year_2013*Year_2014,
                                          ifelse(Value == 2013, Year_2014,
                                                 ifelse(Value == 2014, 1,
                                                        ifelse(Value == 2015, 1/Year_2015,
                                                               ifelse(Value == 2016, 1/(Year_2015*Year_2016),
                                                                      ifelse(Value == 2017, 1/(Year_2015*Year_2016*Year_2017),
                                                                             ifelse(Value == 2018, 1/(Year_2015*Year_2016*Year_2017*Year_2018), 0)))))))))

cpis_2 <- cpis_1 %>%
  select(Country, inflation_factor)

inflation_factor <- cpis_2$inflation_factor[cpis_2$Country == Country.Name]

# ____       ####
# 2          LOADING DATA ####
# 2.1        Matching Items with GTAP-Categories ####

# We insert the information of matched items to GTAP categories

# matching <- read.xlsx("../matching_results_all_countries.xlsx", sprintf("MATCHING_%s", COUNTRY.NAME), stringsAsFactors = FALSE)

# new matching for GTAP 10
matching <- read.xlsx("../Item_GTAP_Concordance.xlsx", sprintf("MATCHING_%s", COUNTRY.NAME))

matching <- matching %>%
  select (-Explanation) %>%
  gather (key = drop, value = item_code, 2:(ncol(matching)-1), na.rm = TRUE) %>%
  select (GTAP, item_code) %>%
  arrange(GTAP)

if(Country.Name == "Thailand"){
  matching <- matching %>%
    mutate_at(vars(item_code), function(x)(x = toupper(x)))
}

if(Country.Name == "Vietnam"){
  matching <- matching %>%
    mutate(item_code = as.character(item_code))
}

matching.check <- count(matching, item_code)%>%
  filter(n != 1)

if(nrow(matching.check) != 0) (paste("WARNING! Item-Codes existing with two different GTAP-categories in Excel-File"))

rm(matching.check)

# 2.2        Insert specific CSV-File ####

# We load the specific Household-CSV-File. INSERT RAW DATA

if(Country.Name == "Bangladesh")  Household <- read_csv(".. .csv") # INSERT CLEANED RAW DATA

if(Country.Name == "India")       Household <- read_csv(".. .csv") # INSERT CLEANED RAW DATA

# Run Cleaning_Indonesia.R
if(Country.Name == "Indonesia")   Household <- read_csv(".. .csv") # INSERT CLEANED RAW DATA

if(Country.Name == "Pakistan")    Household <- read_csv(".. .csv") # INSERT CLEANED RAW DATA

if(Country.Name == "Philippines") Household <- read_csv(".. .csv") # INSERT CLEANED RAW DATA

if(Country.Name == "Thailand")    Household <- read_csv(".. .csv") # INSERT CLEANED RAW DATA

if(Country.Name == "Turkey")      Household <- read_csv(".. .csv") # INSERT CLEANED RAW DATA

if(Country.Name == "Vietnam")     Household <- read_csv(".. .csv") # INSERT CLEANED RAW DATA


# 2.3        Create Virtual ####

# We include our Output from I-O-Analysis!

# GTAP 10 (adjusted as of June 2020)

GTAP.10         <- read.csv2("../2_GTAP/GTAP10.csv", stringsAsFactors = FALSE)%>%
  select(-Number, -Explanation)
Electricity.10  <- read.csv(sprintf("../2_GTAP/all_results_embeded_GTAP_10_2014_re/%s/%s_embeded_electricity_dollar_re.csv", Country.Name, country.name), header = FALSE, col.names = "Electricity_MUSD")
Emissions.10    <- read.csv(sprintf("../2_GTAP/all_results_embeded_GTAP_10_2014_re/%s/total_CO2_emissions_hh_%s_gtap10_2014_re.csv", Country.Name, country.name), header = FALSE, col.names = "CO2_Mt")
Emissions.in.10 <- read.csv(sprintf("../2_GTAP/all_results_embeded_GTAP_10_2014_re/%s/total_CO2_emissions_hh_%s_from_%s_gtap10_2014_re.csv", Country.Name, country.name, country.name), header = FALSE, col.names = "CO2_Mt_from_within")

Virtual         <- data.frame(GTAP.10, Emissions.10, Emissions.in.10, Electricity.10)

rm(Electricity.10, Emissions.10, Emissions.in.10)

# 2.4        Calculate Carbon Intensities ####

consumption_1 <- read_excel("../2_GTAP/VDPM.xlsx")
consumption_2 <- read_excel("../2_GTAP/VIPM.xlsx")

consumption_1.1 <- consumption_1 %>%
  separate(VDPM, c("number", "GTAP"), sep = " ")%>%
  filter(number != "Total")%>%
  select(-number)%>%
  gather(key = Country, value = expenditures_dom, Indonesia:Turkey)

consumption_2.1 <- consumption_2 %>%
  separate(VIPM, c("number", "GTAP"), sep = " ")%>%
  filter(number != "Total")%>%
  select(-number)%>%
  gather(key = Country, value = expenditures_int, Indonesia:Turkey)

consumption_0 <- left_join(consumption_1.1, consumption_2.1, by = c("Country", "GTAP"))%>%
  mutate(expenditures_total = expenditures_dom + expenditures_int)%>%
  mutate(expenditures_USD = expenditures_total*1000000)

consumption_0.0 <- consumption_0 %>%
  filter(Country == Country.Name)%>%
  select(GTAP, expenditures_USD)

consumption_0.1 <- consumption_0.0 %>%
  left_join(Virtual, by = "GTAP")%>%
  mutate(CO2_t    = CO2_Mt            *1000000,
         CO2_wi_t = CO2_Mt_from_within*1000000,
         Ely_USD  = Electricity_MUSD  *1000000)%>%
  select(GTAP, expenditures_USD, CO2_t, CO2_wi_t, Ely_USD)

# Merging Gas and GDT
consumption_0.1.1 <- consumption_0.1 %>%
  filter(GTAP != "gas" & GTAP != "gdt")

consumption_0.1.2 <- consumption_0.1 %>%
  filter(GTAP == "gas" | GTAP == "gdt")%>%
  summarise(
    GTAP = "gasgdt",
    expenditures_USD = sum(expenditures_USD),
    CO2_t = sum(CO2_t),
    CO2_wi_t = sum(CO2_wi_t),
    Ely_USD = sum(Ely_USD)
  )

consumption_0.1 <- rbind(consumption_0.1.1, consumption_0.1.2)

consumption_00 <- consumption_0.1 %>%
  select(GTAP, expenditures_USD)

consumption_0.2 <- consumption_0.1 %>%
  mutate(CO2_intensity_t_per_dollar         = CO2_t   /expenditures_USD,
         CO2_within_intensity_t_per_dollar  = CO2_wi_t/expenditures_USD,
         Ely_USD_intensity_dollar_per_dollar = Ely_USD /expenditures_USD)

intensities <- consumption_0.2 %>%
  select(GTAP, CO2_intensity_t_per_dollar, CO2_within_intensity_t_per_dollar, Ely_USD_intensity_dollar_per_dollar)

# ____       ####
# 3          TRANSFORMING DATA ####

# 3.01       Consistent naming for Bangladesh ####

if(Country.Name == "Bangladesh"){

  Urban.New <- data.frame("urban" = c(1,2), "Urban" = c("Rural", "Urban"))
  
  Household <- Household %>%
    mutate(hh_id = as.numeric(hh_id))%>%
    rename(hh_weights = hh_weigths)%>%
    mutate(expenditures_selfproduced = 0)
}

# 3.02       Consistent naming for India ####

if(Country.Name == "India"){
   Household <- Household
  
   Urban.New <- count(Household, urban, Urban)%>%
    select(-n)
}

# 3.03       Consistent naming for Indonesia ####

if(Country.Name == "Indonesia"){
  Household <- Household %>%
    select(hh_id, hh_size, adults, children, hh_weights, urban, item_code, expenditures)%>%
    mutate(expenditures_selfproduced = NA)

  Urban.New <- data.frame("urban" = c(2,1), "Urban" = c("Rural", "Urban"))
}

# 3.04       Consistent naming for Pakistan ####

if (Country.Name == "Pakistan"){
  Household <- Household %>%
    rename(hh_id = hhcode, item_code = item_no, expenditures = exp, hh_size = hhsize, hh_weights = weights)%>%
    mutate(expenditures_selfproduced = 0,
           electricity_source        = as.numeric(0),
           adults                    = hh_size,
           children                  = 0)
  
Urban.Check <- count(Household, urban)
  
Urban.New <- data.frame("urban" = (c(2,1)), "Urban" = c("Urban", "Rural"))
}

# 3.05       Consistent naming for Philippines ####

if(Country.Name == "Philippines"){
  Household_1 <- Household %>%
    select(hh_id, hh_size, hh_weights, urban, adults, children)
  
  Expenditures_1 <- Expenditures %>%
    gather(key = "item_code", value = "expenditures", '1':'1296')%>%
    filter(expenditures != 0)%>%
    mutate(item_code = as.numeric(item_code))
  
  Household_1 <- Expenditures_1 %>%
    left_join(Household_1, by = "hh_id")%>%
    select(hh_id, hh_size, hh_weights, urban, adults, children, item_code, expenditures)%>%
    mutate(expenditures_selfproduced = 0)
  
  Urban.New <- data.frame(urban = c(1,2), Urban = c("Rural", "Urban"))
  
  Household <- Household_1

}

# 3.06       Consistent naming for Thailand ####

if(Country.Name == "Thailand"){

  Household <- Household %>%
    mutate(expenditures_selfproduced = 0)
  
  T.X <- Household %>%
    select(hh_id, expenditures_survey, income_survey, hh_size, hh_weights)%>%
    rename(expenditures = expenditures_survey, income = income_survey)
  
  Urban.New <- data.frame("urban" = (c(1,2)), "Urban" = c("Urban", "Rural"), stringsAsFactors = FALSE)
}

# 3.07       Consistent naming for Turkey #####

if(Country.Name == "Turkey"){
  H.1 <- H.1 %>%
    rename(hh_id = BULTEN, type = TABNO, item_code = HBS_KOD, expenditures = DEGERD)%>%
    mutate(expenditures = as.numeric(expenditures))%>%
    mutate(expenditures = expenditures*12)%>% #was listed monthly before
    filter(item_code != 9122)%>%
    mutate(expenditures_selfproduced = ifelse(type == 2, expenditures, 0))%>%
    mutate(expenditures = ifelse(type == 1, expenditures, 0))%>%
    group_by(hh_id, item_code)%>%
    summarise(
      expenditures = sum(expenditures),
      expenditures_selfproduced = sum(expenditures_selfproduced)
    )%>%
    ungroup()
  
  H.2 <- H.2 %>%
    rename(hh_id = BULTEN)%>%
    select(-(HHTIPI:OTELKDEG))%>%
    rename(income = YILKULGEL, expenditures_0 = HARCAMA, hh_size = HHB, urban = KIRKNTKD, hh_weights = faktor)%>%
    mutate(hh_weights = as.character(hh_weights))%>%
    mutate(hh_weights = as.numeric(hh_weights))
  
  T.X <- H.2 %>%
    select(hh_id, income, EFB_OECD, hh_size, hh_weights)%>%
    mutate(income = as.character(income))%>%
    mutate(income = as.numeric(income))
  
  H.2 <- H.2 %>%
    select(-income, -expenditures_0, -EFB_OECD)
    
  H.3 <- H.3 %>%
    rename(hh_id = BULTEN, age = YAS)%>%
    mutate(child = ifelse(age < 18,1,0))%>%
    mutate(adult = ifelse(age >= 18,1,0))%>%
    select(hh_id, child, adult)%>%
    group_by(hh_id)%>%
    summarise(
      adults = sum(adult),
      children = sum(child)
      )%>%
    ungroup()
  
  Household <- left_join(H.1, H.2, by = "hh_id")%>%
    left_join(H.3, by = "hh_id")%>%
    mutate(electricity_source = 0)

  Urban.New <- data.frame("urban" = (c(1,2)), "Urban" = c("Rural", "Urban"))
}

# 3.08       Consistent naming for Vietnam ####

if(Country.Name == "Vietnam"){
  Household <- Household %>%
    select(-ethnicity, -drinking.water, -lighting.source, -toilet.type, - education)%>%
    mutate(item_code = as.character(item_code))%>%
    mutate(expenditures_selfproduced = 0)
  
  Urban.New <- data.frame("urban" = c(1,2), "Urban" = c("Urban", "Rural"))
}

# 3.1        Selection of variables ####

# We drop those variables that are not needed for further analysis

Household <- Household %>%
  select(hh_id, hh_size, hh_weights, urban, adults, children, item_code, expenditures, expenditures_selfproduced)

# 3.2        Clearing NAs ####

# we clear our dataset in terms of NAs

 observations.before <- nrow(Household)
Household            <- Household %>% filter(!is.na(hh_id))
 observations.1      <- nrow(Household)
Household            <- Household %>% filter(!is.na(hh_weights))
 observations.2      <- nrow(Household)
Household            <- Household %>% filter(!is.na(hh_size))
 observations.3      <- nrow(Household)
Household            <- Household %>% filter(!is.na(expenditures)) # due to Indias specifics
 observations.3a     <- nrow(Household)
 
print(paste0("This step was meant to delete NAs in hh_id, hh_weights and hh_size and expenditures. There have been ", observations.before - observations.1, " NAs for hh_ID, ", observations.1 - observations.2, " NAs for hh_weights and ", observations.2 - observations.3, " NAs for hh_size and ", observations.3 - observations.3a, " NAs for expenditures. In total, ", observations.before - observations.3a, " observations have been deleted."))
 

# 3.3        Merging Household and matching ####

Household.Merge.0 <- left_join(Household, matching, by = "item_code")

observations.4 <- nrow(Household.Merge.0)

if(Country.Name == "Thailand"){
  Household.Merge.0 <- Household.Merge.0 %>%
    mutate(expenditures_selfproduced = ifelse(GTAP == "in-kind", expenditures, 0),
           expenditures              = ifelse(GTAP == "in-kind", 0, expenditures))
}

# 3.3.1      Binning with expenditures_0 ####

Household.Binning <- Household.Merge.0 %>%
  filter(GTAP != "deleted")%>%
  group_by(hh_id)%>%
  mutate(hh_expenditures_0   = sum(expenditures))%>%
  ungroup()%>%
  mutate(hh_expenditures_0_pc = hh_expenditures_0/hh_size)%>%
  select(hh_id, hh_expenditures_0,hh_expenditures_0_pc, hh_weights, urban, hh_size)%>%
  filter(!duplicated(hh_id))%>%
  mutate(Income_Group_5   = as.numeric(binning(hh_expenditures_0_pc, bins=5,   method=c("wtd.quantile"), labels=seq(1,5,length.out=5),     weights = hh_weights)))%>%
  mutate(Income_Group_10  = as.numeric(binning(hh_expenditures_0_pc, bins=10,  method=c("wtd.quantile"), labels=seq(1,10,length.out=10),   weights = hh_weights)))%>%
  mutate(Income_Group_100 = as.numeric(binning(hh_expenditures_0_pc, bins=100, method=c("wtd.quantile"), labels=seq(1,100,length.out=100), weights = hh_weights)))

# This is because we find few households with absolutely no expenditures. No expenditures, no footprint.
Household.Binning$hh_expenditures_0_pc[Household.Binning$hh_expenditures_0_pc == 0] <- 1

Expenditure.Analysis <- Household.Binning

# Binning for urban and rural each

Household.Binning.R <- Household.Binning %>%
  select(hh_id, hh_expenditures_0_pc, hh_weights, urban, hh_size)%>%
  filter(urban == 1)%>%
  mutate(Income_Group_5_UR = as.numeric(binning(hh_expenditures_0_pc, bins=5,   method=c("wtd.quantile"), labels=seq(1,5,length.out=5),     weights = hh_weights)))
  
Household.Binning.U <- Household.Binning %>%
  select(hh_id, hh_expenditures_0_pc, hh_weights, urban, hh_size)%>%
  filter(urban == 2)%>%
  mutate(Income_Group_5_UR = as.numeric(binning(hh_expenditures_0_pc, bins=5,   method=c("wtd.quantile"), labels=seq(1,5,length.out=5),     weights = hh_weights)))
  
Household.Binning.UR <- rbind(Household.Binning.R, Household.Binning.U)

# Household.Binning.UR includes Income Group after having separated between urban/rural

Household.Binning <- Household.Binning %>%
  select(hh_id, Income_Group_5, Income_Group_10, hh_expenditures_0_pc, hh_weights, urban, hh_size)%>%
  arrange(hh_id)
  
Household.Info <- Household.Binning
  # adding shares for urban status for 5 bins

Household.Binning <- Household.Binning %>%
  group_by(Income_Group_5, urban)%>%
  mutate(p_c_weights = sum(hh_weights*hh_size))%>%
  ungroup()%>%
  group_by(Income_Group_5)%>%
  mutate(p_c_weights_IG = sum(hh_weights*hh_size))%>%
  ungroup()%>%
  mutate(share_urban_5 = p_c_weights/p_c_weights_IG)%>%
  select(-p_c_weights, - p_c_weights_IG)%>%
  
  # addings share for urban status for 10 bins
  
  group_by(Income_Group_10, urban)%>%
  mutate(p_c_weights = sum(hh_weights*hh_size))%>%
  ungroup()%>%
  group_by(Income_Group_10)%>%
  mutate(p_c_weights_IG = sum(hh_weights*hh_size))%>%
  ungroup()%>%
  mutate(share_urban_10 = p_c_weights/p_c_weights_IG)%>%
  select(-p_c_weights, - p_c_weights_IG, -hh_weights, -hh_size)

  # for checking purposes

Household.Binning.0 <- Household.Binning %>%
  select(hh_id, Income_Group_5, Income_Group_10)

Household.Binning.100 <- Expenditure.Analysis %>%
  select(hh_id, Income_Group_100)

  # Thailand Special Treatment: Binning with predefined Expenditures
if(Country.Name == "Thailand"){
  Household.Binning.THAI <- Household.Merge.0 %>%
    select(hh_id)     %>%
    filter(!duplicated(hh_id))             %>%
    left_join(T.X, by = "hh_id") %>%
    mutate(expenditures_pc  = expenditures/hh_size)%>%
    mutate(income_pc = income/hh_size)%>%
    mutate(Income_Group_5_T1  = as.numeric(binning(expenditures_pc, bins=5,  method=c("wtd.quantile"), labels=seq(1,5,length.out=5),   weights = hh_weights)))%>%
    mutate(Income_Group_10_T1 = as.numeric(binning(expenditures_pc, bins=10, method=c("wtd.quantile"), labels=seq(1,10,length.out=10), weights = hh_weights)))%>%
    mutate(Income_Group_5_T2  = as.numeric(binning(income_pc, bins=5,  method=c("wtd.quantile"), labels=seq(1,5,length.out=5),   weights = hh_weights)))%>%
    mutate(Income_Group_10_T2 = as.numeric(binning(income_pc, bins=10, method=c("wtd.quantile"), labels=seq(1,10,length.out=10), weights = hh_weights)))
  }

 # Turkey Special Treatment: Binning with Income
if(Country.Name == "Turkey"){
  Household.Binning.Turkey <- Household.Merge.0 %>%
    select(hh_id)%>%
    filter(!duplicated(hh_id))%>%
    left_join(T.X, by = "hh_id")%>%
    mutate(expenditures_0_pc = income/hh_size)%>%
    mutate(Income_Group_5_T  = as.numeric(binning(expenditures_0_pc, bins=5,  method=c("wtd.quantile"), labels=seq(1,5,length.out=5),   weights = hh_weights)))%>%
    mutate(Income_Group_10_T = as.numeric(binning(expenditures_0_pc, bins=10, method=c("wtd.quantile"), labels=seq(1,10,length.out=10), weights = hh_weights)))
  }

#  3.3.2.1   Binning with expenditures + expenditures selfproduced ####

# in this section we assign bins to the households based on expenditures as well as on self-produced (imputed) expenditures

#Household.Binning.SP <- Household.Merge.0 %>%
#  filter(GTAP != "deleted")%>%
#  mutate(expenditures_incl_sp = expenditures + expenditures_selfproduced)%>%
#  group_by(hh_id)%>%
#   mutate(expenditures_incl_sp_0 = sum(expenditures_incl_sp))%>%
#  ungroup()%>%
#  mutate(hh_expenditures_incl_sp_0_pc = expenditures_incl_sp_0/hh_size)%>%
#  select(hh_id, expenditures_incl_sp_0, hh_expenditures_incl_sp_0_pc, hh_weights, urban, hh_size)%>%
#  filter(!duplicated(hh_id))%>%
#  mutate(Income_Group_5_sp  = as.numeric(binning(hh_expenditures_incl_sp_0_pc, bins=5,  method=c("wtd.quantile"), labels=seq(1,5,length.out=5),   weights = hh_weights)))%>%
#  mutate(Income_Group_10_sp = as.numeric(binning(hh_expenditures_incl_sp_0_pc, bins=10, method=c("wtd.quantile"), labels=seq(1,10,length.out=10), weights = hh_weights)))%>%
#  select(hh_id, Income_Group_5_sp, Income_Group_10_sp, hh_expenditures_incl_sp_0_pc, hh_weights, urban, hh_size)%>%
#  arrange(hh_id)%>%
#  
## adding shares for urban status for 5 bins
#
#  group_by(Income_Group_5_sp, urban)%>%
#  mutate(p_c_weights = sum(hh_weights*hh_size))%>%
#  ungroup()%>%
#  group_by(Income_Group_5_sp)%>%
#  mutate(p_c_weights_IG = sum(hh_weights*hh_size))%>%
#  ungroup()%>%
#  mutate(share_urban_5_sp = p_c_weights/p_c_weights_IG)%>%
#  select(-p_c_weights, - p_c_weights_IG)%>%
#  
#  # addings share for urban status for 10 bins
#  
#  group_by(Income_Group_10_sp, urban)%>%
#  mutate(p_c_weights = sum(hh_weights*hh_size))%>%
#  ungroup()%>%
#  group_by(Income_Group_10_sp)%>%
#  mutate(p_c_weights_IG = sum(hh_weights*hh_size))%>%
#  ungroup()%>%
#  mutate(share_urban_10_sp = p_c_weights/p_c_weights_IG)%>%
#  select(-p_c_weights, - p_c_weights_IG, -urban)

#  3.3.2.2   Binning without expenditures for dwellings ####

# in this section we assign bins to the households based on the expenditures without "dwe"

#Household.Binning.DWE <- Household.Merge.0 %>%
#  filter(GTAP != "deleted")%>%
#  filter(GTAP != "dwe")%>%
#  group_by(hh_id)%>%
#  mutate(hh_expenditures_0   = sum(expenditures))%>%
#  ungroup()%>%
#  mutate(hh_expenditures_0_pc = hh_expenditures_0/hh_size)%>%
#  select(hh_id, hh_expenditures_0,hh_expenditures_0_pc, hh_weights, hh_size, urban)%>%
#  filter(!duplicated(hh_id))%>%
#  mutate(Income_Group_5  = as.numeric(binning(hh_expenditures_0_pc, bins=5,  method=c("wtd.quantile"), labels=seq(1,5,length.out=5),   weights = hh_weights)))%>%
#  mutate(Income_Group_10 = as.numeric(binning(hh_expenditures_0_pc, bins=10, method=c("wtd.quantile"), labels=seq(1,10,length.out=10), weights = hh_weights)))%>%
#  select(hh_id, Income_Group_5, Income_Group_10, hh_expenditures_0, hh_expenditures_0_pc, hh_weights, hh_size, urban)%>%
#  arrange(hh_id)

#  3.3.3     Binning Check (SELF-PRODUCED items) ####

# this section serves checking-purposes: does including self-produced items while binning make a remarkable difference?

#Binning.Check <- left_join(Household.Binning, Household.Binning.SP, by = "hh_id")%>%
#  mutate(population = hh_size*hh_weights)

#  3.3.3.1   Binning Check with 5 Bins ####

# How many people changed their Income Group due to including self-produced items?

#Binning.Check.5 <- Binning.Check %>%
#  select(-share_urban_5, -share_urban_5_sp, -share_urban_10, -share_urban_10_sp, -urban, -hh_size, -Income_Group_10, -Income_Group_10_sp)%>%
#  mutate(changed_IG = Income_Group_5_sp - Income_Group_5)%>%
#  mutate(changed = ifelse(changed_IG == 0,0,1))%>%
#  mutate(changed = changed*population)
#
#print(paste0("By including the imputed expenditures for self-produced items ", round((sum(Binning.Check.5$changed)/sum(Binning.Check.5$population)),3)*100, " % of the population changed its Income Group."))
#
## Corresponding Graphical Analysis
#
#Binning.Check.5.1 <- Binning.Check.5 %>%
#  select(-hh_expenditures_0_pc, -hh_expenditures_incl_sp_0_pc, -changed_IG, -changed)%>%
#  group_by(Income_Group_5_sp, Income_Group_5)%>%
#  summarise(
#    affected_hh  = sum(hh_weights),
#    affected_pop = sum(population)
#  )%>%
#  ungroup()
#
## Households
#P.3.3.3.1a <- ggplot(Binning.Check.5.1, aes(x = factor(Income_Group_5_sp), y = affected_hh,  fill = as.factor(Income_Group_5))) + geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.75, color = "black") + theme_bw() + xlab("Income Group based on Expenditures incl. Self-Produced Items") + ylab ("Households") + scale_y_continuous(labels = function(x)format(x, decimal.mark = ",", big.mark = ".", scientific = FALSE)) + labs(fill = "Income Group with \ndirect expenditures only") + scale_fill_brewer(palette = "Pastel1")
## Population
#P.3.3.3.1b <- ggplot(Binning.Check.5.1, aes(x = factor(Income_Group_5_sp), y = affected_pop, fill = as.factor(Income_Group_5))) + geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.75, color = "black") + theme_bw() + xlab("Income Group based on Expenditures incl. Self-Produced Items") + ylab ("Population") + scale_y_continuous(labels = function(x)format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) + labs(fill = "Income Group with \ndirect expenditures only") + scale_fill_brewer(palette = "Pastel1")

#  3.3.3.2   Binning Check with 10 Bins ####

#Binning.Check.10 <- Binning.Check %>%
#  select(-share_urban_5, -share_urban_5_sp, -share_urban_10, -share_urban_10_sp, -urban, -hh_size, -Income_Group_5, -Income_Group_5_sp)%>%
#  mutate(changed_IG = Income_Group_10_sp - Income_Group_10)%>%
#  mutate(changed = ifelse(changed_IG == 0,0,1))%>%
#  mutate(changed = changed*population)
#
#print(paste0("By including the imputed expenditures for self-produced items ", round((sum(Binning.Check.10$changed)/sum(Binning.Check.10$population)),3)*100, " % of the population changed its Income Group."))
#
## Corresponding Graphical Analysis
#
#Binning.Check.10.1 <- Binning.Check.10 %>%
#  select(-hh_expenditures_0_pc, -hh_expenditures_incl_sp_0_pc, -changed_IG, -changed)%>%
#  group_by(Income_Group_10_sp, Income_Group_10)%>%
#  summarise(
#    affected_hh = sum(hh_weights),
#    affected_pop = sum(population)
#  )%>%
#  ungroup()
#
## Households
#P.3.3.3.2a <- ggplot(Binning.Check.10.1, aes(x = factor(Income_Group_10_sp), y = affected_hh,  fill = as.factor(Income_Group_10))) + geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.75, color = "black") + theme_bw() + xlab("Income Group based on Expenditures incl. Self-Produced Items") + ylab ("Households") + scale_y_continuous(labels = function(x)format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) + labs(fill = "Income Group with \ndirect expenditures only") + scale_fill_brewer(palette = "Set3")
## Population
#P.3.3.3.2b <- ggplot(Binning.Check.10.1, aes(x = factor(Income_Group_10_sp), y = affected_pop, fill = as.factor(Income_Group_10))) + geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.75, color = "black") + theme_bw() + xlab("Income Group based on Expenditures incl. Self-Produced Items") + ylab ("Population") + scale_y_continuous(labels = function(x)format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) + labs(fill = "Income Group with \ndirect expenditures only") + scale_fill_brewer(palette = "Set3")

#  3.3.3.3   Binning Check incl. Factor urban ####

# Who changed their Income Group (including urban/rural)?

# Binning.Check.U.5 <- Binning.Check %>%
#   select(Income_Group_5, Income_Group_5_sp, urban, hh_weights, population)%>%
#   mutate(changed_IG    = Income_Group_5_sp - Income_Group_5)%>%
#   mutate(changed_IG    = ifelse (changed_IG == 0, 0, 1))%>%
#   mutate(changed_Y_hh  = hh_weights*changed_IG)%>%
#   mutate(changed_Y_pop = population*changed_IG)%>%
#   mutate(changed_IG    = ifelse(changed_IG == 0, 1, 0))%>%
#   mutate(changed_N_hh  = hh_weights*changed_IG)%>%
#   mutate(changed_N_pop = population*changed_IG)%>%
#   select(- hh_weights, - population, - changed_IG)%>%
#   group_by(urban)%>%
#   summarise(
#     changed_HH      = sum(changed_Y_hh),
#     changed_pop     = sum(changed_Y_pop),
#     not_changed_HH  = sum(changed_N_hh),
#     not_changed_pop = sum(changed_N_pop)
#   )%>%
#   ungroup()
# 
# # Analysis for population
# 
# Binning.Check.U.5.1 <- Binning.Check.U.5 %>%
#   select(-changed_HH, -not_changed_HH)%>%
#   rename(changed     = changed_pop)%>%
#   rename(not_changed = not_changed_pop)
# 
# Binning.Check.U.5.1 <- melt(Binning.Check.U.5.1, id.vars = "urban")
# 
# Binning.Check.U.5.1 <- left_join(Binning.Check.U.5.1, Urban.New, by = "urban" )
# 
# P.3.3.3.3a <- ggplot(Binning.Check.U.5.1, aes(y = value, x = Urban, fill = variable))+ geom_col(color = "black", width = 0.5, position = position_dodge((0.86))) + theme_bw()+ xlab ("Area") + ylab("Population") + scale_y_continuous(labels = function(x)format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))  + scale_fill_brewer(palette = "Pastel1", labels = c("changed", "changed not")) + labs(fill = "Variation of Income Group")
# 
# # Analysis for households
# 
# Binning.Check.U.5.2 <- Binning.Check.U.5 %>%
#   select(-changed_pop, -not_changed_pop)%>%
#   rename(changed     = changed_HH)%>%
#   rename(not_changed = not_changed_HH)
# 
# Binning.Check.U.5.2 <- melt(Binning.Check.U.5.2, id.vars = "urban")
# 
# Binning.Check.U.5.2 <- left_join(Binning.Check.U.5.2, Urban.New, by = "urban" )
# 
# P.3.3.3.3b <- ggplot(Binning.Check.U.5.2, aes(y = value, x = Urban, fill = variable))+ geom_col(color = "black", width = 0.5, position = position_dodge((0.86))) + theme_bw()+ xlab ("Area") + ylab("Households") + scale_y_continuous(labels = function(x)format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) + labs(fill = "Variation of Income Group") + scale_fill_brewer(palette = "Pastel1", labels = c("changed", "changed not"))
# 
# # remove Binning Checks
# 
# rm(Binning.Check, Binning.Check.10, Binning.Check.10.1, Binning.Check.5, Binning.Check.5.1, Binning.Check.U.5, Binning.Check.U.5.1, Binning.Check.U.5.2)

#  3.3.3.TH  Binning Check for Thailand only ####

# if(Country.Name == "Thailand"){
# Binning.Check.T <- left_join(Household.Binning.THAI, Household.Binning.0, by = "hh_id")%>%
#   mutate(population    = hh_size*hh_weights)%>%
#   mutate(changed_IG_5_1  = Income_Group_5_T1 - Income_Group_5)%>%
#   mutate(changed_IG_10_1 = Income_Group_10_T1 - Income_Group_10)%>%
#   mutate(changed_5_1     = ifelse(changed_IG_5_1 == 0,0,1))%>%
#   mutate(changed_5_1     = changed_5_1*population)%>%
#   mutate(changed_10_1    = ifelse(changed_IG_10_1 == 0,0,1))%>%
#   mutate(changed_10_1    = changed_10_1*population)%>%
#   mutate(changed_IG_5_2  = Income_Group_5_T2 - Income_Group_5)%>%
#   mutate(changed_IG_10_2 = Income_Group_10_T2 - Income_Group_10)%>%
#   mutate(changed_5_2     = ifelse(changed_IG_5_2 == 0,0,1))%>%
#   mutate(changed_5_2     = changed_5_2*population)%>%
#   mutate(changed_10_2    = ifelse(changed_IG_10_2 == 0,0,1))%>%
#   mutate(changed_10_2    = changed_10_2*population)
# 
# print(paste0("By using the average income coming directly from the survey ", round((sum(Binning.Check.T$changed_5_1)/sum(Binning.Check.T$population)),3)*100, " % of the population changed its Income Group."))
# print(paste0("By using the average expenditures coming directly from the survey ", round((sum(Binning.Check.T$changed_5_2)/sum(Binning.Check.T$population)),3)*100, " % of the population changed its Income Group."))
# 
# Binning.Check.T.5 <- Binning.Check.T %>%
#   group_by(Income_Group_5_T1, Income_Group_5)%>%
#   summarise(
#     affected_hh  = sum(hh_weights),
#     affected_pop = sum(population)
#   )%>%
#   ungroup()
# 
# # Households
# P.3.3.3.T.1a <- ggplot(Binning.Check.T.5, aes(x = factor(Income_Group_5_T1), y = affected_hh,  fill = as.factor(Income_Group_5))) + geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.75, color = "black") + theme_bw() + xlab("Income Group based on Income coming directly from Survey") + ylab ("Households") + scale_y_continuous(labels = function(x)format(x, decimal.mark = ",", big.mark = ".", scientific = FALSE)) + labs(fill = "Income Group with \ndirect expenditures only") + scale_fill_brewer(palette = "Pastel1")
# # Population
# P.3.3.3.T.1b <- ggplot(Binning.Check.T.5, aes(x = factor(Income_Group_5_T1), y = affected_pop, fill = as.factor(Income_Group_5))) + geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.75, color = "black") + theme_bw() + xlab("Income Group based on Income coming directly from Survey") + ylab ("Population") + scale_y_continuous(labels = function(x)format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) + labs(fill = "Income Group with \ndirect expenditures only") + scale_fill_brewer(palette = "Pastel1")
# 
# Binning.Check.T.10 <- Binning.Check.T %>%
#   group_by(Income_Group_10_T1, Income_Group_10)%>%
#   summarise(
#     affected_hh  = sum(hh_weights),
#     affected_pop = sum(population)
#   )%>%
#   ungroup()
# 
# # Households
# P.3.3.3.T.2a <- ggplot(Binning.Check.T.10, aes(x = factor(Income_Group_10_T1), y = affected_hh,  fill = as.factor(Income_Group_10))) + geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.75, color = "black") + theme_bw() + xlab("Income Group based on Income coming directly from Survey") + ylab ("Households") + scale_y_continuous(labels = function(x)format(x, decimal.mark = ",", big.mark = ".", scientific = FALSE)) + labs(fill = "Income Group with \ndirect expenditures only") + scale_fill_brewer(palette = "Pastel1")
# # Population
# P.3.3.3.T.2b <- ggplot(Binning.Check.T.10, aes(x = factor(Income_Group_10_T1), y = affected_pop, fill = as.factor(Income_Group_10))) + geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.75, color = "black") + theme_bw() + xlab("Income Group based on Income coming directly from Survey") + ylab ("Population") + scale_y_continuous(labels = function(x)format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) + labs(fill = "Income Group with \ndirect expenditures only") + scale_fill_brewer(palette = "Pastel1")
# }
# 
#  3.3.3.TU  Binning Check for Turkey only ####

# if(Country.Name == "Turkey"){
#   Binning.Check.T <- left_join(Household.Binning.Turkey, Household.Binning.0, by = "hh_id")%>%
#     mutate(population    = hh_size*hh_weights)%>%
#     mutate(changed_IG_5  = Income_Group_5_T - Income_Group_5)%>%
#     mutate(changed_IG_10 = Income_Group_10_T - Income_Group_10)%>%
#     mutate(changed_5     = ifelse(changed_IG_5 == 0,0,1))%>%
#     mutate(changed_5     = changed_5*population)%>%
#     mutate(changed_10    = ifelse(changed_IG_10 == 0,0,1))%>%
#     mutate(changed_10    = changed_10*population)
#   
#   print(paste0("By using the average expenditures coming directly from the survey ", round((sum(Binning.Check.T$changed_5)/sum(Binning.Check.T$population)),3)*100, " % of the population changed its Income Group."))
#   
#   Binning.Check.T.5 <- Binning.Check.T %>%
#     group_by(Income_Group_5_T, Income_Group_5)%>%
#     summarise(
#       affected_hh  = sum(hh_weights),
#       affected_pop = sum(population)
#     )%>%
#     ungroup()
#   
#   # Households
#   P.3.3.3.T.1a <- ggplot(Binning.Check.T.5, aes(x = factor(Income_Group_5_T), y = affected_hh,  fill = as.factor(Income_Group_5))) + geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.75, color = "black") + theme_bw() + xlab("Income Group based on Income coming directly from Survey") + ylab ("Households") + scale_y_continuous(labels = function(x)format(x, decimal.mark = ",", big.mark = ".", scientific = FALSE)) + labs(fill = "Income Group with \ndirect expenditures only") + scale_fill_brewer(palette = "Pastel1")
#   # Population
#   P.3.3.3.T.1b <- ggplot(Binning.Check.T.5, aes(x = factor(Income_Group_5_T), y = affected_pop, fill = as.factor(Income_Group_5))) + geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.75, color = "black") + theme_bw() + xlab("Income Group based on Income coming directly from Survey") + ylab ("Population") + scale_y_continuous(labels = function(x)format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) + labs(fill = "Income Group with \ndirect expenditures only") + scale_fill_brewer(palette = "Pastel1")
#   
#   Binning.Check.T.10 <- Binning.Check.T %>%
#     group_by(Income_Group_10_T, Income_Group_10)%>%
#     summarise(
#       affected_hh  = sum(hh_weights),
#       affected_pop = sum(population)
#     )%>%
#     ungroup()
#   
#   # Households
#   P.3.3.3.T.2a <- ggplot(Binning.Check.T.10, aes(x = factor(Income_Group_10_T), y = affected_hh,  fill = as.factor(Income_Group_10))) + geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.75, color = "black") + theme_bw() + xlab("Income Group based on Expenditures coming directly from Survey") + ylab ("Households") + scale_y_continuous(labels = function(x)format(x, decimal.mark = ",", big.mark = ".", scientific = FALSE)) + labs(fill = "Income Group with \ndirect expenditures only") + scale_fill_brewer(palette = "Pastel1")
#   # Population
#   P.3.3.3.T.2b <- ggplot(Binning.Check.T.10, aes(x = factor(Income_Group_10_T), y = affected_pop, fill = as.factor(Income_Group_10))) + geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.75, color = "black") + theme_bw() + xlab("Income Group based on Expenditures coming directly from Survey") + ylab ("Population") + scale_y_continuous(labels = function(x)format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) + labs(fill = "Income Group with \ndirect expenditures only") + scale_fill_brewer(palette = "Pastel1")
#   }
#  3.3.3.S   Check on Turkey and Thailand ####

# We want to investigate on the difference between Income and Expenditures (per capita) over different income groups

# if(Country.Name == "Turkey" | Country.Name == "Thailand"){
#   Exp.Check.T <- left_join(Household.Binning, T.X, by = "hh_id")%>%
#     select(hh_id, Income_Group_5, Income_Group_10, hh_expenditures_0_pc, urban, income, hh_size, hh_weights)%>%
#     mutate(hh_income_0_pc = income/hh_size)%>%
#     mutate(difference = hh_income_0_pc - hh_expenditures_0_pc)%>%
#     mutate(share = difference/hh_expenditures_0_pc)%>%
#     left_join(Urban.New, by = "urban")
#   
# # Plotting both, Income and Expenditures in local currency
#   Exp.Check.T.1 <- Exp.Check.T %>%
#     gather(key = "Type", value = "unit", hh_expenditures_0_pc, hh_income_0_pc)
#   
#   P.3.3.3.S.1 <- ggplot(Exp.Check.T.1, aes(x = factor(Income_Group_5), y = unit, fill = Type))+ geom_boxplot(aes(weight = hh_weights, fill = Type), width = 0.5)+
#     theme_bw()+ xlab("Income Group") + ylab("Expenditures / Income per capita") + 
#     scale_y_continuous(labels = function(x)format(x, decimal.mark = ",", big.mark = ".", scientific = FALSE))
#   P.3.3.3.S.2 <- ggplot(Exp.Check.T.1, aes(x = factor(Income_Group_5), y = unit, fill = Type))+ geom_boxplot(aes(weight = hh_weights, fill = Type), outlier.shape = NA, width = 0.5)+
#     theme_bw()+ xlab("Income Group") + ylab("Expenditures / Income per capita")+coord_cartesian(ylim = c(0, max(boxplot.stats(Exp.Check.T.1$unit)$stats)*3)) +
#     scale_y_continuous(labels = function(x)format(x, decimal.mark = ",", big.mark = ".", scientific = FALSE))
#   
# # Plotting the difference over Income Groups
#   
#   P.3.3.3.S.3 <- ggplot(Exp.Check.T, aes(x = factor(Income_Group_5), y = difference)) + geom_boxplot(aes(weight = hh_weights), width = 0.5) + theme_bw()+
#     xlab("Income Group") + ylab("Difference between Income and Expenditures")+ 
#     scale_y_continuous(labels = function(x)format(x, decimal.mark = ",", big.mark = ".", scientific = FALSE))+
#     stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white")
#   P.3.3.3.S.4 <- ggplot(Exp.Check.T, aes(x = factor(Income_Group_5), y = difference)) + geom_boxplot(aes(weight = hh_weights), width = 0.5, outlier.shape = NA) + theme_bw()+
#     xlab("Income Group") + ylab("Difference between Income and Expenditures")+ 
#     scale_y_continuous(labels = function(x)format(x, decimal.mark = ",", big.mark = ".", scientific = FALSE))+coord_cartesian(y = c(max(boxplot.stats(Exp.Check.T$difference)$stats)*3.5, boxplot.stats(Exp.Check.T$difference)$stats[5]*3.5))+
#     stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white")
#   
# # Plotting the share of difference over Expenditures_0 over Income Group
#   
#   P.3.3.3.S.5 <- ggplot(Exp.Check.T, aes(x = factor(Income_Group_5), y = share)) + geom_boxplot(aes(weight = hh_weights), width = 0.5) + theme_bw()+
#     xlab("Income Group") + ylab("Share of difference between \n Income and Expenditures over Expenditures")+ 
#     stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white")+ scale_y_continuous(labels = scales::percent)
#   
#   P.3.3.3.S.6 <- ggplot(Exp.Check.T, aes(x = factor(Income_Group_5), y = share)) + geom_boxplot(aes(weight = hh_weights), width = 0.5, outlier.shape = NA) + theme_bw()+
#     xlab("Income Group") + ylab("Share of difference between \n Income and Expenditures over Expenditures")+
#     coord_cartesian(y = c(min(boxplot.stats(Exp.Check.T$share)$stats)*1.2, max(boxplot.stats(Exp.Check.T$share)$stats)*1.5))+
#     stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white")+ scale_y_continuous(labels = scales::percent)
#   
# # Including Factor Urban
#   
#   P.3.3.3.S.7 <- ggplot(Exp.Check.T, aes(x = factor(Income_Group_5), y = difference)) + geom_boxplot(aes(weight = hh_weights, fill = Urban), width = 0.5) + theme_bw()+
#     xlab("Income Group") + ylab("Difference between Income and Expenditures")+ 
#     scale_y_continuous(labels = function(x)format(x, decimal.mark = ",", big.mark = ".", scientific = FALSE))
#   
#   P.3.3.3.S.8 <- ggplot(Exp.Check.T, aes(x = factor(Income_Group_5), y = difference)) + geom_boxplot(aes(weight = hh_weights, fill = Urban), width = 0.5, outlier.shape = NA) + theme_bw()+
#     xlab("Income Group") + ylab("Difference between Income and Expenditures")+ 
#     scale_y_continuous(labels = function(x)format(x, decimal.mark = ",", big.mark = ".", scientific = FALSE))+coord_cartesian(y = c(min(boxplot.stats(Exp.Check.T$difference)$stats)*3.5, max(boxplot.stats(Exp.Check.T$difference)$stats)*3.5))
#   
# }

#  3.3.3.C   Sensitivity Check on Currency ####

Expenditure.Analysis <- Expenditure.Analysis %>%
  mutate(hh_expenditures_USD = hh_expenditures_0*exchange.rate)

Mean.Expenditure.USD <- weighted.mean(Expenditure.Analysis$hh_expenditures_USD, Expenditure.Analysis$hh_weights)

P.3.3.3.C.1 <- ggplot(Expenditure.Analysis, aes(x = factor(Income_Group_5), y = hh_expenditures_USD_pc))+ geom_boxplot(aes(weight = hh_weights), width = 0.5)+
  theme_bw()+ xlab("Income Group")+ ylab("Expenditures per Capita") + scale_y_continuous(labels = scales::dollar) +
  stat_summary(fun.y = "mean", geom = "line", aes(group=1), color = "red", size = 1, linetype = "dashed")+ stat_summary(fun.y = "mean", geom = "point", shape = 23, fill = "white", size = 2)+
  geom_hline(yintercept = Mean.Expenditure.USD, color = "darkblue", size = 1, linetype = "dotted")

# Setting up an individual range for each country
if(Country.Name == "Bangladesh") breaks.0 <- c(0, Mean.Expenditure.USD, 5000, 7500, 10000) 
if(Country.Name == "India")      breaks.0 <- c(0, Mean.Expenditure.USD, 1000, 2000, 3000)
if(Country.Name == "Indonesia")  breaks.0 <- c(0, Mean.Expenditure.USD, 2000, 3000, 4000) 
if(Country.Name == "Pakistan")   breaks.0 <- c(0, Mean.Expenditure.USD, 1000, 2000, 3000)
if(Country.Name == "Thailand")   breaks.0 <- c(0, Mean.Expenditure.USD, 2000, 4000, 6000, 8000) 
if(Country.Name == "Turkey")     breaks.0 <- c(0, Mean.Expenditure.USD, 10000, 20000, 30000)
if(Country.Name == "Vietnam")    breaks.0 <- c(0, Mean.Expenditure.USD, 2000, 4000, 6000)

P.3.3.3.C.2 <- ggplot(Expenditure.Analysis, aes(x = factor(Income_Group_5), y = hh_expenditures_USD))+ geom_boxplot(aes(weight = hh_weights), width = 0.5, outlier.shape = NA)+
  theme_bw()+ xlab("Income Group")+ ylab("Expenditures per Capita")+ 
  stat_summary(fun.y = "mean", geom = "line", aes(group=1), color = "red", size = 1, linetype = "dashed")+ stat_summary(fun.y = "mean", geom = "point", shape = 23, fill = "white", size = 2)+
  geom_hline(yintercept = Mean.Expenditure.USD, color = "darkblue", size = 1, linetype = "dotted") + coord_cartesian(y = c(0, max(boxplot.stats(Expenditure.Analysis$hh_expenditures_USD_pc)$stats)*2))+
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1), breaks = breaks.0)+labs(title = Country.Name)

# 3.4        Outlier Correction ####

# We correct Outliers on the household and item-Level --> If one household deviates on expenditures on one specific item across its quintile, it is adjusted

# Adding up item-Expenditures for the same household
if(Country.Name != "Philippines" & Country.Name != "Indonesia"){
Household.Merge.0 <- Household.Merge.0 %>%
  group_by(hh_id, item_code)%>%
  mutate(expenditures = sum(expenditures))%>%
  filter(!duplicated((item_code)))%>%
  ungroup()
}

observations.4a <- observations.4 - nrow(Household.Merge.0)

print(paste("There were ", observations.4a, " cases of double entries for one household and one item."))

Household.Corrected <- left_join(Household.Merge.0, Household.Binning.0, by = "hh_id")%>%
  select(hh_id, item_code, expenditures, Income_Group_5, hh_size, hh_weights)%>%
  mutate(hh_pop = hh_size*hh_weights)%>%
  group_by(Income_Group_5, item_code)%>%
  summarise(median_item       = wtd.quantile(expenditures, weights = hh_pop, probs = 0.5),
            ninetynineth_item = wtd.quantile(expenditures, weights = hh_pop, probs = 0.999))%>%
  ungroup()

Household.Merge.1 <- Household.Merge.0 %>%
  left_join(Household.Binning.0, by = "hh_id")%>%
  left_join(Household.Corrected, by = c("Income_Group_5", "item_code"))%>%
  mutate(Outlier = ifelse(expenditures > ninetynineth_item, 1, 0))

# 3.4.1      Analysis of Outliers ####

Outlier <- sum(Household.Merge.1$Outlier)

print(paste("We find ", Outlier, " Outliers in this data set and replace them by the median"))

Outlier.Analysis <- Household.Merge.1 %>%
  select(hh_id, Income_Group_5, expenditures, median_item, ninetynineth_item, GTAP, Outlier)%>%
  filter(Outlier == 1)

O.A.1 <- count(Outlier.Analysis, Income_Group_5, GTAP)

P.O.C <- ggplot(O.A.1, aes(x = factor(Income_Group_5), y = n))+
  geom_col()+facet_wrap(~ GTAP)+ theme_bw()+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+ggtitle("Outliers per Item over Income Group", Country.Name)+
  coord_cartesian(ylim = c(0,20))

Outlier.Analysis <- Outlier.Analysis %>%
  mutate(overshoot_percent = expenditures/ninetynineth_item)%>%
  mutate(median_over_ninetynineth = median_item/ninetynineth_item)%>%
  mutate(ninetynineth = 1)

Outlier.Analysis.1 <- Outlier.Analysis %>%
  filter(overshoot_percent == Inf)

print(paste("We find ", nrow(Outlier.Analysis.1), "cases, where the 99.9th percentile within one quintile spends zero, but one household spends something."))

Outlier.Analysis.2 <- Outlier.Analysis %>%
  filter(overshoot_percent != Inf)%>%
  select(Income_Group_5, GTAP, overshoot_percent, ninetynineth, median_over_ninetynineth)%>%
  group_by(Income_Group_5, GTAP)%>%
  summarise(
    overshoot_percent = mean(overshoot_percent),
    ninetynineth = mean(ninetynineth),
    median_over_ninetynineth = mean(median_over_ninetynineth)
  )%>%
  ungroup()%>%
  gather(key = "measure", value = "value", overshoot_percent:median_over_ninetynineth)

# We try to gain information on the extent of the outlier. How much bigger is the outlier than the actual median 99th percentile

P.O.C.1 <- ggplot(Outlier.Analysis.2, aes(x = factor(Income_Group_5), y = value)) +
  geom_col(aes(fill = measure), position = "dodge") + facet_wrap(~ GTAP)+
  coord_cartesian(ylim = c(0,3))

# 3.4.2      Replacement of Outliers ####

# Outliers are replaced by the median of their Quintile

Household.Merge.2 <- Household.Merge.1 %>%
  select(-Income_Group_5, - Income_Group_10)%>%
  mutate(expenditures = ifelse(expenditures > ninetynineth_item, median_item, expenditures))%>%
  select(-median_item, -ninetynineth_item, -Outlier)

Household.Merge.0 <- Household.Merge.2

# 3.4.3      Sensitivity Check on Outlier Sensitive Binning ####

Household.Merge.2.Binning <- Household.Merge.2 %>%
  filter(GTAP != "deleted")%>%
  group_by(hh_id)%>%
  mutate(hh_expenditures_0   = sum(expenditures))%>%
  ungroup()%>%
  mutate(hh_expenditures_0_pc = hh_expenditures_0/hh_size)%>%
  select(hh_id, hh_expenditures_0,hh_expenditures_0_pc, hh_weights, urban, hh_size)%>%
  filter(!duplicated(hh_id))%>%
  mutate(Income_Group_5   = as.numeric(binning(hh_expenditures_0_pc, bins=5,   method=c("wtd.quantile"), labels=seq(1,5,length.out=5),     weights = hh_weights)))%>%
  mutate(Income_Group_10  = as.numeric(binning(hh_expenditures_0_pc, bins=10,  method=c("wtd.quantile"), labels=seq(1,10,length.out=10),   weights = hh_weights)))

Binning.Correction.Check <- Household.Merge.2.Binning %>%
  select(hh_id, hh_size, hh_weights, Income_Group_5)%>%
  rename(Income_Group_5_C = Income_Group_5)%>%
  left_join(Household.Binning.0, by = "hh_id")%>%
  select(-Income_Group_10)%>%
  mutate(pop = hh_size*hh_weights)%>%
  group_by(Income_Group_5_C, Income_Group_5)%>%
  summarise(affected = sum(pop))

P.O.C.2 <- ggplot(Binning.Correction.Check, aes(x = factor(Income_Group_5_C), y = affected, fill = factor(Income_Group_5)))+
  geom_col(colour = "black") + theme_bw()+ scale_fill_nejm()+
  labs(fill = "Quintile before\nOutlier Correction")+
  scale_y_continuous(labels = function(x)format(x, decimal.mark = ",", big.mark = ".", scientific = FALSE))+
  xlab("Quintile after Outlier Correction")+ylab("Population")

# 3.4.4      Binning final ####

Household.Binning.1 <- Household.Merge.2.Binning %>%
  select(hh_id, Income_Group_5, Income_Group_10)

rm(Household.Merge.2)

# 3.5.1      Processing Household.Merge ####

Household.Merge <- Household.Merge.0 %>%
  arrange(hh_id)%>%
  filter(GTAP != "deleted" | is.na(GTAP))%>% # including |is.na(GTAP)? This should make sense as otherwise NAs would be dropped here
  group_by(hh_id)%>%
   mutate(hh_expenditures_check          = sum(expenditures))%>%
   mutate(hh_expenditures_selfprod_check = sum(expenditures_selfproduced))%>%
  ungroup()

observations.4a <- nrow(Household.Merge)

# 3.5.2      Check on Expenditures for Self-Produced Items ####

#Household.Check.1 <- Household.Merge %>%
#  select(hh_id, hh_size, hh_weights, hh_expenditures_check, hh_expenditures_selfprod_check)%>%
#  filter(!duplicated(hh_id))
#
#Household.Check.2 <- left_join(Household.Check.1, Household.Binning, by = "hh_id")%>%
#  select(-hh_expenditures_0_pc)%>%
#  mutate(share_sp_exp = hh_expenditures_selfprod_check/hh_expenditures_check)%>%
#  mutate(population   = hh_weights*hh_size)%>%
#  left_join(Urban.New, by = "urban")
#
## a)over Income Groups with different y-axis (5 bins)
#P.3.3.4.1.a1 <- ggplot(Household.Check.2, aes(x = factor(Income_Group_5), y = share_sp_exp)) + geom_boxplot(aes(weight = hh_weights),width = .5) + theme_bw() + stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") + xlab("Income Group") + ylab("Share of self-produced items over all expenditures")
## without outliers
#P.3.3.4.1.a2 <- ggplot(Household.Check.2, aes(x = factor(Income_Group_5), y = share_sp_exp)) + geom_boxplot(aes(weight = hh_weights),width = .5, outlier.shape = NA) + theme_bw() + stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white")+
#  coord_cartesian(ylim = c(0,max(boxplot.stats(Household.Check.2$share_sp_exp)$stats)*2.5))+ xlab("Income Group") + ylab("Share of self-produced items over all expenditures") + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#
## weighting with per capita
#P.3.3.4.1.a4 <- ggplot(Household.Check.2, aes(x = factor(Income_Group_5), y = share_sp_exp)) + geom_boxplot(aes(weight = population),width = .5) + theme_bw() + stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white")+ xlab("Income Group")+ ylab("Share of self-produced items over all expenditures")+ scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#P.3.3.4.1.a5 <- ggplot(Household.Check.2, aes(x = factor(Income_Group_5), y = share_sp_exp)) + geom_boxplot(aes(weight = population),width = .5, outlier.shape = NA) + theme_bw() + stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white")+ xlab("Income Group")+ ylab("Share of self-produced items over all expenditures")+
#  coord_cartesian(ylim = c(0,max(boxplot.stats(Household.Check.2$share_sp_exp)$stats)*2.5))+ scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#
## b)over Income Groups with different y-axis (10 bins)
#P.3.3.4.1.b1 <- ggplot(Household.Check.2, aes(x = factor(Income_Group_10), y = share_sp_exp)) + geom_boxplot(aes(weight = hh_weights),width = .5) + theme_bw() + stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") + xlab("Income Group") + ylab("Share of self-produced items over all expenditures")+ scale_y_continuous(labels = scales::percent_format(accuracy = 1))
## without outliers
#P.3.3.4.1.b2 <- ggplot(Household.Check.2, aes(x = factor(Income_Group_10), y = share_sp_exp)) + geom_boxplot(aes(weight = hh_weights),width = .5, outlier.shape = NA) + theme_bw() + stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white")+ xlab("Income Group")+ ylab("Share of self-produced items over all expenditures") + 
#  coord_cartesian(ylim = c(0,max(boxplot.stats(Household.Check.2$share_sp_exp)$stats)*2.5))+ scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#
## c)over rural/urban
#P.3.3.4.1.c1 <- ggplot(Household.Check.2, aes(x = factor(Urban), y = share_sp_exp)) + geom_boxplot(aes(weight = hh_weights),width = .5) + theme_bw() + stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white")+ xlab("Income Group") + ylab("Share of self-produced items over all expenditures")
## without outliers
#P.3.3.4.1.c2 <- ggplot(Household.Check.2, aes(x = factor(Urban), y = share_sp_exp)) + geom_boxplot(aes(weight = hh_weights),width = .5, outlier.shape = NA) + theme_bw() + stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") + 
#  coord_cartesian(ylim = c(0,max(boxplot.stats(Household.Check.2$share_sp_exp)$stats)*2.5))+ xlab("Income Group") + ylab("Share of self-produced items over all expenditures")+scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#
## d) over Income Groups with different y-axis AND rural/urban (5 bins)
#P.3.3.4.1.d1 <- ggplot(Household.Check.2, aes(x=factor(Urban), y = share_sp_exp, fill = Urban)) + geom_boxplot(aes(weight = hh_weights), width = .5) + theme_bw() + facet_grid(~Income_Group_5)+ stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 1, fill = "white") + xlab("Income Group") + ylab("Share of self-produced items over all expenditures") + theme(strip.background = element_rect(colour = "black", size = 1))
## without outliers
#P.3.3.4.1.d2 <- ggplot(Household.Check.2, aes(x=factor(Urban), y = share_sp_exp, fill = Urban)) + geom_boxplot(aes(weight = hh_weights), width = .5, outlier.shape = NA) + theme_bw() + facet_grid(~Income_Group_5)+ stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 1, fill = "white") + xlab("Income Group") + ylab("Share of self-produced items over all expenditures") + theme(strip.background = element_rect(colour = "black", size = 1)) + 
#  coord_cartesian(ylim = c(0,max(boxplot.stats(Household.Check.2$share_sp_exp)$stats)*2.5))+scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#
## e) over Income Groups with different y-axis AND rural/urban (10 bins)
#P.3.3.4.1.e1 <- ggplot(Household.Check.2, aes(x=factor(Urban), y = share_sp_exp, fill = Urban)) + geom_boxplot(aes(weight = hh_weights), width = .5) + theme_bw() + facet_grid(~Income_Group_10)+ stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 1, fill = "white") + xlab("Income Group") + ylab("Share of self-produced items over all expenditures") + theme(strip.background = element_rect(colour = "black", size = 1))+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
## without outliers
#P.3.3.4.1.e2 <- ggplot(Household.Check.2, aes(x=factor(Urban), y = share_sp_exp, fill = Urban)) + geom_boxplot(aes(weight = hh_weights), width = .5, outlier.shape = NA) + theme_bw() + facet_grid(~Income_Group_10)+ stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 1, fill = "white") + xlab("Income Group") + ylab("Share of self-produced items over all expenditures") + theme(strip.background = element_rect(colour = "black", size = 1)) + 
#  coord_cartesian(ylim = c(0,max(boxplot.stats(Household.Check.2$share_sp_exp)$stats)*2.5)) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#
#P.3.3.4.1.e3 <- ggplot(Household.Check.2, aes(x=factor(Income_Group_10), y = share_sp_exp, fill = Income_Group_10)) + geom_boxplot(aes(weight = hh_weights), width = .5, outlier.shape = NA) + theme_bw() + facet_grid(~Urban)+ stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 1, fill = "white") + xlab("Income Group") + ylab("Share of self-produced items over all expenditures") + theme(strip.background = element_rect(colour = "black", size = 1)) + 
#  coord_cartesian(ylim = c(0,max(boxplot.stats(Household.Check.2$share_sp_exp)$stats)*2.5))+scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#
#rm(Household.Check.1, Household.Check.2)

# 3.5.3      Information on Deleted Items (NA) ####

Household.Deleted <- Household.Merge.0 %>%
  filter(is.na(GTAP))

# Items <- read.xlsx("../matching_results_all_countries.xlsx", sprintf("ITEM_%s", COUNTRY.NAME))
Items <- read.xlsx("../Item_Codes_Description.xlsx", sprintf("ITEM_%s", COUNTRY.NAME))%>%
  mutate(item_code = as.character(item_code))%>%
  select(item_code, item_name)

Items.Deleted.1 <- count(Household.Deleted, item_code)%>%
  mutate(share_data.points = n/nrow(Household.Merge.0))%>%
  mutate(item_code = as.character(item_code))

Items.Deleted.2 <- Household.Deleted%>%
  group_by(item_code)%>%
  mutate(expenditures_del_sum              = sum(expenditures))             %>%
  mutate(expenditures_selfproduced_del_sum = sum(expenditures_selfproduced))%>%
  ungroup()%>%
  
  select(item_code, expenditures_del_sum, expenditures_selfproduced_del_sum)%>%
  filter(!duplicated(item_code))%>%
  
  mutate(share_deleted    = expenditures_del_sum              / sum(Household.Merge.0$expenditures))             %>%
  mutate(share_deleted_sp = expenditures_selfproduced_del_sum / sum(Household.Merge.0$expenditures_selfproduced))%>%
  mutate(item_code = as.character(item_code))


Items.Deleted.3 <- left_join(Items.Deleted.1, Items, by = "item_code")
Items.Deleted.4 <- left_join(Items.Deleted.3, Items.Deleted.2, by = "item_code")

if(nrow(Items.Deleted.4) > 0){write.xlsx(Items.Deleted.4, "Deleted_Items_in_HH_survey.xlsx")}

rm(Items, Items.Deleted.1, Items.Deleted.2, Items.Deleted.3, Items.Deleted.4, Household.Deleted)

# 3.5.4      Information on "Other"-Items ####

Household.Other.1 <- Household.Merge %>%
  filter(GTAP == "other")%>%
  group_by(hh_id)%>%
  summarise(
    expenditures.other = sum(expenditures)
  )

Household.Other.2 <- Household.Merge %>%
  group_by(hh_id)%>%
  summarise(
    expenditures = sum(expenditures)
  )

Household.Other.3 <- left_join(Household.Other.2, Household.Other.1, by = "hh_id")

Household.Other.4 <- Household.Merge %>%
  select(hh_id, hh_size, hh_weights, urban)%>%
  filter(!duplicated(hh_id))%>%
  left_join(Household.Binning.1, by = "hh_id")%>%
  left_join(Household.Other.3, by = "hh_id")

Household.Other.4$expenditures.other[is.na(Household.Other.4$expenditures.other)] <- 0

Household.Other.4 <- Household.Other.4 %>%
  mutate(share.other = expenditures.other/expenditures)

P.3.5.4 <- ggplot(Household.Other.4, aes(x = factor(Income_Group_5), y = share.other))+
  geom_boxplot(aes(weight = hh_weights), outlier.shape = NA, width = 0.75) + theme_bw()+
  ggtitle("Share of Expenditures on other", Country.Name) + xlab("Expenditure Quintile")+coord_cartesian(ylim = c(0,0.1))+
  ylab("")+ scale_y_continuous(labels = scales::percent_format(accuracy = 1))

rm(Household.Other.1, Household.Other.2, Household.Other.3, Household.Other.4)

# 3.5.5      Processing Household.Merge ####

Household.Merge <- Household.Merge %>%
  filter(!is.na(GTAP))
obs.5a <- nrow(Household.Merge)


print(paste0("After merging the Household data and matching ", (observations.3-observations.4), " observations were deleted."))

# 3.5.6      Preparing Analysis of Energy/Food/Goods/Services ####

types_0 <- read.xlsx("../Item_Categories_Concordance.xlsx", sprintf("Item_%s", Country.Name), startRow = 1, colNames = FALSE)%>%
  filter(X1 == "food" | X1 == "energy" | X1 == "services" | X1 == "goods" | X1 == "other_binning" | X1 == "deleted" | X1 == "in-kind" | X1 == "self-produced")

types_1 <- types_0 %>%
  mutate_at(vars(-X1), list(function(x) x = as.character(x)))%>%
  pivot_longer(-X1, names_to = "type", values_to = "item")%>%
  filter(!is.na(item))%>%
  select(-type)

HM.0 <- Household.Merge %>%
  select(hh_id, hh_size, hh_weights, item_code, expenditures, hh_expenditures_check)%>%
  mutate(item_code = as.character(item_code))%>%
  left_join(types_1, by = c("item_code" = "item"))%>%
  filter(X1 != "other_binning" & X1 != "deleted" & X1 != "in-kind" & X1 != "self-produced")%>%
  group_by(hh_id, X1)%>%
  summarise(hh_size               = first(hh_size),
            hh_weights            = first(hh_weights),
            expenditures          = sum(expenditures, na.rm = TRUE),
            hh_expenditures_check = first(hh_expenditures_check))%>%
  ungroup()%>%
  mutate(share = expenditures/hh_expenditures_check)%>%
  select(hh_id, X1, share)%>%
  spread(key = X1, value = share)%>%
  rename(share_energy = energy, share_food = food, share_goods = goods, share_services = services)
  
#write_csv(HM.0, sprintf("../Expenditure_types_%s.csv", Country.Name))

rm(HM.0)

# 3.6        Merging gdt and gas ####
# 3.6.1      Merging gdt and gas for Household-Data ####

gas.gdt     <- Household.Merge %>%
  filter(GTAP == "gas" | GTAP == "gdt")

row.gas.gdt <- nrow(gas.gdt)

gas.gdt     <- gas.gdt %>%
  mutate(item_code = as.character(item_code))%>%
  mutate(hh_id = as.character(hh_id))%>%
  group_by(hh_id) %>%
  summarise(
    expenditures                   = sum(expenditures),
    expenditures_selfproduced      = sum(expenditures_selfproduced),
    item_code                      = first(item_code),
    adults                         = first(adults),
    children                       = first(children),
    hh_size                        = first(hh_size),
    #electricity_source             = first(electricity_source),
    hh_weights                     = first(hh_weights),
    urban                          = first(urban),
    GTAP                           = "gasgdt",
    hh_expenditures_check          = first(hh_expenditures_check),
    hh_expenditures_selfprod_check = first(hh_expenditures_selfprod_check)
  )%>%
  ungroup()
  
Household.Merge <- Household.Merge %>%
  mutate(hh_id = as.character(hh_id))%>%
  mutate(item_code = as.character(item_code))%>%
  mutate(GTAP = as.character(GTAP))

if((row.gas.gdt - nrow(gas.gdt))!= 0){
 Household.Merge <- Household.Merge %>%
   filter(GTAP != "gas" & GTAP != "gdt")
 
 Household.Merge <- bind_rows(Household.Merge, gas.gdt)%>%
   arrange(hh_id)
}else{
  Household.Merge$GTAP <- recode(Household.Merge$GTAP, gas = "gasgdt", gdt = "gasgdt")
}

# 3.6.2      Merging gdt and gas for Virtual ####

gas.gdt.2 <- Virtual %>%
  filter(GTAP == "gas" | GTAP == "gdt")%>%
  summarise(
    GTAP               = "gasgdt",
    Electricity_MUSD   = sum(Electricity_MUSD),
    CO2_Mt             = sum(CO2_Mt),
    CO2_Mt_from_within = sum(CO2_Mt_from_within)
  )

Virtual.1 <- Virtual %>%
  filter(GTAP != "gas" & GTAP != "gdt")%>%
  mutate(GTAP = as.character(GTAP))

Virtual <- bind_rows(gas.gdt.2, Virtual.1)

rm(Virtual.1, gas.gdt, gas.gdt.2)

# 3.7        Grouping hh_id und GTAP together ####

Household.Merge.T.1 <- Household.Merge %>%
  select(hh_id, adults, children, hh_size, hh_weights, urban, hh_expenditures_check, hh_expenditures_selfprod_check)%>%
  filter(!duplicated(hh_id))

Household.Merge.T.2 <- Household.Merge %>%
  select(hh_id, expenditures, expenditures_selfproduced, GTAP)%>%
  group_by(hh_id, GTAP)%>%
  summarise(
    expenditures = sum(expenditures),
    expenditures_selfproduced = sum(expenditures_selfproduced)
  )%>%
  ungroup()

Household.Merge <- left_join(Household.Merge.T.2, Household.Merge.T.1, by = "hh_id")

if(Country.Name == "Thailand"){
  Household.Merge <- Household.Merge %>%
    filter(GTAP != "in-kind")
}

observations.6 <- nrow(Household.Merge)

Household.Merge.3 <- Household.Merge %>%
  mutate(expenditures_inflated = expenditures*inflation_factor)%>%
  mutate(expenditures_USD_2014 = expenditures_inflated*exchange.rate)%>%
  group_by(hh_id)%>%
  mutate(hh_expenditure_USD = sum(expenditures_USD_2014))%>%
  ungroup()%>%
  select(-expenditures_inflated)

# Including Other!

Household.Merge <- Household.Merge.3

Sector_Size <- Household.Merge %>%
  select(hh_id, hh_weights, expenditures_USD_2014, GTAP)%>%
  group_by(GTAP)%>%
  summarise(sector_size_survey = sum(expenditures_USD_2014*hh_weights))%>%
  ungroup()%>%
  left_join(consumption_00, by = "GTAP")%>%
  mutate(share_hh_over_GTAP = sector_size_survey/expenditures_USD)

#write.xlsx(Sector_Size, "../Comparison_Expenditures_HH_Survey_GTAP_new.xlsx", sheetName = Country.Name, append = TRUE)


observations.7 <- nrow(Household.Merge.3)  

# 3.7.1      Decomposition ####

if(decomposition.part == TRUE){
Decomposition <- left_join(Household.Merge, Virtual, by = "GTAP")%>%
  rename(exp_USD = expenditures_USD_2014) %>%
  rename(hh_tot_inc_USD = hh_expenditure_USD)

colnames(Decomposition)[colnames(Decomposition)=="CO2_Mt"]             <- sprintf("%s_Co2_Mt", Country.Name)

Decomposition_1 <- Decomposition %>% 
  select(hh_id, GTAP, exp_USD, sprintf("%s_Co2_Mt", Country.Name)) 

Decomposition_2 <- Decomposition %>% 
  select(hh_id, hh_weights, hh_size, hh_tot_inc_USD, urban)%>%
  filter(!duplicated(hh_id))

Decomposition_1 <- dcast(melt(Decomposition_1,id.vars=c("hh_id", "GTAP")), hh_id~variable+GTAP)

Decomposition_3 <- left_join(Decomposition_1,Decomposition_2, by = "hh_id")

#direct_emissions <- read_xlsx(sprintf(("Scripts/Code_indirec_and_direct_emissions/Direct_emissions_%s_HH_values_only_2011_neu.xlsx"), country.name))

# Fuel Consumption Items
items.fuel <- read.xlsx("../Items_fuel_consumption.xlsx", Country.Name) # Available upon request
items.fuel <- items.fuel %>%
  filter(!is.na(ITEM))%>%
  gather("ITEM_1", "item_code", - ITEM, na.rm = TRUE)%>%
  select(-ITEM_1)%>%
  arrange(ITEM)%>%
  mutate(item_code = as.numeric(item_code))

if(Country.Name == "Vietnam" | Country.Name == "Thailand"){
  items.fuel <- items.fuel %>%
    mutate(item_code = as.character(item_code))
}

items.fuel.2 <- inner_join(Household.Merge.0, items.fuel, by = "item_code")%>%
  select(hh_id, expenditures, expenditures_selfproduced, ITEM)%>%
  mutate(expenditures_USD              = expenditures             *exchange.rate*inflation_factor)%>%
  select(-expenditures, -expenditures_selfproduced)%>%
  group_by(hh_id, ITEM)%>%
  summarise(expenditures_USD    = sum(expenditures_USD))%>%
  ungroup()

items.fuel.3 <- dcast(melt(items.fuel.2, id.vars = c("hh_id","ITEM"), na.rm=FALSE), hh_id~variable+ITEM)%>%
  mutate(hh_id = as.character(hh_id))

Decomposition_final <- left_join(Decomposition_3, items.fuel.3, by = "hh_id")

write_csv(Decomposition_final, sprintf("../%s_data_for_decomposition_wide_GTAP10_new.csv", Country.Name))
}

# 3.8        Processing Household.Merge ####

Household.final <- Household.Merge

observations.8 <- nrow(Household.final)

# 3.8.1      Share of expenditures on electricity over all expenditures  ####

Household.Merge.ely <- Household.Merge%>%
  filter(GTAP == "ely")%>%
  select(hh_id, expenditures)

Household.Merge.ely.2 <- Expenditure.Analysis %>%
  select(-Income_Group_100)%>%
  mutate(hh_id = as.character(hh_id))%>%
  left_join(Household.Merge.ely, by = "hh_id")

Household.Merge.ely.2$expenditures[is.na(Household.Merge.ely.2$expenditures)] <- 0

Household.Merge.ely.2 <- Household.Merge.ely.2 %>%
  mutate(share_ely = expenditures/hh_expenditures_0)%>%
  left_join(Urban.New, by = "urban")

write_csv(Household.Merge.ely.2, "electricity_share_analysis.csv")

wtd.mean.ely <- wtd.mean(Household.Merge.ely.2$share_ely, weights = Household.Merge.ely.2$hh_weights)

P.3.6.2 <- ggplot(Household.Merge.ely.2, aes(x = factor(Income_Group_5), y = share_ely))+
  geom_boxplot(aes(weight = hh_weights), outlier.shape = NA, width = 0.5) + theme_bw()+
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white")+
  ylab("Share of expenditures on electricity over all expenditures") + xlab("Income Group")+
  coord_cartesian(ylim = c(0, 0.1))+scale_y_continuous(labels = scales::percent_format(accuracy = 1))

P.3.6.2.1 <- ggplot(Household.Merge.ely.2, aes(x = factor(Income_Group_5), y = share_ely, fill = Urban))+
  geom_boxplot(aes(weight = hh_weights), outlier.shape = NA, width = 0.5) + theme_bw()+
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white")+
  ylab("Share of expenditures on electricity over all expenditures") + xlab("Income Group")+
  coord_cartesian(ylim = c(0, 0.1))+scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(fill = "")

# We split the household up in order to map them together

Household.Merge.ely.2.1 <- Household.Merge.ely.2 %>%
  select(hh_id, Income_Group_5, share_ely, Urban, hh_weights)

Household.Merge.ely.2.2 <- Household.Merge.ely.2 %>%
  select(hh_id, Income_Group_5, share_ely, hh_weights)%>%
  mutate(Urban = "Country")

Household.Merge.ely.2.3 <- Household.Merge.ely.2 %>%
  select(hh_id, share_ely, Urban, hh_weights)%>%
  mutate(Income_Group_5 = 0)

Household.Merge.ely.2.4 <- Household.Merge.ely.2 %>%
  select(hh_id, share_ely, hh_weights)%>%
  mutate(Income_Group_5 = 0)%>%
  mutate(Urban = "Country")

Household.Merge.ely.2.5 <-Household.Merge.ely.2.1 %>%
  rbind(Household.Merge.ely.2.2)%>%
  rbind(Household.Merge.ely.2.3)%>%
  rbind(Household.Merge.ely.2.4)%>%
  mutate(Urban = fct_reorder2(Urban, Urban, Urban))%>%
  arrange(hh_id, Income_Group_5)

Household.Merge.ely.2.6 <- Household.Merge.ely.2.5 %>%
  group_by(Income_Group_5, Urban)%>%
  summarise(
    share_ely_mean = weighted.mean(share_ely, weight = hh_weights)
  )%>%
  ungroup()

Household.Merge.ely.2.5 <- Household.Merge.ely.2.5 %>%
  left_join(Household.Merge.ely.2.6, by = c("Income_Group_5", "Urban"))

P.3.6.2.2 <- ggplot(Household.Merge.ely.2.5, aes(x = factor(Income_Group_5), fill = Urban))+
  geom_boxplot(aes(y = share_ely, weight = hh_weights), outlier.shape = NA, varwidth = TRUE) + theme_bw() +
  xlab("Income Group") + ylab ("")+
  coord_cartesian(ylim = c(0, 0.1))+scale_y_continuous(labels = scales::percent_format(accuracy = 1))+ labs(fill = "")+
  scale_x_discrete(labels = c("Overall", "1", "2", "3", "4", "5")) + scale_fill_brewer(palette = "Pastel1")+
  ggtitle(Country.Name, "Share of Expenditures on Electricity over all Expenditures")

# 3.9        Analyzing NAs and Zeros in "dwe"(dwellings) + Analyzing the share of expenditures on dwe ####

# dwellings <- Household.Merge %>%
#   select(hh_id, GTAP, expenditures)%>%
#   filter(GTAP == "dwe")%>%
#   select(-GTAP)%>%
#   mutate(hh_id = as.numeric(hh_id))
# 
# dwellings.1 <- left_join(Household.Binning.DWE, dwellings, by = "hh_id")%>%
#   rename(expenditures_DWE = expenditures)
# 
# dwellings.1$expenditures_DWE[is.na(dwellings.1$expenditures_DWE)] <- 0 # imputed
# 
# dwellings.1 <- dwellings.1 %>%
#   mutate(hh_expenditures_without_dwe = hh_expenditures_0 - expenditures_DWE)%>%
#   mutate(share_DWE = expenditures_DWE/hh_expenditures_without_dwe)%>%
#   mutate(share_inc_DWE = expenditures_DWE/hh_expenditures_0)
# 
# P.3.7a <- ggplot(dwellings.1, aes(x = factor(Income_Group_5), y = share_DWE)) + geom_boxplot(aes(weight = hh_weights),width = .5) + theme_bw() + ylab("Share of Expenditures on DWELLINGS \n over all Expenditures without Dwellings") + xlab("Income Group") + stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white")+ scale_y_continuous(labels = scales::percent_format(accuracy = 1))
# 
# # without outliers
# P.3.7b <- ggplot(dwellings.1, aes(x = factor(Income_Group_5), y = share_DWE)) + geom_boxplot(aes(weight = hh_weights),width = .5, outlier.shape = NA) + theme_bw() + ylab("Share of Expenditures on DWELLINGS \n over all Expenditures without Dwellings") + xlab("Income Group") + stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") + 
#   coord_cartesian(ylim = c(0,max(boxplot.stats(dwellings.1$share_DWE)$stats)*1.5)) + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
# 
# # over expenditures including dwellings
# P.3.7c <- ggplot(dwellings.1, aes(x = factor(Income_Group_5), y = share_inc_DWE)) + geom_boxplot(aes(weight = hh_weights),width = .5, outlier.shape = NA) + theme_bw() + ylab("Share of Expenditures on DWELLINGS \n over all Expenditures without Dwellings") + xlab("Income Group") + stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") + 
#   coord_cartesian(ylim = c(0,max(boxplot.stats(dwellings.1$share_DWE)$stats)*1.5)) + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
# 
# rm(dwellings, dwellings.1)

# 3.10       Joining Household_final and Virtual ####

Incidence      <- left_join(Household.final, intensities, by = "GTAP")
observations.9 <- nrow(Incidence)
print(paste0("By merging the Household.final and Virtual ", observations.8-observations.9, " observations have been deleted."))

Incidence_1 <- Incidence %>%
  select(-expenditures, -expenditures_selfproduced, -hh_expenditures_check, -hh_expenditures_selfprod_check)%>%
  filter(GTAP != "other")%>%
  mutate(CO2_t        = CO2_intensity_t_per_dollar       *expenditures_USD_2014,
         CO2_within_t = CO2_within_intensity_t_per_dollar*expenditures_USD_2014,
         Ely_USD      = Ely_USD_intensity_dollar_per_dollar  *expenditures_USD_2014)
  
Incidence.1 <- Incidence %>%
  group_by(GTAP)%>%
  summarise(
    Sector_size = sum(expenditures_USD_2014*hh_weights)
  )%>%
  ungroup()%>%
  left_join(Virtual, by ="GTAP")%>%
  mutate(burden_ely        = Electricity_MUSD*1000000*0.25)%>%
  mutate(burden_CO2        = CO2_Mt*1000000*carbon.price)%>%
  mutate(burden_CO2_within = CO2_Mt_from_within*1000000*carbon.price)

Incidence.2 <- Incidence.1 %>%
  mutate(ely_percent    = burden_ely/Sector_size)%>%
  mutate(CO2_percent    = burden_CO2/Sector_size)%>%
  mutate(CO2_wi_percent = burden_CO2_within/Sector_size)%>%
  select(GTAP, CO2_wi_percent, CO2_percent, ely_percent)%>%
  rename('Case I' = CO2_wi_percent, 'Case II' = CO2_percent, 'Case III' = ely_percent)

# write.xlsx(Incidence.2, sprintf("../Table_%s.xlsx", Country.Name))
# write_csv(Incidence.2,  sprintf("../Table_%s.csv", Country.Name))

# 3.11       One observation per household ####

Incidence_2 <- Incidence_1 %>%
  group_by(hh_id)%>%
   summarise(
    expenditures_USD_2014        = sum (expenditures_USD_2014),
    hh_expenditures_USD          = first(hh_expenditure_USD),
    adults                       = first(adults),
    children                     = first(children),
    hh_size                      = first(hh_size),
    hh_weights                   = first(hh_weights),
    urban                        = first(urban),
    CO2_t                        = sum(CO2_t),
    CO2_within_t                 = sum(CO2_within_t),
    Ely_USD                      = sum(Ely_USD)
   )%>%
  ungroup()

observations.10 <- nrow(Incidence_2)
print(paste0(observations.9, " observations have been summarised to ", observations.10, " observations."))

# ____       ####
# 4          ANALYSING DATA ####
# 4.1        Merging Incidence with Binning ####

# Binning-Dataframe resulted from binning without NA and "deleted", including "other" and "dwe"

Incidence.final <- Incidence_2 %>%
  mutate(hh_id = as.numeric(hh_id))%>%
  left_join(Household.Binning.1, by = "hh_id")

if(Country.Name == "Bangladesh" | Country.Name == "Philippines"){
  Incidence.final <- left_join(Incidence_2, Household.Binning.1, by = "hh_id")
}

# 4.2        Analysis with 5 income groups ####

# Thailand has households without expenditures. In order to avoid bugs, we declare their expenditures to be one Dollar
Incidence.final$hh_expenditures_USD[Incidence.final$hh_expenditures_USD == 0] <- 1

Incidence.Analysis <- Incidence.final %>%
  mutate(hh_expenditure_USD_pc = expenditures_USD_2014/hh_size)%>%
  
# calculating the measures per capita
  mutate(tCO2_per_capita                = CO2_t        / (hh_size))%>%
  mutate(tCO2_from_within_per_capita    = CO2_within_t / (hh_size))%>%
  mutate(electricity_USD_per_capita     = Ely_USD      / (hh_size))%>%
  
# Raises in Electricity-Price, global CO2-tax, local CO2-tax per household
# "absolute burden"
  mutate(exp_pc_CO2         = tCO2_per_capita*carbon.price)%>%                             # How much money would one person have to spend if a carbon tax would be imposed?
  mutate(exp_pc_CO2_within  = tCO2_from_within_per_capita*carbon.price)%>%
  mutate(exp_pc_electricity = electricity_USD_per_capita*electricity.price.increase)%>%  
  
# Burden: additional expenditures over expenditures (per capita)
# "Relative Belastung" / "relative burden"
  mutate(burden_CO2_per_capita         = exp_pc_CO2         / hh_expenditure_USD_pc)%>%
  mutate(burden_CO2_within_per_capita  = exp_pc_CO2_within  / hh_expenditure_USD_pc)%>%
  mutate(burden_electricity_per_capita = exp_pc_electricity / hh_expenditure_USD_pc)

write_csv(Incidence.Analysis, sprintf("../Incidence.Analysis.%s_11_2020.csv", Country.Name))

