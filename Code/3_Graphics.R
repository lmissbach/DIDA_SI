# 0        General ####

# Authors: L. Missbach - missbach@mcc-berlin.net, L. Montrone - montrone@mcc-berlin.net, I. Dorband - dorband@mcc-berlin.net

# 1        Packages ####

library("cowplot")
library("data.table")
library("foreign")
library("ggthemes")
library("ggpubr")
library("ggsci")
library("Hmisc")
library("janitor")
library("officer")
library("openxlsx")
library("quantreg")
library("rattle")
library("reshape2")
library("scales") 
library("tidyverse")
library("utils")
library("wesanderson")
library("weights")
options(scipen=999)

# 1.1      Working Directory ####

setwd("..")

# 2        Loading Data ####

Urban.New.1 <- data.frame("urban" = c(1,2), "Urban" = c("Rural", "Urban"), urban_01 = c(0,1)) # applies to Bangladesh, INDIA, Pakistan, TURKEY, Philippines
Urban.New.2 <- data.frame("urban" = c(1,2), "Urban" = c("Urban", "Rural"), urban_01 = c(1,0)) # applies to Indonesia, Thailand, Vietnam

shape.for.graphical.analysis.1 <- function(File, Code.Urban){
  File <- File %>%
    left_join(Code.Urban, by = "urban") %>%
    select(hh_id, hh_weights, hh_size, Urban, Income_Group_5, burden_CO2_within_per_capita, burden_CO2_per_capita, burden_electricity_per_capita)
  
  File_0 <- File %>%
    mutate(population = hh_weights*hh_size)
  
  population <- sum(File_0$population)
  
  File_1 <- File %>%
    select(-Income_Group_5)%>%
    mutate(Income_Group_5 = 0)
  
  File_2 <- File %>%
    select(-Urban)%>%
    mutate(Urban = "Country")
  
  File_3 <- File %>%
    select(-Urban, -Income_Group_5)%>%
    mutate(Income_Group_5 = 0)%>%
    mutate(Urban = "Country")
  
  File_4 <- File %>%
    rbind(File_1)%>%
    rbind(File_2)%>%
    rbind(File_3)%>%
    mutate(Urban = fct_reorder2(Urban, Urban, Urban))%>%
    arrange(hh_id, Income_Group_5)
  
  File_0 <- File_4%>%
    mutate(population = (hh_size*hh_weights)/population)%>%
    group_by(Income_Group_5, Urban)%>%
    summarise(population = sum(population)/2)%>%
    ungroup()%>%
    select(population)
  
  Files <- list("File_4" = File_4, "File_0" = File_0)
  
  return(Files)
}

shape.for.graphical.analysis.1.1 <- function(File){
  File_1 <- File %>%
    filter(Income_Group_5 == 0)%>%
    select(- Income_Group_5)
  
  
  File_2 <- File %>%
    filter(Income_Group_5 != 0)%>%
    filter(Urban != "Country")%>%
    select(-Urban)
  
  File_2a <- File_2 %>%
    group_by(Income_Group_5)%>%
    summarise(burden_w_mean = weighted.mean(burden_CO2_within_per_capita, hh_weights))%>%
    ungroup()
  
  File_2b <- File_2 %>%
    group_by(Income_Group_5)%>%
    summarise(burden_mean = weighted.mean(burden_CO2_per_capita, hh_weights))%>%
    ungroup()
  
  File_2 <- File_2 %>%
    left_join(File_2a, by = "Income_Group_5")%>%
    left_join(File_2b, by = "Income_Group_5")
  
  File_2c <- File_2 %>%
    group_by(Income_Group_5)%>%
    summarise(
      y5  = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.05),
      y25 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.25),
      y50 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.5),
      y75 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.75),
      y95 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.95),
      mean = wtd.mean(burden_CO2_within_per_capita, weights = hh_weights))%>%
    ungroup()

  Files <- list("File_1" = File_1, "File_2" = File_2, "File_3" = File_2c)
  
  return(Files)
}

shape.for.graphical.analysis.2 <- function(File, Code.Urban){
  File <- File %>%
    left_join(Code.Urban, by = "urban")%>%
    select(hh_id, hh_size, hh_weights, Urban, Income_Group_5, burden_CO2_per_capita, burden_CO2_within_per_capita, burden_electricity_per_capita, burden_TRSP_per_capita, burden_ELY_CO2_per_capita)
  
  File_1 <- File %>%
    group_by(Income_Group_5)%>%
    summarise(
      wtd.median_CO2        = wtd.quantile(burden_CO2_per_capita,         weight = hh_weights, probs = 0.5),
      wtd.median_CO2_within = wtd.quantile(burden_CO2_within_per_capita,  weight = hh_weights, probs = 0.5),
      wtd.median_ely        = wtd.quantile(burden_electricity_per_capita, weight = hh_weights, probs = 0.5),
      wtd.median_TRSP       = wtd.quantile(burden_TRSP_per_capita,        weight = hh_weights, probs = 0.5),
      wtd.median_ELY_CO2    = wtd.quantile(burden_ELY_CO2_per_capita,     weight = hh_weights, probs = 0.5),
      
      wtd.25_CO2        = wtd.quantile(burden_CO2_per_capita,         weight = hh_weights, probs = 0.25),
      wtd.25_CO2_within = wtd.quantile(burden_CO2_within_per_capita,  weight = hh_weights, probs = 0.25),
      wtd.25_ely        = wtd.quantile(burden_electricity_per_capita, weight = hh_weights, probs = 0.25),
      wtd.25_TRSP       = wtd.quantile(burden_TRSP_per_capita,        weight = hh_weights, probs = 0.25),
      wtd.25_ELY_CO2    = wtd.quantile(burden_ELY_CO2_per_capita,     weight = hh_weights, probs = 0.25),
      
      wtd.75_CO2        = wtd.quantile(burden_CO2_per_capita,         weight = hh_weights, probs = 0.75),
      wtd.75_CO2_within = wtd.quantile(burden_CO2_within_per_capita,  weight = hh_weights, probs = 0.75),
      wtd.75_ely        = wtd.quantile(burden_electricity_per_capita, weight = hh_weights, probs = 0.75),
      wtd.75_TRSP       = wtd.quantile(burden_TRSP_per_capita,        weight = hh_weights, probs = 0.75),
      wtd.75_ELY_CO2    = wtd.quantile(burden_ELY_CO2_per_capita,     weight = hh_weights, probs = 0.75)
      
    )%>%
    ungroup()
  
  File_2 <- File_1 %>%
    mutate(CO2              = wtd.median_CO2       /File_1$wtd.median_CO2[1],
           CO2_within       = wtd.median_CO2_within/File_1$wtd.median_CO2_within[1],
           ELY              = wtd.median_ely       /File_1$wtd.median_ely[1],
           TRSP             = wtd.median_TRSP      /File_1$wtd.median_TRSP[1],
           ELY_CO2          = wtd.median_ELY_CO2   /File_1$wtd.median_ELY_CO2[1])%>%
    mutate(CO2_low          = wtd.25_CO2           /File_1$wtd.median_CO2[1],
           CO2_within_low   = wtd.25_CO2_within    /File_1$wtd.median_CO2_within[1],
           ELY_low          = wtd.25_ely           /File_1$wtd.median_ely[1],
           TRSP_low         = wtd.25_TRSP          /File_1$wtd.median_TRSP[1],
           ELY_CO2_low      = wtd.25_ELY_CO2       /File_1$wtd.median_ELY_CO2[1])%>%
    mutate(CO2_upper        = wtd.75_CO2           /File_1$wtd.median_CO2[1],
           CO2_within_upper = wtd.75_CO2_within    /File_1$wtd.median_CO2_within[1],
           ELY_upper        = wtd.75_ely           /File_1$wtd.median_ely[1],
           TRSP_upper       = wtd.75_TRSP          /File_1$wtd.median_TRSP[1],
           ELY_CO2_upper    = wtd.75_ELY_CO2       /File_1$wtd.median_ELY_CO2[1])
  
  File_3 <- File_2 %>%
    select(Income_Group_5, CO2:ELY_CO2_upper, wtd.median_CO2_within)
  
  File_3a <- File_3 %>%
    select(Income_Group_5, CO2, CO2_low, CO2_upper)%>%
    mutate(Type_0 = "CO2")%>%
    rename(pure = CO2, low = CO2_low, upper = CO2_upper)
  
  File_3b <- File_3 %>%
    select(Income_Group_5, CO2_within, CO2_within_low, CO2_within_upper, wtd.median_CO2_within)%>%
    mutate(Type_0 = "CO2_within")%>%
    rename(pure = CO2_within, low = CO2_within_low, upper = CO2_within_upper)%>%
    mutate(wtd.median_CO2_within = ifelse(Income_Group_5 != 1, NA, round(wtd.median_CO2_within*100,1)))%>%
    mutate(label = ifelse(Income_Group_5 != 1, NA, paste0(wtd.median_CO2_within, "%")))
  
  File_3c <- File_3 %>%
    select(Income_Group_5, ELY, ELY_low, ELY_upper)%>%
    mutate(Type_0 = "ELY")%>%
    rename(pure = ELY, low = ELY_low, upper = ELY_upper)
  
  File_3d <- File_3 %>%
    select(Income_Group_5, TRSP, TRSP_low, TRSP_upper)%>%
    mutate(Type_0 = "TRSP")%>%
    rename(pure = TRSP, low = TRSP_low, upper = TRSP_upper)
  
  File_3e <- File_3 %>%
    select(Income_Group_5, ELY_CO2, ELY_CO2_low, ELY_CO2_upper)%>%
    mutate(Type_0 = "ELY_CO2")%>%
    rename(pure = ELY_CO2, low = ELY_CO2_low, upper = ELY_CO2_upper)
  
  #Add_on <- data.frame(Income_Group_5 = c(6,6,6), Type_0 = c("CO2", "ELY", "CO2_within"))
  
  File_4 <- bind_rows(File_3a, File_3b)%>%
    bind_rows(File_3c)%>%
    bind_rows(File_3d)%>%
    bind_rows(File_3e)
  
  File_final <- File_4  
  return(File_final)
}

shape.for.graphical.analysis.2.1 <- function(Incidence.X){
  
  Incidence.X.1 <- Incidence.X %>%
    select(Income_Group_5, type, value)%>%
    mutate(area = "nation")
  
  Incidence.X.2 <- Incidence.X %>%
    select(Income_Group_5, type, value.urban)%>%
    mutate(area = "urban")%>%
    rename(value = value.urban)
  
  Incidence.X.3 <- Incidence.X %>%
    select(Income_Group_5, type, value.rural)%>%
    mutate(area = "rural")%>%
    rename(value = value.rural)
  
  Incidence.X.4 <- Incidence.X.1 %>%
    rbind(Incidence.X.2)%>%
    rbind(Incidence.X.3)
  
  return(Incidence.X.4)
  
}

shape.for.graphical.analysis.2.2 <- function(Incidence.X, Country.Name){
  Incidence.Y <- Incidence.X %>%
    select(hh_id, hh_size, hh_weights, Income_Group_5, hh_expenditure_USD_pc, starts_with("burden"),
           starts_with("exp_"))%>%
    mutate(total_expenditures_CO2        = exp_pc_CO2*       hh_size*hh_weights,
           total_expenditures_CO2_within = exp_pc_CO2_within*hh_size*hh_weights,
           population                    = hh_size*hh_weights)
  
  no_households                     <- sum(Incidence.Y$hh_weights)
  no_population                     <- sum(Incidence.Y$population)
  total_expenditures_CO2_all        <- sum(Incidence.Y$total_expenditures_CO2)
  total_expenditures_CO2_within_all <- sum(Incidence.Y$total_expenditures_CO2_within)
  
  LST_CO2_per_hh                    <- total_expenditures_CO2_all/no_households
  LST_CO2_per_capita                <- total_expenditures_CO2_all/no_population
  LST_CO2_within_per_hh             <- total_expenditures_CO2_within_all/no_households
  LST_CO2_within_per_capita         <- total_expenditures_CO2_within_all/no_population  
  
  Incidence.Y.1 <- Incidence.Y %>%
    mutate(exp_pc_CO2_LST_hh        = -(exp_pc_CO2        - (LST_CO2_per_hh/hh_size)),
           exp_pc_CO2_LST_pc        = -(exp_pc_CO2        - LST_CO2_per_capita),
           exp_pc_CO2_within_LST_hh = -(exp_pc_CO2_within - (LST_CO2_within_per_hh/hh_size)),
           exp_pc_CO2_within_LST_pc = -(exp_pc_CO2_within - (LST_CO2_within_per_capita)),
           # Attention: Negative Values indicate positive budget change
           burden_CO2_pc_LST_hh        = exp_pc_CO2_LST_hh/hh_expenditure_USD_pc,
           burden_CO2_pc_LST_pc        = exp_pc_CO2_LST_pc/hh_expenditure_USD_pc,
           burden_CO2_within_pc_LST_hh = exp_pc_CO2_within_LST_hh/hh_expenditure_USD_pc,
           burden_CO2_within_pc_LST_pc = exp_pc_CO2_within_LST_pc/hh_expenditure_USD_pc,
           
           exp_pc_CO2                   = -exp_pc_CO2,
           exp_pc_CO2_within            = -exp_pc_CO2_within,
           burden_CO2_per_capita        = -burden_CO2_per_capita,
           burden_CO2_within_per_capita = -burden_CO2_within_per_capita)%>%
    select(hh_id, hh_weights, Income_Group_5, starts_with("burden_"), starts_with("exp"))
  
  Incidence.Y.2 <- Incidence.Y.1 %>%
    group_by(Income_Group_5)%>%
    summarise(
      exp_pc_CO2               = wtd.quantile(exp_pc_CO2              , probs = 0.5, weights = hh_weights),
      exp_pc_CO2_LST_pc        = wtd.quantile(exp_pc_CO2_LST_pc       , probs = 0.5, weights = hh_weights),
      exp_pc_CO2_LST_hh        = wtd.quantile(exp_pc_CO2_LST_hh       , probs = 0.5, weights = hh_weights),
      exp_pc_CO2_within        = wtd.quantile(exp_pc_CO2_within       , probs = 0.5, weights = hh_weights),
      exp_pc_CO2_within_LST_pc = wtd.quantile(exp_pc_CO2_within_LST_pc, probs = 0.5, weights = hh_weights),
      exp_pc_CO2_within_LST_hh = wtd.quantile(exp_pc_CO2_within_LST_hh, probs = 0.5, weights = hh_weights),
      
      
      burden_CO2_pc_no_LST = wtd.quantile(burden_CO2_per_capita, probs = 0.5, weights = hh_weights),
      burden_CO2_pc_LST_hh = wtd.quantile(burden_CO2_pc_LST_hh,  probs = 0.5, weights = hh_weights),
      burden_CO2_pc_LST_pc = wtd.quantile(burden_CO2_pc_LST_pc,  probs = 0.5, weights = hh_weights),
      
      burden_CO2_within_pc_no_LST = wtd.quantile(burden_CO2_within_per_capita, probs = 0.5, weights = hh_weights),
      burden_CO2_within_pc_LST_hh = wtd.quantile(burden_CO2_within_pc_LST_hh,  probs = 0.5, weights = hh_weights),
      burden_CO2_within_pc_LST_pc = wtd.quantile(burden_CO2_within_pc_LST_pc,  probs = 0.5, weights = hh_weights))%>%
    ungroup()%>%
    mutate(Country = Country.Name)%>%
    select(Country, everything())
  
  list_0 <- list("Full_df" = Incidence.Y.1, "Summarised_df" = Incidence.Y.2)
  
  return(list_0)
  
}

shape.for.graphical.analysis.3 <- function(Incidence.X){
  Incidence.X.1 <- Incidence.X %>%
    group_by(Income_Group_10)%>%
    summarise(
      y5  = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.05),
      y25 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.25),
      y50 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.5),
      y75 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.75),
      y95 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.95),
      mean = wtd.mean(burden_CO2_within_per_capita, weights = hh_weights))%>%
    ungroup()
  
  return(Incidence.X.1)
}

shape.for.graphical.analysis.4 <- function(Incidence.X){
  Incidence.X.1 <- Incidence.X %>%
    group_by(Income_Group_5)%>%
    summarise(
      y5  = wtd.quantile(burden_CO2_per_capita, weights = hh_weights, probs = 0.05),
      y25 = wtd.quantile(burden_CO2_per_capita, weights = hh_weights, probs = 0.25),
      y50 = wtd.quantile(burden_CO2_per_capita, weights = hh_weights, probs = 0.5),
      y75 = wtd.quantile(burden_CO2_per_capita, weights = hh_weights, probs = 0.75),
      y95 = wtd.quantile(burden_CO2_per_capita, weights = hh_weights, probs = 0.95),
      mean = wtd.mean(burden_CO2_per_capita, weights = hh_weights))%>%
    ungroup()
  
  return(Incidence.X.1)
}

shape.for.graphical.analysis.5 <- function(Incidence.X){
  Incidence.X.1 <- Incidence.X %>%
    group_by(Income_Group_5)%>%
    summarise(
      y5  = wtd.quantile(burden_electricity_per_capita, weights = hh_weights, probs = 0.05),
      y25 = wtd.quantile(burden_electricity_per_capita, weights = hh_weights, probs = 0.25),
      y50 = wtd.quantile(burden_electricity_per_capita, weights = hh_weights, probs = 0.5),
      y75 = wtd.quantile(burden_electricity_per_capita, weights = hh_weights, probs = 0.75),
      y95 = wtd.quantile(burden_electricity_per_capita, weights = hh_weights, probs = 0.95),
      mean = wtd.mean(burden_electricity_per_capita, weights = hh_weights))%>%
    ungroup()
  
  return(Incidence.X.1)
}

shape.for.graphical.analysis.6 <- function(Incidence.X, Code.Urban){
  File <- Incidence.X %>%
    left_join(Code.Urban, by = "urban")%>%
    select(hh_id, hh_size, hh_weights, Urban, Income_Group_5, burden_CO2_within_per_capita)%>%
    mutate(pop = hh_size*hh_weights)%>%
    mutate(pop_share = pop/sum(pop))
  
  File_a <- File %>%
    mutate(Urban = "National")
  
  File_1 <- rbind(File, File_a)%>%
    mutate(Urban = fct_reorder2(Urban, Urban, Urban))%>%
    arrange(hh_id, Income_Group_5)%>%
    group_by(Income_Group_5, Urban)%>%
    summarise(
      pop_share = sum(pop_share),
      y5  = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.05),
      y25 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.25),
      y50 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.5),
      y75 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.75),
      y95 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.95),
      mean = wtd.mean(burden_CO2_within_per_capita, weights = hh_weights)
    )%>%
    ungroup()%>%
    mutate(IG_new = factor(Income_Group_5, levels = c(5,4,3,2,1)))
}

# 2.1      Bangladesh ####

Incidence.Bangladesh   <- read_csv("Aggregated_Data/Incidence.Analysis.Bangladesh_11_2020.csv")
I.B <- left_join(Incidence.Bangladesh, Urban.New.1, by = "urban")
Incidence.Bangladesh.1 <- (shape.for.graphical.analysis.1(Incidence.Bangladesh, Urban.New.1))$File_4
Incidence.Bangladesh.1.1 <- shape.for.graphical.analysis.1.1(Incidence.Bangladesh.1)$File_1
Incidence.Bangladesh.1.2 <- shape.for.graphical.analysis.3(Incidence.Bangladesh)
Incidence.Bangladesh.1.3 <- shape.for.graphical.analysis.4(Incidence.Bangladesh)
Incidence.Bangladesh.1.4 <- shape.for.graphical.analysis.5(Incidence.Bangladesh)
Incidence.Bangladesh.1.5 <- shape.for.graphical.analysis.6(Incidence.Bangladesh, Urban.New.1)
Incidence.Bangladesh.2 <- shape.for.graphical.analysis.2(Incidence.Bangladesh, Urban.New.1)
Incidence.Bangladesh.2.2.1 <- shape.for.graphical.analysis.2.2(Incidence.Bangladesh, "Bangladesh")$Full_df
Incidence.Bangladesh.2.2.2 <- shape.for.graphical.analysis.2.2(Incidence.Bangladesh, "Bangladesh")$Summarised_df

# 2.2      India ####

Incidence.India.01   <- read_csv("Aggregated_Data/Incidence.Analysis.India_11_2020_1.csv")
Incidence.India.02   <- read_csv("Aggregated_Data/Incidence.Analysis.India_11_2020_2.csv")
Incidence.India      <- rbind(Incidence.India.01, Incidence.India.02)

I.Ii <- left_join(Incidence.India, Urban.New.1, by = "urban")
Incidence.India.1 <- (shape.for.graphical.analysis.1(Incidence.India, Urban.New.1))$File_4
Incidence.India.1.1 <- shape.for.graphical.analysis.1.1(Incidence.India.1)$File_1
Incidence.India.1.2 <- shape.for.graphical.analysis.3(Incidence.India)
Incidence.India.1.3 <- shape.for.graphical.analysis.4(Incidence.India)
Incidence.India.1.4 <- shape.for.graphical.analysis.5(Incidence.India)
Incidence.India.1.5 <- shape.for.graphical.analysis.6(Incidence.India, Urban.New.1)
Incidence.India.2 <- shape.for.graphical.analysis.2(Incidence.India, Urban.New.1)
Incidence.India.2.2.1 <- shape.for.graphical.analysis.2.2(Incidence.India, "India")$Full_df
Incidence.India.2.2.2 <- shape.for.graphical.analysis.2.2(Incidence.India, "India")$Summarised_df

# 2.3      Indonesia ####

Incidence.Indonesia.1 <- read_csv("Aggregated_Data/Incidence.Analysis.Indonesia_11_2020_1.csv")
Incidence.Indonesia.2 <- read_csv("Aggregated_Data/Incidence.Analysis.Indonesia_11_2020_2.csv")
Incidence.Indonesia.3 <- read_csv("Aggregated_Data/Incidence.Analysis.Indonesia_11_2020_3.csv")
Incidence.Indonesia.4 <- read_csv("Aggregated_Data/Incidence.Analysis.Indonesia_11_2020_4.csv")
Incidence.Indonesia.5 <- read_csv("Aggregated_Data/Incidence.Analysis.Indonesia_11_2020_5.csv")
Incidence.Indonesia <- rbind(Incidence.Indonesia.1, Incidence.Indonesia.2)%>%
  rbind(Incidence.Indonesia.3)%>%
  rbind(Incidence.Indonesia.4)%>%
  rbind(Incidence.Indonesia.5)

I.Io <- left_join(Incidence.Indonesia, Urban.New.2, by = "urban")
Incidence.Indonesia.1 <- (shape.for.graphical.analysis.1(Incidence.Indonesia, Urban.New.2))$File_4
Incidence.Indonesia.1.1 <- shape.for.graphical.analysis.1.1(Incidence.Indonesia.1)$File_1
Incidence.Indonesia.1.2 <- shape.for.graphical.analysis.3(Incidence.Indonesia)
Incidence.Indonesia.1.3 <- shape.for.graphical.analysis.4(Incidence.Indonesia)
Incidence.Indonesia.1.4 <- shape.for.graphical.analysis.5(Incidence.Indonesia)
Incidence.Indonesia.1.5 <- shape.for.graphical.analysis.6(Incidence.Indonesia, Urban.New.2)
Incidence.Indonesia.2 <- shape.for.graphical.analysis.2(Incidence.Indonesia, Urban.New.2)
Incidence.Indonesia.2.2.1 <- shape.for.graphical.analysis.2.2(Incidence.Indonesia, "Indonesia")$Full_df
Incidence.Indonesia.2.2.2 <- shape.for.graphical.analysis.2.2(Incidence.Indonesia, "Indonesia")$Summarised_df

# 2.4      Pakistan ####

Incidence.Pakistan   <- read_csv("Aggregated_Data/Incidence.Analysis.Pakistan_11_2020.csv")
I.P <- left_join(Incidence.Pakistan, Urban.New.1, by = "urban")
Incidence.Pakistan.1 <- (shape.for.graphical.analysis.1(Incidence.Pakistan, Urban.New.1))$File_4
Incidence.Pakistan.1.1 <- shape.for.graphical.analysis.1.1(Incidence.Pakistan.1)$File_1
Incidence.Pakistan.1.2 <- shape.for.graphical.analysis.3(Incidence.Pakistan)
Incidence.Pakistan.1.3 <- shape.for.graphical.analysis.4(Incidence.Pakistan)
Incidence.Pakistan.1.4 <- shape.for.graphical.analysis.5(Incidence.Pakistan)
Incidence.Pakistan.1.5 <- shape.for.graphical.analysis.6(Incidence.Pakistan, Urban.New.1)
Incidence.Pakistan.2 <- shape.for.graphical.analysis.2(Incidence.Pakistan, Urban.New.1)
Incidence.Pakistan.2.2.1 <- shape.for.graphical.analysis.2.2(Incidence.Pakistan, "Pakistan")$Full_df
Incidence.Pakistan.2.2.2 <- shape.for.graphical.analysis.2.2(Incidence.Pakistan, "Pakistan")$Summarised_df

# 2.5      Philippines ####

Incidence.Philippines   <- read_csv("Aggregated_Data/Incidence.Analysis.Philippines_11_2020.csv")
I.P <- left_join(Incidence.Philippines, Urban.New.1, by = "urban")
Incidence.Philippines.1 <- (shape.for.graphical.analysis.1   (Incidence.Philippines, Urban.New.1))$File_4
Incidence.Philippines.1.1 <- shape.for.graphical.analysis.1.1(Incidence.Philippines.1)$File_1
Incidence.Philippines.1.2 <- shape.for.graphical.analysis.3  (Incidence.Philippines)
Incidence.Philippines.1.3 <- shape.for.graphical.analysis.4  (Incidence.Philippines)
Incidence.Philippines.1.4 <- shape.for.graphical.analysis.5  (Incidence.Philippines)
Incidence.Philippines.1.5 <- shape.for.graphical.analysis.6  (Incidence.Philippines, Urban.New.1)
Incidence.Philippines.2 <- shape.for.graphical.analysis.2    (Incidence.Philippines, Urban.New.1)
Incidence.Philippines.2.2.1 <- shape.for.graphical.analysis.2.2(Incidence.Philippines, "Philippines")$Full_df
Incidence.Philippines.2.2.2 <- shape.for.graphical.analysis.2.2(Incidence.Philippines, "Philippines")$Summarised_df

# 2.6      Thailand ####

Incidence.Thailand   <- read_csv("Aggregated_Data/Incidence.Analysis.Thailand_11_2020.csv")
I.P <- left_join(Incidence.Thailand, Urban.New.1, by = "urban")
Incidence.Thailand.1 <- (shape.for.graphical.analysis.1   (Incidence.Thailand, Urban.New.1))$File_4
Incidence.Thailand.1.1 <- shape.for.graphical.analysis.1.1(Incidence.Thailand.1)$File_1
Incidence.Thailand.1.2 <- shape.for.graphical.analysis.3  (Incidence.Thailand)
Incidence.Thailand.1.3 <- shape.for.graphical.analysis.4  (Incidence.Thailand)
Incidence.Thailand.1.4 <- shape.for.graphical.analysis.5  (Incidence.Thailand)
Incidence.Thailand.1.5 <- shape.for.graphical.analysis.6  (Incidence.Thailand, Urban.New.1)
Incidence.Thailand.2 <- shape.for.graphical.analysis.2    (Incidence.Thailand, Urban.New.1)
Incidence.Thailand.2.2.1 <- shape.for.graphical.analysis.2.2(Incidence.Thailand, "Thailand")$Full_df
Incidence.Thailand.2.2.2 <- shape.for.graphical.analysis.2.2(Incidence.Thailand, "Thailand")$Summarised_df

# 2.7      Turkey ####

Incidence.Turkey   <- read_csv("Aggregated_Data/Incidence.Analysis.Turkey_11_2020.csv")
I.Tu <- left_join(Incidence.Turkey, Urban.New.1, by = "urban")
Incidence.Turkey.1 <- (shape.for.graphical.analysis.1(Incidence.Turkey, Urban.New.1))$File_4
Incidence.Turkey.1.1 <- shape.for.graphical.analysis.1.1(Incidence.Turkey.1)$File_1
Incidence.Turkey.1.2 <- shape.for.graphical.analysis.3(Incidence.Turkey)
Incidence.Turkey.1.3 <- shape.for.graphical.analysis.4(Incidence.Turkey)
Incidence.Turkey.1.4 <- shape.for.graphical.analysis.5(Incidence.Turkey)
Incidence.Turkey.1.5 <- shape.for.graphical.analysis.6(Incidence.Turkey, Urban.New.1)
Incidence.Turkey.2 <- shape.for.graphical.analysis.2(Incidence.Turkey, Urban.New.1)
Incidence.Turkey.2.2.1 <- shape.for.graphical.analysis.2.2(Incidence.Turkey, "Turkey")$Full_df
Incidence.Turkey.2.2.2 <- shape.for.graphical.analysis.2.2(Incidence.Turkey, "Turkey")$Summarised_df

# 2.8      Vietnam ####

Incidence.Vietnam   <- read_csv("Aggregated_Data//Incidence.Analysis.Vietnam_11_2020.csv")
I.V <- left_join(Incidence.Vietnam, Urban.New.1, by = "urban")
Incidence.Vietnam.1 <- (shape.for.graphical.analysis.1(Incidence.Vietnam, Urban.New.2))$File_4
Incidence.Vietnam.1.1 <- shape.for.graphical.analysis.1.1(Incidence.Vietnam.1)$File_1
Incidence.Vietnam.1.2 <- shape.for.graphical.analysis.3(Incidence.Vietnam)
Incidence.Vietnam.1.3 <- shape.for.graphical.analysis.4(Incidence.Vietnam)
Incidence.Vietnam.1.4 <- shape.for.graphical.analysis.5(Incidence.Vietnam)
Incidence.Vietnam.1.5 <- shape.for.graphical.analysis.6(Incidence.Vietnam, Urban.New.2)
Incidence.Vietnam.2 <- shape.for.graphical.analysis.2(Incidence.Vietnam, Urban.New.2)
Incidence.Vietnam.2.2.1 <- shape.for.graphical.analysis.2.2(Incidence.Vietnam, "Vietnam")$Full_df
Incidence.Vietnam.2.2.2 <- shape.for.graphical.analysis.2.2(Incidence.Vietnam, "Vietnam")$Summarised_df

# 3        Functions / Graphical Output ####

ppi <- 400

# MAIN FIGURES ####
# 4        Normalized Figure                                (Figure 1)  ####

normalize_new <- function(Incidence.X, Country.Name, limit_low, limit_up, step_0, XLAB = "", YLAB = "", ATT = element_text(size = 7), ATX = element_text(size = 6), ATY = element_text(size = 6)){
  #Incidence.X <- Incidence.X %>%
  #  select(-value.urban, - value.rural)
  Incidence.X <- Incidence.X %>%
    rename(type = Type_0)%>%
    mutate(Label_2 = ifelse(Country.Name == "Pakistan" & type == "TRSP" & Income_Group_5 > 3, round(pure,2), NA))%>%
    filter(type != "ELY")
  
  
  Incidence.X$help <- paste(Incidence.X$Income_Group_5, "_", Incidence.X$type)
  
  if(Country.Name == "Thailand" | Country.Name == "Turkey" | Country.Name == "Indonesia" | Country.Name == "India") nudge_0 <- -0.25 else nudge_0 <- -0.25
  
  P.1 <- ggplot(Incidence.X, aes(x = factor(Income_Group_5)))+
    geom_hline(yintercept = 1, colour = "black", size = 0.3)+
    #geom_ribbon(aes(ymin = low, ymax = upper, group = type, fill = type), alpha = 0.2)+
    geom_label_repel(aes(y = 1,    group = type,  label = label),   size = 1.6, segment.linetype = 1, segment.size = 0.1, box.padding = 0.00, label.padding = 0.10, label.r = 0.05, direction = "y", min.segment.length = 0, nudge_y = nudge_0)+
    geom_label_repel(aes(y = 3,    group = type,  label = Label_2), size = 1.6, segment.linetype = 1, segment.size = 0.1, box.padding = 0.00, label.padding = 0.10, label.r = 0.05, direction = "y", min.segment.length = 0, nudge_y = -0.6)+
    #geom_label_repel(aes(y = pure, group = type, segment.linetype = 1, label = label_emissions_coverage, segment.size = 1, size = 15), min.segment.length = 0, hjust = 1, force_pull = 0, nudge_x = 1)+
    geom_line(aes( y = pure, group = type, colour = type, alpha = type), size = 0.4, position = position_dodge(0.2))+
    geom_point(aes(y = pure, group = type, fill = type, shape = type, alpha = type), size = 1.5, colour = "black", position = position_dodge(0.2), stroke = 0.2)+
    scale_colour_npg(  labels = c("International Carbon Price","National Carbon Price", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price")) +
    scale_fill_npg  (  labels = c("International Carbon Price","National Carbon Price", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price"))+
    scale_shape_manual(labels = c("International Carbon Price","National Carbon Price", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price"), values = c(21,22,23,24,25))+
    scale_alpha_manual(labels = c("International Carbon Price","National Carbon Price", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price"), values = c(1,1,1,1,1))+
    labs(fill = "", colour = "", shape = "", alpha = "", linetype = "")+
    theme_bw() + 
    scale_x_discrete(labels = c("1","2","3","4","5"))+
    scale_y_continuous(breaks = seq(limit_low, limit_up, step_0))+
    theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 7), legend.position = "bottom", strip.text = element_text(size = 7), strip.text.y = element_text(angle = 180), panel.grid.major = element_line(size = 0.3), panel.grid.minor = element_blank(), axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7), legend.title = element_text(size = 7), plot.margin = unit(c(0.1,0.2,0,0), "cm"), panel.border = element_rect(size = 0.3))+
    coord_cartesian(ylim = c(limit_low-0.0, (limit_up+0.0)))+
    #guides(fill = guide_legend(nrow = 2, order = 1), colour = guide_legend(nrow = 2, order = 1), shape = guide_legend(nrow = 2, order = 1), alpha = FALSE, size = FALSE)+
    guides(fill = FALSE, colour = FALSE, shape = FALSE, size = FALSE, alpha = FALSE)+
    xlab(XLAB)+ylab(YLAB)+ ggtitle(Country.Name)
  
  return(P.1)
}

P.1.BAN   <- normalize_new(Incidence.Bangladesh.2,  "Bangladesh",  0.5, 2.5, 0.50, ATX = element_blank(), YLAB = "Incidence normalized by first Quintile")
P.1.India <- normalize_new(Incidence.India.2,       "India",       0.5, 2.5, 0.50, ATX = element_blank(), ATY = element_blank())
P.1.IDN   <- normalize_new(Incidence.Indonesia.2,   "Indonesia",   0.5, 2.5, 0.50, ATX = element_blank(), ATY = element_blank())
P.1.PAK   <- normalize_new(Incidence.Pakistan.2,    "Pakistan",    0.5, 2.5, 0.50, ATX = element_blank(), ATY = element_blank())
P.1.PHI   <- normalize_new(Incidence.Philippines.2, "Philippines", 0.5, 2.5, 0.50, XLAB = "Expenditure Quintile", YLAB = "Incidence normalized by first Quintile")
P.1.THA   <- normalize_new(Incidence.Thailand.2,    "Thailand",    0.5, 2.5, 0.50, XLAB = "Expenditure Quintile", ATY = element_blank())
P.1.TUR   <- normalize_new(Incidence.Turkey.2,      "Turkey",      0.5, 2.5, 0.50, XLAB = "Expenditure Quintile", ATY = element_blank())
P.1.VIE   <- normalize_new(Incidence.Vietnam.2,     "Vietnam",     0.5, 2.5, 0.50, XLAB = "Expenditure Quintile", ATY = element_blank())

P.10 <- cowplot::align_plots(P.1.BAN, P.1.India, P.1.IDN, P.1.PAK, P.1.PHI, P.1.THA, P.1.TUR, P.1.VIE, align = "hv")
s.1 <- ggdraw(P.10[[1]])
s.2 <- ggdraw(P.10[[2]])
s.3 <- ggdraw(P.10[[3]])
s.4 <- ggdraw(P.10[[4]])
s.5 <- ggdraw(P.10[[5]])
s.6 <- ggdraw(P.10[[6]])
s.7 <- ggdraw(P.10[[7]])
s.8 <- ggdraw(P.10[[8]])

# png("Figures/Figure_1_%d.png", width = 15, height = 16, unit = "cm", res = ppi)
# 
# s.1
# s.2
# s.3
# s.4
# s.5
# s.6
# s.7
# s.8
# 
# dev.off()

# Requires running line 527 uncommented and line 528 uncommented

L.1 <- get_legend(P.1.BAN)


# 4.1      Included Emissions ####

agg_emissions_ban <- read_csv("Aggregated_Data/Emissions/Agg_Emissions_Bangladesh.csv")
agg_emissions_ini <- read_csv("Aggregated_Data/Emissions/Agg_Emissions_India.csv")
agg_emissions_ino <- read_csv("Aggregated_Data/Emissions/Agg_Emissions_Indonesia.csv")
agg_emissions_pak <- read_csv("Aggregated_Data/Emissions/Agg_Emissions_Pakistan.csv")
agg_emissions_phi <- read_csv("Aggregated_Data/Emissions/Agg_Emissions_Philippines.csv")
agg_emissions_tha <- read_csv("Aggregated_Data/Emissions/Agg_Emissions_Thailand.csv")
agg_emissions_tur <- read_csv("Aggregated_Data/Emissions/Agg_Emissions_Turkey.csv")
agg_emissions_vie <- read_csv("Aggregated_Data/Emissions/Agg_Emissions_Vietnam.csv")

plot_function <- function(data_x, ATY = element_text(size = 6), ATT = element_text(size = 7)){
  
  P.1 <- ggplot(data_x, aes(x = Type, y = share))+
    geom_col(aes(fill = Type), colour = "black", width = 0.75, size = 0.2)+
    geom_label_repel(aes(y = 0.95, group = Type, label = Label), size = 1.5, segment.linetype = 1, direction = "x", min.segment.length = 0, nudge_x = 0.1, label.size = 0.1, label.padding = 0.05, box.padding = 0, label.r = 0.01)+
    theme_bw()+
    coord_cartesian(ylim = c(0,1.02))+
    scale_fill_npg()+
    guides(fill = FALSE, size = FALSE)+
    ylab(bquote('Share of covered ' ~CO[2]~ 'Emissions')) + xlab("")+ 
    scale_y_continuous(labels = percent_format(), expand = c(0,0))+
    ggtitle("")+
    theme(axis.text.y = ATY, axis.text.x = element_blank(), axis.title = ATT, plot.title = element_text(size = 7), legend.position = "bottom", strip.text = element_text(size = 7), strip.text.y = element_text(angle = 180), panel.grid.major = element_line(size = 0.3), panel.grid.minor = element_blank(), axis.ticks.y = element_line(size = 0.2), axis.ticks.x = element_blank(),
          legend.text = element_text(size = 7), legend.title = element_text(size = 7), plot.margin = unit(c(0.1,0.1,0,0), "cm"), panel.border = element_rect(size = 0.3), legend.background = element_rect(fill = "white", colour = "black"))
  
}

plot_ban <- plot_function(agg_emissions_ban)
plot_ini <- plot_function(agg_emissions_ini, ATY = element_blank(), ATT = element_blank())
plot_ino <- plot_function(agg_emissions_ino, ATY = element_blank(), ATT = element_blank())
plot_pak <- plot_function(agg_emissions_pak, ATY = element_blank(), ATT = element_blank())
plot_phi <- plot_function(agg_emissions_phi)
plot_tha <- plot_function(agg_emissions_tha, ATY = element_blank(), ATT = element_blank())
plot_tur <- plot_function(agg_emissions_tur, ATY = element_blank(), ATT = element_blank())
plot_vie <- plot_function(agg_emissions_vie, ATY = element_blank(), ATT = element_blank())

Figure_A <- ggarrange(P.1.India, plot_ini, P.1.IDN, plot_ino, P.1.PAK, plot_pak, 
                      P.1.THA, plot_tha, P.1.TUR, plot_tur, P.1.VIE, plot_vie, ncol = 6, nrow = 2, widths = c(1,0.25,1,0.25,1,0.25), align = "h")
Figure_B <- ggarrange(P.1.BAN, plot_ban, P.1.PHI, plot_phi, align = "h", nrow = 2, ncol = 2, widths = c(1,0.5,1,0.5))
Figure_C <- ggarrange(Figure_B, Figure_A, ncol = 2, widths = c(1,2.21), common.legend = TRUE, legend = "bottom", legend.grob = L.1)

pdf("Figures/Figure_1.pdf", width = 16.51/2.54, height = 10.75/2.54)
Figure_C
dev.off()


# 5        Distributions                                    (Figure 2+3) ####

calculate_median <- function(x){
  x <- x %>%
    group_by(Income_Group_5)%>%
    mutate(cumsum_shares = cumsum(share))%>%
    filter(cumsum_shares >= 0.5)%>%
    slice(which.min(cumsum_shares))%>%
    ungroup()%>%
    rename(median = burden_CO2_within_per_capita)%>%
    select(Income_Group_5, median)
}
calculate_median_y <- function(x0, xmedian, adjust_0){
  ggplot_build(ggplot(x0, aes(y = share, x = burden_CO2_within_per_capita, group = factor(Income_Group_5)))+
                 geom_smooth(method = "loess", span = adjust_0, se = FALSE))$data[[1]]%>%
    select(x,y,group)%>%
    left_join(xmedian, by = c("group" = "Income_Group_5"))%>%
    mutate(help = median - x)%>%
    mutate(help_0 = ifelse(help <0, help*-1, help))%>%
    group_by(group)%>%
    filter(help_0 == min(help_0))%>%
    ungroup()%>%
    rename(Income_Group_5 = group, median.x = x, median.y = y)%>%
    select(-median, -help, -help_0)%>%
    select(median.x, median.y, Income_Group_5)
}

plotting_ten.1 <- function(Incidence.X, Country.Name, fill0 = FALSE, ATY = element_blank(), ATX = element_text(size = 20), ATT = element_text(size = 25), XLAB = "", YLAB = "", YLIM = 0.01, adjust_1 = 0.2){
  adjust_0 <- adjust_1
  
  # Round Values up, calculate households per bins
  Incidence.X0 <- Incidence.X %>%
    filter(Income_Group_5 != 0)%>%
    mutate(burden_CO2_within_per_capita = round(burden_CO2_within_per_capita,3))%>%
    group_by(Income_Group_5, Urban, burden_CO2_within_per_capita)%>%
    summarise(weights = sum(hh_weights))%>%
    ungroup()
  # Calculate total households
  IG_weights <- Incidence.X %>%
    filter(Income_Group_5 != 0)%>%
    group_by(Income_Group_5, Urban)%>%
    summarise(IG_weights = sum(hh_weights))%>%
    ungroup()
  # Calculate Shares
  Incidence.X0 <- left_join(Incidence.X0, IG_weights)%>%
    mutate(share = weights/IG_weights)
  
  # Select Subsamples
  Incidence.X1 <- Incidence.X0 %>%
    filter(Urban == "Urban")
  Incidence.X2 <- Incidence.X0 %>%
    filter(Urban == "Rural")
  Incidence.X3 <- Incidence.X0 %>%
    filter(Urban == "Country")
  
  #Calculate Median
  Median.X1 <- calculate_median(Incidence.X1)
  Median.X2 <- calculate_median(Incidence.X2)
  Median.X3 <- calculate_median(Incidence.X3) 
  
  # Calculate Median Y
  t.X.1 <- calculate_median_y(Incidence.X1, Median.X1, adjust_0)
  t.X.2 <- calculate_median_y(Incidence.X2, Median.X2, adjust_0)
  t.X.3 <- calculate_median_y(Incidence.X3, Median.X3, adjust_0)
  
  Incidence.X1.1 <- Incidence.X1 %>%
    left_join(t.X.1, by = c("Income_Group_5"))
  Incidence.X2.1 <- Incidence.X2 %>%
    left_join(t.X.2, by = c("Income_Group_5"))
  Incidence.X3.1 <- Incidence.X3 %>%
    left_join(t.X.3, by = c("Income_Group_5"))

  max_median <- max(Incidence.X3.1$median.x)
  min_median <- min(Incidence.X3.1$median.x)
  
  P_3 <- ggplot(Incidence.X3.1, aes(group = factor(Income_Group_5), colour = factor(Income_Group_5), linetype = factor(Income_Group_5)))+
    theme_bw()+
    theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 7), legend.position = "bottom", strip.text = element_text(size = 7), strip.text.y = element_text(angle = 180), panel.grid.major = element_line(size = 0.3), panel.grid.minor = element_blank(), axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7), legend.title = element_text(size = 7), plot.margin = unit(c(0.1,0.1,0,0), "cm"), panel.border = element_rect(size = 0.3))+
    annotate("rect", xmin = min_median, xmax = max_median, ymin = 0, ymax = 0.11, alpha = 0.5, fill = "grey")+
    annotate("segment", x = min_median, xend = max_median, y = 0.098, yend = 0.098, arrow = arrow(ends = "both", angle = 90, length = unit (.05, "cm")), size = 0.2)+
    annotate("text", x = (min_median + max_median)/2, y = 0.101, label = "paste(Delta, V)", parse = TRUE, size = 1.5)+
    geom_point(aes(x = median.x, y = median.y, group = factor(Income_Group_5), fill = factor(Income_Group_5)), shape = 21, size = 1.3, stroke = 0.2, colour = "black")+
    geom_smooth(aes(x = burden_CO2_within_per_capita, y = share), size = 0.3, method = "loess", n = 160, span = adjust_0, se = FALSE, fullrange = TRUE)+
    xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", linetype = "", fill = "")+
    scale_y_continuous(breaks = c(0,0.05,0.1), expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
    scale_x_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1), breaks = seq(0,0.08, 0.02))+
    coord_cartesian(xlim = c(0,0.085), ylim = c(0,0.105))+
    #geom_segment(aes(x = median, xend = median, y = 0, yend = 100, colour = factor(Income_Group_5), linetype = factor(Income_Group_5)), size = 1)+
    scale_colour_manual(  values = c("#BC3C29FF","#7876B1FF","#000000","#20854EFF",   "#0072B5FF"))+
    scale_fill_manual(    values = c("#BC3C29FF","#7876B1FF","#000000","#20854EFF",   "#0072B5FF"))+
    scale_linetype_manual(values = c("solid", "dashed", "dashed", "dashed", "solid"))+
    ggtitle(Country.Name)+
    #guides(fill = guide_legend("Expenditure Quintile"), colour = guide_legend("Expenditure Quintile"), linetype = guide_legend("Expenditure Quintile"))
    guides(fill = fill0, colour = fill0, linetype = fill0)
  
  Incidence.X.4 <- rbind(Incidence.X1.1, Incidence.X2.1)%>%
    filter(Income_Group_5 == 1 | Income_Group_5 == 5)
  
  
  P_4 <- ggplot(Incidence.X.4, aes(colour = factor(Income_Group_5), linetype = factor(Urban)))+
    theme_bw()+
    theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 7), legend.position = "bottom", strip.text = element_text(size = 7), strip.text.y = element_text(angle = 180), panel.grid.major = element_line(size = 0.3), panel.grid.minor = element_blank(), axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7), legend.title = element_text(size = 7), plot.margin = unit(c(0.1,0.1,0,0), "cm"), panel.border = element_rect(size = 0.3))+
    geom_smooth(aes(x = burden_CO2_within_per_capita, y = share), size = 0.3, method = "loess", span = adjust_0, se = FALSE, n = 160, fullrange = TRUE)+
    geom_point(aes(x = median.x, y = median.y, fill = factor(Income_Group_5), alpha = factor(Urban)), shape = 21, size = 1.3, stroke = 0.2, colour = "black")+
    xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", linetype = "", fill = "")+
    scale_y_continuous(breaks = c(0,0.05, 0.1), expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
    scale_x_continuous(expand = c(0,0), breaks = seq(0,0.08, 0.02), labels = scales::percent_format(accuracy = 1))+
    coord_cartesian(xlim = c(0,0.085), ylim = c(0,0.105))+
    #geom_segment(aes(x = median, xend = median, y = 0, yend = 100, colour = factor(Income_Group_5), linetype = factor(Income_Group_5)), size = 1)+
    scale_colour_manual(  values = c("#BC3C29FF", "#0072B5FF"))+
    scale_fill_manual(    values = c("#BC3C29FF", "#0072B5FF"))+
    scale_linetype_manual(values = c("solid", "dotted"))+
    scale_alpha_manual(   values = c(1,0.5))+
    ggtitle(Country.Name)+
    #guides(fill = guide_legend("Expenditure Quintile"), alpha = FALSE, colour = guide_legend("Expenditure Quintile"))
    guides(fill = fill0, colour = fill0, linetype = fill0, alpha = fill0)
  
  Files <- list("Plot_3" = P_3, "Plot_4" = P_4)
  
  return(Files)
}

P.10.Ba.3 <- plotting_ten.1(Incidence.Bangladesh.1,                    "Bangladesh",  ATY = element_text(size = 6, vjust = 0.1),    ATX = element_blank(),         ATT = element_text(size = 7),          XLAB = "", YLAB = "Share of Households per Quintile")$Plot_3
P.10.Ii.3 <- plotting_ten.1(Incidence.India.1,       adjust_1 = 0.1,   "India",       ATY = element_blank(),                        ATX = element_blank(),         ATT = element_blank(),          XLAB = "")$Plot_3
P.10.Io.3 <- plotting_ten.1(Incidence.Indonesia.1,   adjust_1 = 0.1,   "Indonesia",   ATY = element_blank(),                        ATX = element_blank(),         ATT = element_blank(),          XLAB = "")$Plot_3
P.10.Pa.3 <- plotting_ten.1(Incidence.Pakistan.1,    adjust_1 = 0.1,   "Pakistan",    ATY = element_blank(),                        ATX = element_blank() ,        ATT = element_blank(),          XLAB = "")$Plot_3
P.10.Ph.3 <- plotting_ten.1(Incidence.Philippines.1, adjust_1 = 0.35,  "Philippines", ATY = element_text(size = 6, vjust = 0.1),    ATX = element_text(size = 6), ATT = element_text(size = 7),  XLAB = "Carbon Price Incidence", YLAB = "Share of Households per Quintile")$Plot_3
P.10.Th.3 <- plotting_ten.1(Incidence.Thailand.1,    adjust   = 0.15,    "Thailand",    ATY = element_blank(),                      ATX = element_text(size = 6), ATT = element_text(size = 7),  XLAB = "Carbon Price Incidence")$Plot_3
P.10.Tu.3 <- plotting_ten.1(Incidence.Turkey.1,      adjust_1 = 0.12,  "Turkey",      ATY = element_blank(),                        ATX = element_text(size = 6), ATT = element_text(size = 7),  XLAB = "Carbon Price Incidence")$Plot_3
P.10.Vi.3 <- plotting_ten.1(Incidence.Vietnam.1,                       "Vietnam",     ATY = element_blank(),                        ATX = element_text(size = 6), ATT = element_text(size = 7),  XLAB = "Carbon Price Incidence")$Plot_3

P.10 <- cowplot::align_plots(P.10.Ba.3, P.10.Ii.3, P.10.Io.3, P.10.Pa.3, P.10.Ph.3, P.10.Th.3, P.10.Tu.3, P.10.Vi.3, align = "hv")
s.1 <- ggdraw(P.10[[1]])
s.2 <- ggdraw(P.10[[2]])
s.3 <- ggdraw(P.10[[3]])
s.4 <- ggdraw(P.10[[4]])
s.5 <- ggdraw(P.10[[5]])
s.6 <- ggdraw(P.10[[6]])
s.7 <- ggdraw(P.10[[7]])
s.8 <- ggdraw(P.10[[8]])

# png("Figure_2_1%d.png", width = 15, height = 16, unit = "cm", res = ppi)
# 
# s.1
# s.2
# s.3
# s.4
# s.5
# s.6
# s.7
# s.8
# 
# dev.off()

# run this with line 711 uncommented and with line 712 commented
L.2 <- get_legend(P.10.Ba.3)

Figure_2 <- ggarrange(s.1, s.2, s.3, s.4, s.5, s.6, s.7, s.8, ncol = 4, nrow = 2, common.legend = TRUE, legend.grob = L.2, legend = "bottom")

pdf("Figures/Figure_2.pdf", width = 16.51/2.54, height = 10/2.54)
Figure_2
dev.off()

P.10.Ba.4 <- plotting_ten.1(Incidence.Bangladesh.1, adjust_1 = 0.25, "Bangladesh",  ATY = element_text(size = 6, vjust = 0.1), ATX = element_blank(),         ATT = element_text(size = 7),          XLAB = "", YLAB = "Share of Households per Quintile")$Plot_4
P.10.Ii.4 <- plotting_ten.1(Incidence.India.1,      adjust_1 = 0.12, "India",       ATY = element_blank(),                      ATX = element_blank(),         ATT = element_blank(),          XLAB = "")$Plot_4
P.10.Io.4 <- plotting_ten.1(Incidence.Indonesia.1,  adjust_1 = 0.15, "Indonesia",   ATY = element_blank(),                      ATX = element_blank(),         ATT = element_blank(),          XLAB = "")$Plot_4
P.10.Pa.4 <- plotting_ten.1(Incidence.Pakistan.1,   adjust_1 = 0.15, "Pakistan",    ATY = element_blank(),                      ATX = element_blank() ,        ATT = element_blank(),          XLAB = "")$Plot_4
P.10.Ph.4 <- plotting_ten.1(Incidence.Philippines.1,adjust_1 = 0.3, "Philippines",  ATY = element_text(size = 6, vjust = 0.1), ATX = element_text(size = 6), ATT = element_text(size = 7),  XLAB = "Carbon Price Incidence", YLAB = "Share of Households per Quintile")$Plot_4
P.10.Th.4 <- plotting_ten.1(Incidence.Thailand.1,   adjust_1 = 0.15, "Thailand",    ATY = element_blank(),                      ATX = element_text(size = 6), ATT = element_text(size = 7),  XLAB = "Carbon Price Incidence")$Plot_4
P.10.Tu.4 <- plotting_ten.1(Incidence.Turkey.1,     adjust_1 = 0.15, "Turkey",      ATY = element_blank(),                      ATX = element_text(size = 6), ATT = element_text(size = 7),  XLAB = "Carbon Price Incidence")$Plot_4
P.10.Vi.4 <- plotting_ten.1(Incidence.Vietnam.1,    adjust_1 = 0.3, "Vietnam",     ATY = element_blank(),                      ATX = element_text(size = 6), ATT = element_text(size = 7),  XLAB = "Carbon Price Incidence")$Plot_4

P.10 <- cowplot::align_plots(P.10.Ba.4, P.10.Ii.4, P.10.Io.4, P.10.Pa.4, P.10.Ph.4, P.10.Th.4, P.10.Tu.4, P.10.Vi.4, align = "hv")
s.1 <- ggdraw(P.10[[1]])
s.2 <- ggdraw(P.10[[2]])
s.3 <- ggdraw(P.10[[3]])
s.4 <- ggdraw(P.10[[4]])
s.5 <- ggdraw(P.10[[5]])
s.6 <- ggdraw(P.10[[6]])
s.7 <- ggdraw(P.10[[7]])
s.8 <- ggdraw(P.10[[8]])

# png("Figure_3_1%d.png", width = 15, height = 16, unit = "cm", res = ppi)
# 
# s.1
# s.2
# s.3
# s.4
# s.5
# s.6
# s.7
# s.8
# 
# dev.off()


# run this with line 735 commented and with line 734 uncommented
L.3 <- get_legend(P.10.Ba.4)

Figure_3 <- ggarrange(s.1, s.2, s.3, s.4, s.5, s.6, s.7, s.8, ncol = 4, nrow = 2, common.legend = TRUE, legend.grob = L.3, legend = "bottom")

pdf("Figures/Figure_3.pdf", width = 16.51/2.54, height = 10/2.54)
Figure_3
dev.off()

# 6        Non-parametric Engel-Curves                      (Figure 4) ####

# NOTE
# Confidential data. Access upon reasonable request and subject to approval by local statistics office.

#dec_1_BAN <- read.csv("../Bangladesh_data_for_decomposition_wide_GTAP10_new.csv")%>%
#  mutate(expenditures_USD_Gas = NA,
#         expenditures_USD_Coal = NA)
#dec_1_INI <- read.csv("../India_data_for_decomposition_wide_GTAP10_new.csv")%>%
#  mutate(expenditures_USD_Gas = NA)
#dec_1_INO <- read.csv("../Indonesia_data_for_decomposition_wide_GTAP10_new.csv")%>%
#  mutate(expenditures_USD_Coal = NA)
#dec_1_PAK <- read.csv("../Pakistan_data_for_decomposition_wide_GTAP10_new.csv")
#dec_1_PHI <- read.csv("../Philippines_data_for_decomposition_wide_GTAP10_new.csv")%>%
#  mutate(expenditures_USD_Coal = NA,
#         expenditures_USD_Gas = NA)
#dec_1_THA <- read.csv("../Decomposition_files_06_2020/Thailand_data_for_decomposition_wide_GTAP10_new.csv")%>%
#  mutate(expenditures_USD_Firewood = NA,
#         expenditures_USD_Coal = NA)
#dec_1_TUR <- read.csv("../Turkey_data_for_decomposition_wide_GTAP10_new.csv")%>%
#  mutate(expenditures_USD_Kerosene = NA,
#         expenditures_USD_Firewood = NA,
#         expenditures_USD_Charcoal = NA,
#         expenditures_USD_Diesel   = NA)
#dec_1_VIE <- read.csv("../Vietnam_data_for_decomposition_wide_GTAP10_new.csv")%>%
#  mutate(expenditures_USD_Gas = NA,
#         expenditures_USD_Kerosene = NA,
#         expenditures_USD_Charcoal = NA)

transform_0 <- function(x){
  y <- x %>%
    select(hh_id, hh_weights, hh_size, hh_tot_inc_USD, urban, starts_with("expenditures_USD"), exp_USD_ely)%>%
    replace(is.na(.), 0) %>%  
    mutate(exp_Biomass     = expenditures_USD_Firewood + expenditures_USD_Charcoal,
           exp_Cooking     = expenditures_USD_LPG      + expenditures_USD_Kerosene + expenditures_USD_Gas,
           exp_Transport   = expenditures_USD_Petrol   + expenditures_USD_Diesel,
           exp_Electricity = exp_USD_ely,
           exp_Coal        = expenditures_USD_Coal,
           exp_Energy      = exp_Biomass + exp_Cooking + exp_Transport + exp_Electricity + exp_Coal)%>%
    mutate(hh_expenditures_0_pc = hh_tot_inc_USD)%>%
    mutate(Income_Group_5   = as.numeric(binning(hh_expenditures_0_pc, bins=5  , method = c("wtd.quantile"), labels = seq(1,5,length.out = 5),  weights = hh_weights)))%>%
    mutate(Income_Group_100 = as.numeric(binning(hh_expenditures_0_pc, bins=100, method = c("wtd.quantile"), labels = seq(1,100,length.out=100), weights = hh_weights)))
  
  y1 <- y %>%
    select(hh_id, hh_weights, hh_size, hh_tot_inc_USD, Income_Group_100, Income_Group_5)
  
  y2 <- y %>%
    select(hh_id, starts_with("exp_"), - exp_USD_ely)%>%
    pivot_longer(!hh_id, names_to = "type", values_to ="exp_USD")
  
  y3 <- left_join(y2, y1, by = "hh_id")%>%
    group_by(Income_Group_100, type)%>%
    summarise(mean_exp_USD    = weighted.mean(exp_USD, hh_weights),
              mean_hh_inc_USD = weighted.mean(hh_tot_inc_USD, hh_weights),
              hh_weights      = sum(hh_weights))%>%
    ungroup()%>%
    mutate(share = mean_exp_USD/mean_hh_inc_USD)%>%
    filter(type != "exp_Energy")
  
  y4 <- left_join(y2, y1, by = "hh_id")%>%
    mutate(share = exp_USD/hh_tot_inc_USD)%>%
    mutate(exp_pc_hh = hh_tot_inc_USD/hh_size)
  
  y5 <- y %>%
    select(hh_id, hh_weights, hh_tot_inc_USD, Income_Group_5)%>%
    group_by(Income_Group_5)%>%
    summarise(avg.income = wtd.mean(hh_tot_inc_USD, hh_weights))%>%
    ungroup()
  
  out <- list('t1' = y3, 't2' = y4, 't3' = y5)
  
}

dec_2_BAN <- transform_0(dec_1_BAN)$t1
dec_2_INI <- transform_0(dec_1_INI)$t1
dec_2_INO <- transform_0(dec_1_INO)$t1
dec_2_PAK <- transform_0(dec_1_PAK)$t1
dec_2_PHI <- transform_0(dec_1_PHI)$t1
dec_2_THA <- transform_0(dec_1_THA)$t1
dec_2_TUR <- transform_0(dec_1_TUR)$t1
dec_2_VIE <- transform_0(dec_1_VIE)$t1


plot_engel <- function(data, Country.Name, fill0 = FALSE, ATY = element_blank(), ATX = element_text(size = 6, hjust = 1), ATT = element_text(size = 7), XLAB = "", YLAB = "", YLIM = 100){
  
  P <- ggplot(data)+
    geom_smooth(formula = as.formula (y ~ x), aes(x = Income_Group_100, y = share, weight = hh_weights, colour = factor(type), fill = factor(type)), size = 0.3, method = "loess", se = TRUE, fullrange = TRUE, span = 0.75)+
    theme_bw()+
    theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 7), legend.position = "bottom", strip.text = element_text(size = 7), strip.text.y = element_text(angle = 180), panel.grid.major = element_line(size = 0.3), panel.grid.minor = element_blank(), axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7), legend.title = element_text(size = 7), plot.margin = unit(c(0.1,0.1,0,0), "cm"), panel.border = element_rect(size = 0.3))+
    xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", linetype = "")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    scale_x_continuous(expand = c(0,0), breaks = seq(0,100,25))+
    coord_cartesian(ylim = c(0,YLIM), xlim = c(0,100))+
    scale_colour_npg(name = "", labels = c("Biomass", "Coal", "Other Cooking Fuels", "Electricity", "Transport Fuels"))+
    scale_fill_npg  (name = "", labels = c("Biomass", "Coal", "Other Cooking Fuels", "Electricity", "Transport Fuels"))+
    ggtitle(Country.Name)+
    #guides(colour = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))+
    guides(fill = fill0, colour = fill0, linetype = fill0)+
    labs(fill = "", colour = "")

  
  return(P)
  
}

P.11.BAN <- plot_engel(dec_2_BAN, "Bangladesh",  YLAB = "Expenditure Share", YLIM = 0.07, ATY = element_text(size = 6, vjust = 0.1), ATX = element_blank())
P.11.INI <- plot_engel(dec_2_INI, "India",       YLIM = 0.07, ATX = element_blank())
P.11.INO <- plot_engel(dec_2_INO, "Indonesia",   YLIM = 0.07, ATX = element_blank())
P.11.PAK <- plot_engel(dec_2_PAK, "Pakistan",    YLIM = 0.07, ATX = element_blank())
P.11.PHI <- plot_engel(dec_2_PHI, "Philippines", YLAB = "Expenditure Share", XLAB = "Expenditure Centile", YLIM = 0.07, ATY = element_text(size = 6, vjust = 0.1))
P.11.THA <- plot_engel(dec_2_THA, "Thailand",    XLAB = "Expenditure Centile", YLIM = 0.15, ATY = element_text(size = 6, vjust = 0.1))
P.11.TUR <- plot_engel(dec_2_TUR, "Turkey",      XLAB = "Expenditure Centile", YLIM = 0.07)
P.11.VIE <- plot_engel(dec_2_VIE, "Vietnam",     XLAB = "Expenditure Centile", YLIM = 0.07)

P.11 <- cowplot::align_plots(P.11.BAN, P.11.INI, P.11.INO, P.11.PAK, P.11.PHI, P.11.THA, P.11.TUR, P.11.VIE, align = "hv")
s.1 <- ggdraw(P.11[[1]])
s.2 <- ggdraw(P.11[[2]])
s.3 <- ggdraw(P.11[[3]])
s.4 <- ggdraw(P.11[[4]])
s.5 <- ggdraw(P.11[[5]])
s.6 <- ggdraw(P.11[[6]])
s.7 <- ggdraw(P.11[[7]])
s.8 <- ggdraw(P.11[[8]])

# png("Figure_4_%d.png", width = 15, height = 16, unit = "cm", res = ppi)
# 
# s.1
# s.2
# s.3
# s.4
# s.5
# s.6
# s.7
# s.8
# 
# dev.off()

# run this with line 922 uncommented and line 923 commented
L.4 <- get_legend(P.11.BAN)

Figure_4 <- ggarrange(s.1, s.2, s.3, s.4, s.5, s.7, s.8, s.6, ncol = 4, nrow = 2, common.legend = TRUE, legend.grob = L.4, legend = "bottom")

pdf("Figure_4.pdf", width = 16.51/2.54, height = 9.75/2.54)
Figure_4
dev.off()


# 7        Energy Expenditure Shares                        (Figure 5) ####

# Note:

# Data on Energy Expenditure Shares is not part of this repository (ees_BAN, ees_INI, ees_INO, ees_PAK, ees_PHI, ees_THA, ees_TUR, ees_VIE)
# Confidential Data. Available upon request and conditional on approval by responsible statistics authority.

fun_15.1 <- function(Incidence_0, Country.Name, YLIM = 0.161, fill0 = FALSE, ATY = element_blank(), ATX = element_text(size = 6, angle = 45, vjust = 0.9, hjust = 1), ATT = element_text(size = 7), XLAB = "", YLAB = ""){
  
  y <- Incidence_0 %>%
    select(hh_id, hh_size, hh_weights, Income_Group_5, type, share)%>%
    rename(Type = type, Share = share)%>%
    group_by(Income_Group_5, Type)%>%
    summarise(y5 = wtd.quantile(Share, probs = 0.05, weights = hh_weights),
              y25 = wtd.quantile(Share, probs = 0.25, weights = hh_weights),
              y50 = wtd.quantile(Share, probs = 0.5, weights = hh_weights),
              y75 = wtd.quantile(Share, probs = 0.75, weights = hh_weights),
              y95 = wtd.quantile(Share, probs = 0.95, weights = hh_weights),
              mean = wtd.mean(Share, weights = hh_weights))%>%
    ungroup()%>%
    filter(Type != "exp_Energy")%>%
    filter(Income_Group_5 == 1 | Income_Group_5 == 5)
  
  y$Type <- factor(y$Type, levels = c("exp_Electricity", "exp_Cooking", "exp_Transport", "exp_Biomass", "exp_Coal"), labels = c("Electricity", "Cooking", "Transport", "Biomass", "Coal"))
  
  
  Plot_1 <-  ggplot(data = y)+
    theme_bw()+
    geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, x = factor(Type), fill = factor(Income_Group_5)), stat = "identity", position = position_dodge(0.7), outlier.shape = NA, width = 0.5, size = 0.2) +
    stat_summary(aes(y = mean, group = interaction(Income_Group_5, Type), x = factor(Type)), fun = "mean", geom = "point", position =  position_dodge(0.7), shape = 23, size = 0.6, fill = "white", stroke = 0.2)+
    coord_cartesian(ylim = c(0.0,YLIM))+
    scale_y_continuous(label = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = seq(0,0.4, 0.05))+
    scale_colour_npg()+
    scale_fill_npg()+
    theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 7), legend.position = "bottom", strip.text = element_text(size = 7), strip.text.y = element_text(angle = 180), panel.grid.major = element_line(size = 0.3), panel.grid.minor = element_blank(), axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7), legend.title = element_text(size = 7), plot.margin = unit(c(0.1,0.1,0,0), "cm"), panel.border = element_rect(size = 0.3))+
    xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", linetype = "", fill = "")+
    guides(fill = guide_legend("Expenditure Quintile"))+
    guides(fill = fill0, colour = fill0, linetype = fill0)+
    ggtitle(Country.Name)
  
  return(Plot_1)
}

P.15.1.BAN <- fun_15.1(ees_BAN, "Bangladesh", ATX = element_blank(), ATY = element_text(size = 6, vjust = 0.1), YLAB = "Expenditure Share")
P.15.1.INI <- fun_15.1(ees_INI, "India"     , ATX = element_blank())
P.15.1.INO <- fun_15.1(ees_INO, "Indonesia" , ATX = element_blank())
P.15.1.PAK <- fun_15.1(ees_PAK, "Pakistan"  , ATX = element_blank())

P.15.1.PHI <- fun_15.1(ees_PHI, "Philippines", ATY = element_text(size = 6, vjust = 0.1), YLAB = "Expenditure Share")
P.15.1.THA <- fun_15.1(ees_THA, "Thailand", YLIM = 0.4, ATY = element_text(size = 6, vjust = 0.1), YLAB = "Expenditure Share")
P.15.1.TUR <- fun_15.1(ees_TUR, "Turkey")
P.15.1.VIE <- fun_15.1(ees_VIE, "Vietnam")

P.15.1 <- cowplot::align_plots(P.15.1.BAN, P.15.1.INI, P.15.1.INO, P.15.1.PAK, P.15.1.PHI, P.15.1.THA, P.15.1.TUR, P.15.1.VIE, align = "hv")
s.1 <- ggdraw(P.15.1[[1]])
s.2 <- ggdraw(P.15.1[[2]])
s.3 <- ggdraw(P.15.1[[3]])
s.4 <- ggdraw(P.15.1[[4]])
s.5 <- ggdraw(P.15.1[[5]])
s.6 <- ggdraw(P.15.1[[6]])
s.7 <- ggdraw(P.15.1[[7]])
s.8 <- ggdraw(P.15.1[[8]])

# run this with line 1011 uncommented and line 1012 commented 
L.5 <- get_legend(P.15.1.BAN)

Figure_5 <- ggarrange(s.1, s.2, s.3, s.4, s.5, s.7, s.8, s.6, ncol = 4, nrow = 2, common.legend = TRUE, legend.grob = L.5, legend = "bottom")

pdf("Figures/Figure_5.pdf", width = 16.51/2.54, height = 10.75/2.54)
Figure_5
dev.off()


# 8        Distribution incl Including Lump Sum Transfers   (Figure 6) ####

calculate_median <- function(x){
  x <- x %>%
    group_by(Income_Group_5, Type)%>%
    arrange(value)%>%
    mutate(cumsum_shares = cumsum(share))%>%
    filter(cumsum_shares >= 0.5)%>%
    slice(which.min(cumsum_shares))%>%
    ungroup()%>%
    rename(median = value)%>%
    select(Income_Group_5, Type, median)
}

calculate_median_y <- function(x0, xmedian, adjust_0){
  ggplot_build(ggplot(x0, aes(y = share, x = value, group = interaction(factor(Income_Group_5), Type)))+
                 geom_smooth(method = "loess", span = adjust_0, se = FALSE, n = 700))$data[[1]]%>%
    select(x,y,group)%>%
    mutate(group = ifelse(group == 1, 1, 5))%>%
    left_join(xmedian, by = c("group" = "Income_Group_5"))%>%
    mutate(help = median - x)%>%
    mutate(help_0 = ifelse(help <0, help*-1, help))%>%
    group_by(group)%>%
    filter(help_0 == min(help_0))%>%
    ungroup()%>%
    rename(Income_Group_5 = group, median.x = x, median.y = y)%>%
    select(-median, -help, -help_0)%>%
    select(median.x, median.y, Income_Group_5)
}

plotting_ten.1 <- function(Incidence.X, Country.Name, fill0 = FALSE, ATY = element_blank(), ATX = element_text(size = 6), ATT = element_text(size = 7), XLAB = "", YLAB = "", YLIM = 0.01, adjust_1 = 0.2){
  adjust_0 <- adjust_1
  
  # Round Values up, calculate households per bins
  Incidence.X0 <- Incidence.X %>%
    select(hh_id, hh_weights, Income_Group_5, burden_CO2_within_per_capita, burden_CO2_within_pc_LST_pc)%>%
    pivot_longer(starts_with("burden"), names_to = "type", values_to = "value")%>%
    filter(Income_Group_5 == 1 | Income_Group_5 == 5)%>%
    filter(!is.na(value) & value != "-Inf" & value > -0.5 & value != "Inf" & value < 0.5)%>%
    #mutate(value = value*(-1))%>%
    mutate(Type = ifelse(type == "burden_CO2_within_per_capita", "National Carbon Price", "National Carbon Price and equal per capita transfer"),
           value = round(value,3))%>%
    group_by(Income_Group_5, Type, value)%>%
    summarise(weights = sum(hh_weights))%>%
    ungroup()
  
  # Calculate total households
  IG_weights <- Incidence.X %>%
    select(hh_id, Income_Group_5, hh_weights)%>%
    group_by(Income_Group_5)%>%
    summarise(IG_weights = sum(hh_weights))%>%
    ungroup()
  # Calculate Shares
  Incidence.X1 <- left_join(Incidence.X0, IG_weights)%>%
    mutate(share = weights/IG_weights)%>%
    filter(Type == "National Carbon Price")
  Incidence.X2 <- left_join(Incidence.X0, IG_weights)%>%
    mutate(share = weights/IG_weights)%>%
    filter(Type != "National Carbon Price")
  
  #Calculate Median
  Median.X1 <- calculate_median(Incidence.X1)%>%
    filter(Type == "National Carbon Price")
  Median.X2 <- calculate_median(Incidence.X2)%>%
    filter(Type != "National Carbon Price")
  
  # Calculate Median Y
  t.X.1 <- calculate_median_y(Incidence.X1, Median.X1, adjust_0)
  t.X.2 <- calculate_median_y(Incidence.X2, Median.X2, adjust_0)
  
  Incidence.X1.1 <- Incidence.X1 %>%
    left_join(t.X.1, by = c("Income_Group_5"))
  Incidence.X2.1 <- Incidence.X2 %>%
    left_join(t.X.2, by = "Income_Group_5")
  Incidence.X3.1 <- bind_rows(Incidence.X1.1, Incidence.X2.1)%>%
    mutate(Label = ifelse(Income_Group_5 == 1 & Type == "National Carbon Price", "National Carbon Price (Expenditure Quintile 1)",
                          ifelse(Income_Group_5 == "5" & Type == "National Carbon Price", "National Carbon Price (Expenditure Quintile 5)",
                                 ifelse(Income_Group_5 == "1" & Type == "National Carbon Price and equal per capita transfer", "National Carbon Price and equal per capita Transfer (Expenditure Quintile 1)",
                                        "National Carbon Price and equal per capita Transfer (Expenditure Quintile 5)"))))
  
  P_3 <- ggplot(Incidence.X3.1, aes(colour = Label, linetype = Label))+
    theme_bw()+
    geom_vline(aes(xintercept = 0), size = 0.3)+
    theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 7), legend.position = "bottom", strip.text = element_text(size = 7), strip.text.y = element_text(angle = 180), panel.grid.major = element_line(size = 0.3), panel.grid.minor = element_blank(), axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7), legend.title = element_text(size = 7), plot.margin = unit(c(0.1,0.1,0,0), "cm"), panel.border = element_rect(size = 0.3))+
    geom_smooth(aes(x = value, y = share, group = Label), size = 0.3, method = "loess", n = 500, span = adjust_0, se = FALSE, fullrange = TRUE)+
    geom_point(aes(x = median.x, y = median.y, group = Label, fill = Label), shape = 21, size = 1.3, colour = "black", stroke = 0.2)+
    xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", linetype = "", fill = "")+
    scale_y_continuous(breaks = c(0,0.05,0.1), expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
    scale_x_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1), breaks = seq(-0.1,0.15, 0.05))+
    coord_cartesian(xlim = c(-0.085,0.165), ylim = c(0,0.1))+
    #geom_segment(aes(x = median, xend = median, y = 0, yend = 100, colour = factor(Income_Group_5), linetype = factor(Income_Group_5)), size = 1)+
    scale_colour_manual(  values = c("#BC3C29FF", "#0072B5FF", "#BC3C29FF", "#0072B5FF"))+
    scale_fill_manual(    values = c("#BC3C29FF", "#0072B5FF", "#BC3C29FF", "#0072B5FF"))+
    scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"))+
    ggtitle(Country.Name)+
    #guides(fill = guide_legend("", nrow = 4), colour = guide_legend("", nrow = 4), linetype = guide_legend("", nrow = 4), direction = "vertical")
    guides(fill = fill0, colour = fill0, linetype = fill0)
  
  return(P_3)
}

P.10.Ba.3 <- plotting_ten.1(Incidence.Bangladesh.2.2.1,  adjust_1 =  0.2,  "Bangladesh",  ATY = element_text(size = 6, vjust = 0.1),  ATX = element_blank(),         ATT = element_text(size = 7),          XLAB = "", YLAB = "Share of Households per Quintile")
P.10.Ii.3 <- plotting_ten.1(Incidence.India.2.2.1,       adjust_1 =  0.1,  "India",       ATY = element_blank(),                      ATX = element_blank(),         ATT = element_blank(),          XLAB = "")
P.10.Io.3 <- plotting_ten.1(Incidence.Indonesia.2.2.1,   adjust_1 =  0.1,  "Indonesia",   ATY = element_blank(),                      ATX = element_blank(),         ATT = element_blank(),          XLAB = "")
P.10.Pa.3 <- plotting_ten.1(Incidence.Pakistan.2.2.1,    adjust_1 =  0.1,  "Pakistan",    ATY = element_blank(),                      ATX = element_blank() ,        ATT = element_blank(),          XLAB = "")
P.10.Ph.3 <- plotting_ten.1(Incidence.Philippines.2.2.1, adjust_1 =  0.35, "Philippines", ATY = element_text(size = 6, vjust = 0.1),  ATX = element_text(size = 6), ATT = element_text(size = 7),  XLAB = "Household Budget Change", YLAB = "Share of Households per Quintile")
P.10.Th.3 <- plotting_ten.1(Incidence.Thailand.2.2.1,    adjust_1 =  0.15, "Thailand",    ATY = element_blank(),                      ATX = element_text(size = 6), ATT = element_text(size = 7),  XLAB = "Household Budget Change")
P.10.Tu.3 <- plotting_ten.1(Incidence.Turkey.2.2.1,      adjust_1 =  0.12, "Turkey",      ATY = element_blank(),                      ATX = element_text(size = 6), ATT = element_text(size = 7),  XLAB = "Household Budget Change")
P.10.Vi.3 <- plotting_ten.1(Incidence.Vietnam.2.2.1,                       "Vietnam",     ATY = element_blank(),                      ATX = element_text(size = 6), ATT = element_text(size = 7),  XLAB = "Household Budget Change")

P.10 <- cowplot::align_plots(P.10.Ba.3, P.10.Ii.3, P.10.Io.3, P.10.Pa.3, P.10.Ph.3, P.10.Th.3, P.10.Tu.3, P.10.Vi.3, align = "hv")
s.1 <- ggdraw(P.10[[1]])
s.2 <- ggdraw(P.10[[2]])
s.3 <- ggdraw(P.10[[3]])
s.4 <- ggdraw(P.10[[4]])
s.5 <- ggdraw(P.10[[5]])
s.6 <- ggdraw(P.10[[6]])
s.7 <- ggdraw(P.10[[7]])
s.8 <- ggdraw(P.10[[8]])

# run this with line 1070 uncommented and line 1071 commented
L.7 <- get_legend(P.10.Ba.3)

Figure_6 <- ggarrange(s.1, s.2, s.3, s.4, s.5, s.6, s.7, s.8, ncol = 4, nrow = 2, common.legend = TRUE, legend.grob = L.7, legend = "bottom")

pdf("Figures/Figure_6.pdf", width = 16.51/2.54, height = 11.75/2.54)
Figure_6
dev.off()




# MAIN TABLES ####

# Table 1 is purely descriptive

# 9        Vertical vs. Horizontal Effects                  (Table 2 (Table S12, Table S13, Table S14)) ####

vertical_horizontal <- function(Incidence_Country, Country.Name){
  
  Incidence.X <- Incidence_Country %>%
    select(hh_id, hh_weights, burden_CO2_within_per_capita, Income_Group_5)%>%
    group_by(Income_Group_5)%>%
    summarise(y1  = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.01),  
              y5  = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.05),
              y20 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.20),
              y50 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.50),
              y80 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.80),
              y95 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.95),
              y99 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.99),
              sd =  sqrt(wtd.var(burden_CO2_within_per_capita,      weights = hh_weights)),
              mean = wtd.mean(burden_CO2_within_per_capita,    weights = hh_weights))%>%
    ungroup()
  
  Incidence.1.1 <- Incidence.X %>%
    select(Income_Group_5, y5, y95)%>%
    mutate(dif = y95-y5)%>%
    select(-y5, - y95)%>%
    spread(key = Income_Group_5, value = dif)%>%
    mutate(Country = Country.Name)%>%
    select(Country, everything())
  
  Incidence.1.2 <- Incidence.X %>%
    select(Income_Group_5, y20, y80)%>%
    mutate(dif = y80-y20)%>%
    select(-y20, - y80)%>%
    spread(key = Income_Group_5, value = dif)%>%
    mutate(Country = Country.Name)%>%
    select(Country, everything())
  
  Incidence.1.3 <- Incidence.X %>%
    select(Income_Group_5, y1, y99)%>%
    mutate(dif = y99-y1)%>%
    select(-y1, - y99)%>%
    spread(key = Income_Group_5, value = dif)%>%
    mutate(Country = Country.Name)%>%
    select(Country, everything())
  
  Incidence.1.4 <- Incidence.X %>%
    select(Income_Group_5, sd)%>%
    spread(key = Income_Group_5, value = sd)%>%
    mutate(Country = Country.Name)%>%
    select(Country, everything())
  
  Incidence.2 <- Incidence.X %>%
    select(Income_Group_5, y50)%>%
    mutate(fact = ifelse(y50 == max(y50),1,
                         ifelse(y50 == min(y50),2,0)))%>%
    filter(fact != 0)
  
  Incidence.2.1 <- Incidence.2 %>%
    filter(fact == 1)%>%
    rename(Quintile_Max = Income_Group_5, max = y50)%>%
    select(-fact)%>%
    mutate(Country = Country.Name)
  
  Incidence.2.2 <- Incidence.2 %>%
    filter(fact == 2)%>%
    rename(Quintile_Min = Income_Group_5, min = y50)%>%
    select(-fact)%>%
    mutate(Country = Country.Name)
  
  Incidence.2.0 <- left_join(Incidence.2.1, Incidence.2.2, by = "Country")%>%
    select(Country, Quintile_Min, Quintile_Max, min, max)%>%
    mutate(Difference = max - min)%>%
    select(-max, -min)
  
  Incidence.3 <- Incidence.X %>%
    select(Income_Group_5, mean)%>%
    mutate(fact = ifelse(mean == max(mean),1,
                         ifelse(mean == min(mean),2,0)))%>%
    filter(fact != 0)
  
  Incidence.3.1 <- Incidence.3 %>%
    filter(fact == 1)%>%
    rename(Quintile_Max = Income_Group_5, max = mean)%>%
    select(-fact)%>%
    mutate(Country = Country.Name)
  
  Incidence.3.2 <- Incidence.3 %>%
    filter(fact == 2)%>%
    rename(Quintile_Min = Income_Group_5, min = mean)%>%
    select(-fact)%>%
    mutate(Country = Country.Name)
  
  Incidence.3.0 <- left_join(Incidence.3.1, Incidence.3.2, by = "Country")%>%
    select(Country, Quintile_Min, Quintile_Max, min, max)%>%
    mutate(Difference = max - min)%>%
    select(-max, -min)
  
  
  Incidence_0.1 <- left_join(Incidence.2.0, Incidence.1.1, by = "Country")
  Incidence_0.2 <- left_join(Incidence.2.0, Incidence.1.2, by = "Country")
  Incidence_0.3 <- left_join(Incidence.2.0, Incidence.1.3, by = "Country")
  Incidence_0.4 <- left_join(Incidence.3.0, Incidence.1.4, by = "Country")
  
  Incidence_0 <- list("5_95" = Incidence_0.1, "20_80" = Incidence_0.2,
                      "1_99" = Incidence_0.3, "sd"   = Incidence_0.4)
  
  return(Incidence_0)
}

vh_BAN_0 <- vertical_horizontal(Incidence.Bangladesh, "Bangladesh"  )
vh_INI_0 <- vertical_horizontal(Incidence.India,      "India"       )
vh_INO_0 <- vertical_horizontal(Incidence.Indonesia,  "Indonesia"   )
vh_PAK_0 <- vertical_horizontal(Incidence.Pakistan,   "Pakistan"    )
vh_PHI_0 <- vertical_horizontal(Incidence.Philippines, "Philippines")
vh_THA_0 <- vertical_horizontal(Incidence.Thailand,   "Thailand"    )
vh_TUR_0 <- vertical_horizontal(Incidence.Turkey,     "Turkey"      )
vh_VIE_0 <- vertical_horizontal(Incidence.Vietnam,    "Vietnam"     )

vh_BAN <- vh_BAN_0$'5_95'
vh_INI <- vh_INI_0$'5_95'
vh_INO <- vh_INO_0$'5_95'
vh_PAK <- vh_PAK_0$'5_95'
vh_PHI <- vh_PHI_0$'5_95'
vh_THA <- vh_THA_0$'5_95'
vh_TUR <- vh_TUR_0$'5_95'
vh_VIE <- vh_VIE_0$'5_95'

vh_total_5 <- rbind(vh_BAN, vh_INI, vh_INO, vh_PAK, vh_PHI, vh_THA, vh_TUR, vh_VIE)

vh_BAN <- vh_BAN_0$'1_99'
vh_INI <- vh_INI_0$'1_99'
vh_INO <- vh_INO_0$'1_99'
vh_PAK <- vh_PAK_0$'1_99'
vh_PHI <- vh_PHI_0$'1_99'
vh_THA <- vh_THA_0$'1_99'
vh_TUR <- vh_TUR_0$'1_99'
vh_VIE <- vh_VIE_0$'1_99'

vh_total_1 <- rbind(vh_BAN, vh_INI, vh_INO, vh_PAK, vh_PHI, vh_THA, vh_TUR, vh_VIE)

vh_BAN <- vh_BAN_0$'20_80'
vh_INI <- vh_INI_0$'20_80'
vh_INO <- vh_INO_0$'20_80'
vh_PAK <- vh_PAK_0$'20_80'
vh_PHI <- vh_PHI_0$'20_80'
vh_THA <- vh_THA_0$'20_80'
vh_TUR <- vh_TUR_0$'20_80'
vh_VIE <- vh_VIE_0$'20_80'

vh_total_20 <- rbind(vh_BAN, vh_INI, vh_INO, vh_PAK, vh_PHI, vh_THA, vh_TUR, vh_VIE)

vh_BAN <- vh_BAN_0$'sd'
vh_INI <- vh_INI_0$'sd'
vh_INO <- vh_INO_0$'sd'
vh_PAK <- vh_PAK_0$'sd'
vh_PHI <- vh_PHI_0$'sd'
vh_THA <- vh_THA_0$'sd'
vh_TUR <- vh_TUR_0$'sd'
vh_VIE <- vh_VIE_0$'sd'

vh_total_var <- rbind(vh_BAN, vh_INI, vh_INO, vh_PAK, vh_PHI, vh_THA, vh_TUR, vh_VIE)

vh_total <- list(vh_total_5, vh_total_1, vh_total_20, vh_total_var)
write.xlsx(vh_total, "Figures/Table_2_Vertical_horizontal_equity.xlsx", append = TRUE)

# SUPPLEMENTARY FIGURES ####
# 10       Coal Pipeline                                    (Figure S1) ####

# cp <- 
# DOWNLOAD FROM https://docs.google.com/spreadsheets/d/1dxEObsymdexLIsUM0bSm2SGibNr_GIPT51nu_f7kXxc/edit#gid=0

cp_0 <- cp %>%
  select(Country, 'Announced.+.Pre-permit.+.Permitted', 'Construction', 'Operating')%>%
  rename(announced_permitted = 'Announced.+.Pre-permit.+.Permitted')%>%
  filter(Country == "China" | Country == "India" | Country == "Total" | Country == "Indonesia" | Country == "Pakistan" |
           Country == "Bangladesh" | Country == "Vietnam" | Country == "Thailand" | Country == "Turkey" | Country == "Philippines")

cp_1 <- cp_0 %>%
  filter(Country != "Total" & Country != "China")%>%
  mutate(Country = reorder(Country, -Operating))%>%
  pivot_longer(-Country, "Type", values_to = "MW")%>%
  mutate(Type = factor(Type, levels = c("Operating", "Construction", "announced_permitted")))%>%
  mutate(GW = MW/1000)

plot_13 <- function(Country.Name, YLIM, XLIM, YLAB = "", XLAB = "", ATX = element_blank(), ATY = element_text(size = 20, vjust = 0.1), ATT = element_text(size = 25), fill0 = FALSE){
  
  cp_2 <- cp_1 %>%
    filter(Country == Country.Name)
  
  P <- ggplot(cp_2)+
    geom_col(aes(fill = Type, x = Type, y = GW), colour = "black", width = 0.75)+
    scale_fill_npg(name = "", labels = c("Operating", "Construction", "Announced / Permitted"))+
    xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", fill = "")+
    theme_bw()+
    theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 35), panel.border = element_rect(size = 1.5), panel.grid.major = element_line(size = 1), strip.text = element_text(size = 25), strip.text.y = element_text(angle = 180))+
    coord_cartesian(ylim = c(0,YLIM+ 2))+
    scale_y_continuous(expand = c(0,0), breaks = c(0,signif(YLIM, 0), signif(YLIM, 0)/2))+
    #guides(colour = fill0, fill = guide_legend(ncol = 1))+
    guides(fill = fill0, colour = fill0)+
    ggtitle(Country.Name)
  
  return(P)
}

P.13.BAN <- plot_13("Bangladesh", YLIM = 20, YLAB = "Capacity in GW")
P.13.INI <- plot_13("India",      YLIM = 250, YLAB = "Capacity in GW")
P.13.INO <- plot_13("Indonesia",  YLIM = 40, YLAB = "Capacity in GW")
P.13.PAK <- plot_13("Pakistan",   YLIM = 20, ATY = element_blank())
P.13.PHI <- plot_13("Philippines",YLIM = 20, ATY = element_blank())
P.13.THA <- plot_13("Thailand",   YLIM = 10, YLAB = "Capacity in GW")
P.13.TUR <- plot_13("Turkey",     YLIM = 40, ATY = element_blank())
P.13.VIE <- plot_13("Vietnam",    YLIM = 40, ATY = element_blank())

P.13 <- cowplot::align_plots(P.13.BAN, P.13.INI, P.13.INO, P.13.PAK, P.13.PHI, P.13.THA, P.13.TUR, P.13.VIE, align = "hv")
s.1 <- ggdraw(P.13[[1]])
s.2 <- ggdraw(P.13[[2]])
s.3 <- ggdraw(P.13[[3]])
s.4 <- ggdraw(P.13[[4]])
s.5 <- ggdraw(P.13[[5]])
s.6 <- ggdraw(P.13[[6]])
s.7 <- ggdraw(P.13[[7]])
s.8 <- ggdraw(P.13[[8]])


png("Coal Pipeline/Figure_S1_%d.png", width = 15, height = 15, unit = "cm", res = ppi)

s.1
s.2
s.3
s.4
s.5
s.6
s.7
s.8

dev.off()

# L.1 <- ggdraw(get_legend(P.13.BAN))
# 
# png("Coal Pipeline/Legend.png", width = 4*ppi, height = 4*ppi, res = ppi)
# 
# L.1
# 
# dev.off()

cp_2 <- cp %>%
  select(Country, 'Announced.+.Pre-permit.+.Permitted', 'Construction', 'Operating')%>%
  rename(announced_permitted = 'Announced.+.Pre-permit.+.Permitted')%>%
  select(-Operating)%>%
  mutate(Merge = announced_permitted + Construction)%>%
  filter(Merge != 0)%>%
  arrange(Merge)

cp_3 <- cp %>%
  select(Country, Operating)%>%
  filter(Operating != 0)%>%
  arrange(Operating)

# write.xlsx(cp_2, "Coal Pipeline/Summary_1.xlsx")
# write.xlsx(cp_3, "Coal Pipeline/Operating.xlsx")

# 11       Analysis on Shares on Electricity                (Figure S2) ####

ely_ban <- read_csv("Aggregated_Data/Electricity/electricity_share_analysis_BAN.csv")
ely_ini <- read_csv("Aggregated_Data/Electricity/electricity_share_analysis_INI.csv")
ely_ino <- read_csv("Aggregated_Data/Electricity/electricity_share_analysis_INO.csv")
ely_pak <- read_csv("Aggregated_Data/Electricity/electricity_share_analysis_PAK.csv")
ely_phi <- read_csv("Aggregated_Data/Electricity/electricity_share_analysis_PHI.csv")
ely_tha <- read_csv("Aggregated_Data/Electricity/electricity_share_analysis_THA.csv")
ely_tur <- read_csv("Aggregated_Data/Electricity/electricity_share_analysis_TUR.csv")
ely_vie <- read_csv("Aggregated_Data/Electricity/electricity_share_analysis_VIE.csv")

pre_ely_analysis_1 <- function(ely, Country.Name){
  ely_1 <- ely %>%
    select(hh_id, hh_size, hh_weights, Urban, Income_Group_5, share_ely)
  ely_2 <- ely_1 %>%
    mutate(Urban = "National")
  ely_3 <- rbind(ely_1, ely_2)%>%
    mutate(country = Country.Name)%>%
    select(-Income_Group_5)
}

ely_ban_1 <- pre_ely_analysis_1(ely_ban, "Bangladesh")
ely_ini_1 <- pre_ely_analysis_1(ely_ini, "India")
ely_ino_1 <- pre_ely_analysis_1(ely_ino, "Indonesia")
ely_pak_1 <- pre_ely_analysis_1(ely_pak, "Pakistan")
ely_phi_1 <- pre_ely_analysis_1(ely_phi, "Philippines")
ely_tha_1 <- pre_ely_analysis_1(ely_tha, "Thailand")
ely_tur_1 <- pre_ely_analysis_1(ely_tur, "Turkey")
ely_vie_1 <- pre_ely_analysis_1(ely_vie, "Vietnam")

ely_all_1 <- rbind(ely_ban_1, ely_ini_1, ely_ino_1, ely_pak_1, ely_phi_1, ely_tha_1, ely_tur_1, ely_vie_1)%>%
  mutate(Urban = ifelse(Urban == "urban", "Urban", ifelse(Urban == "rural", "Rural", Urban)))%>%
  group_by(country, Urban)%>%
  summarise(
    y5  = wtd.quantile(share_ely, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(share_ely, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(share_ely, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(share_ely, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(share_ely, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   share_ely, weights = hh_weights))%>%
  ungroup()

P.2 <- ggplot(ely_all_1, aes(x = Urban, fill = Urban))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5) +
  geom_point(aes(y = mean), shape = 23, size = 1.5, fill = "white")+
  theme_bw()+
  facet_wrap(~ country, nrow = 1, strip.position = "bottom")+
  coord_cartesian(ylim = c(0,0.17))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  ylab("Share of Electricity Expenditures over all Expenditures")+
  scale_fill_npg()+xlab("")+guides(fill =guide_legend(title = NULL))+
  theme(legend.background = element_rect(fill = "white", colour = "black"),legend.position = c(0.2, 0.8), panel.border = element_rect(size = 1), legend.text = element_text(size = 12), strip.text = element_text(size = 9), strip.background = element_rect(colour = "black", size = 1), axis.text.y = element_text(size = 9), axis.text.x = element_blank(), axis.ticks = element_blank(), axis.title.y = element_text(size = 10))

png("Figure_S2_%d.png", width = 22, height = 12, unit = "cm", res = ppi)

P.2

dev.off()  

# 12       Non-parametric Engel-Curves on expenditure types (Figure S3) ####

transform_0 <- read.xlsx("Items_Categories_Concordance.xlsx")%>%
  select(-Explanation)

transform_112 <- function(x){
  y <- x %>%
    select(hh_id, hh_weights, hh_size, hh_tot_inc_USD, urban, starts_with("exp_USD"))%>%
    replace(is.na(.), 0) %>% 
    rename_at(vars(starts_with("exp_")), list(~ str_replace(., "exp_USD_", "")))%>%
    pivot_longer(!(hh_id:urban), names_to = "GTAP", values_to = "expenditures")%>%
    left_join(transform_0, by = "GTAP")%>%
    filter(Type != "Deleted" & Type != "Other")%>%
    mutate(hh_expenditures_0_pc = hh_tot_inc_USD)%>%
    mutate(Income_Group_5   = as.numeric(binning(hh_expenditures_0_pc, bins=5  , method = c("wtd.quantile"), labels = seq(1,5,length.out = 5),  weights = hh_weights)))%>%
    mutate(Income_Group_100 = as.numeric(binning(hh_expenditures_0_pc, bins=100, method = c("wtd.quantile"), labels = seq(1,100,length.out=100), weights = hh_weights)))
  
  y1 <- y %>%
    select(hh_id, hh_weights, hh_size, hh_tot_inc_USD, Income_Group_100)
  
  y2 <- y %>%
    select(hh_id, Type, expenditures, hh_tot_inc_USD)%>%
    group_by(hh_id, Type)%>%
    summarise(expenditures = sum(expenditures),
              hh_tot_inc_USD = first(hh_tot_inc_USD))%>%
    ungroup()%>%
    mutate(Share = expenditures/hh_tot_inc_USD)%>%
    select(-hh_tot_inc_USD)
  
  
  y3 <- left_join(y2, y1, by = "hh_id")%>%
    group_by(Income_Group_100, Type)%>%
    summarise(Share           = weighted.mean(Share, hh_weights),
              mean_hh_inc_USD = weighted.mean(hh_tot_inc_USD, hh_weights),
              hh_weights      = sum(hh_weights))%>%
    ungroup()
  
  return(y3)
  
}

dec_11.2_BAN <- transform_112(dec_1_BAN)
dec_11.2_INI <- transform_112(dec_1_INI)
dec_11.2_INO <- transform_112(dec_1_INO)
dec_11.2_PAK <- transform_112(dec_1_PAK)
dec_11.2_PHI <- transform_112(dec_1_PHI)
dec_11.2_THA <- transform_112(dec_1_THA)
dec_11.2_TUR <- transform_112(dec_1_TUR)
dec_11.2_VIE <- transform_112(dec_1_VIE)


plot_engel <- function(data, Country.Name, fill0 = FALSE, ATY = element_blank(), ATX = element_text(size = 20, hjust = 1), ATT = element_text(size = 25), XLAB = "", YLAB = "", YLIM = 100){
  
  P <- ggplot(data)+
    geom_smooth(formula = as.formula (y ~ x), aes(x = Income_Group_100, y = Share, weight = hh_weights, colour = factor(Type), fill = factor(Type)), size = 1, method = "loess", se = TRUE, fullrange = TRUE, span = 0.75)+
    theme_bw()+
    theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 35), legend.position = "bottom", panel.border = element_rect(size = 1.5), panel.grid.major = element_line(size = 1), strip.text = element_text(size = 25), strip.text.y = element_text(angle = 180))+
    xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", linetype = "")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = seq(0,YLIM, YLIM/3))+
    scale_x_continuous(expand = c(0,0), breaks = seq(0,100,25))+
    coord_cartesian(ylim = c(0,YLIM), xlim = c(0,100))+
    scale_colour_npg(name = "", labels = c("Energy", "Food", "Goods", "Services"))+
    scale_fill_npg  (name = "", labels = c("Energy", "Food", "Goods", "Services"))+
    ggtitle(Country.Name)+
    #guides(colour = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))+
    #labs(fill = "", colour = "")
    guides(fill = fill0, colour = fill0, linetype = fill0)
  
  return(P)
  
}

P.112.BAN <- plot_engel(dec_11.2_BAN, "Bangladesh",  YLAB = "Expenditure Share", YLIM = 0.6, ATY = element_text(size = 20, vjust = 0.1), ATX = element_blank())
P.112.INI <- plot_engel(dec_11.2_INI, "India",       YLIM = 0.6, ATX = element_blank())
P.112.INO <- plot_engel(dec_11.2_INO, "Indonesia",   YLIM = 0.6, ATX = element_blank())
P.112.PAK <- plot_engel(dec_11.2_PAK, "Pakistan",    YLIM = 0.6, ATX = element_blank())
P.112.PHI <- plot_engel(dec_11.2_PHI, "Philippines", YLAB = "Expenditure Share", XLAB = "Expenditure Centile", YLIM = 0.6, ATY = element_text(size = 20, vjust = 0.1))
P.112.THA <- plot_engel(dec_11.2_THA, "Thailand",    XLAB = "Expenditure Centile", YLIM = 0.6, ATY = element_text(size = 20, vjust = 0.1))
P.112.TUR <- plot_engel(dec_11.2_TUR, "Turkey",      XLAB = "Expenditure Centile", YLIM = 0.6)
P.112.VIE <- plot_engel(dec_11.2_VIE, "Vietnam",     XLAB = "Expenditure Centile", YLIM = 0.6)

P.11 <- cowplot::align_plots(P.112.BAN, P.112.INI, P.112.INO, P.112.PAK, P.112.PHI, P.112.THA, P.112.TUR, P.112.VIE, align = "hv")
s.1 <- ggdraw(P.11[[1]])
s.2 <- ggdraw(P.11[[2]])
s.3 <- ggdraw(P.11[[3]])
s.4 <- ggdraw(P.11[[4]])
s.5 <- ggdraw(P.11[[5]])
s.6 <- ggdraw(P.11[[6]])
s.7 <- ggdraw(P.11[[7]])
s.8 <- ggdraw(P.11[[8]])

png("Figure_S3_%d.png", width = 15, height = 16, unit = "cm", res = ppi)

s.1
s.2
s.3
s.4
s.5
s.6
s.7
s.8

dev.off()

# L.1 <- ggdraw(get_legend(P.112.BAN))
# 
# png("Legend_Engel_real_%d.png", width = 5*ppi, height = 2*ppi, res = ppi)
# 
# L.1
# 
# dev.off()

# 13       Analysis on included Emissions                   (Figure S4) ####


pre_analyse_1 <- function(Country.Name){
  
  COUNTRY.NAME <- toupper(Country.Name)
  country.name <- tolower(Country.Name)
  
  matching <- read.xlsx("Items_GTAP_Concordance.xlsx", sprintf("MATCHING_%s", COUNTRY.NAME))
  colnames(matching) <- c("GTAP", "Explanation", seq(1:(ncol(matching)-2)))
  matching_1 <- matching %>%
    select(GTAP, "1")%>%
    rename(items = "1")%>%
    mutate(items_covered = ifelse(is.na(items), 0, 1))%>%
    filter(GTAP != "deleted" & GTAP != "other")
  
  GTAP.10         <- read.csv2("GTAP/GTAP10.csv")%>%
    select(-Number, -Explanation)
  
  # Needs to be unzipped first
  
  Electricity.10  <- read.csv(sprintf("GTAP/all_results_embeded_GTAP_10_2014/%s/%s_embeded_electricity_dollar_gtap10_2014.csv", Country.Name, country.name),                   header = FALSE, col.names = "Electricity_MUSD")
  Emissions.10    <- read.csv(sprintf("GTAP/all_results_embeded_GTAP_10_2014/%s/total_CO2_emissions_hh_%s_gtap10_2014.csv", Country.Name, country.name),                       header = FALSE, col.names = "CO2_Mt")
  Emissions.in.10 <- read.csv(sprintf("GTAP/all_results_embeded_GTAP_10_2014/%s/total_CO2_emissions_hh_%s_from_%s_gtap10_2014.csv", Country.Name, country.name, country.name), header = FALSE, col.names = "CO2_Mt_from_within")
  Virtual         <- data.frame(GTAP.10, Emissions.10, Emissions.in.10, Electricity.10)
  
  Analysis_1 <- left_join(matching_1, Virtual, by = "GTAP")%>%
    select(-items)%>%
    mutate(CO2_Mt_covered           = items_covered*CO2_Mt)%>%
    mutate(CO2_within_Mt_covered    = items_covered*CO2_Mt_from_within)%>%
    mutate(Electricity_MUSD_covered = items_covered*Electricity_MUSD)
  
  Analysis_1.1 <- Analysis_1 %>%
    filter(items_covered == 0)%>%
    mutate(sum_CO2 = sum(CO2_Mt))%>%
    mutate(sum_CO2w = sum(CO2_Mt_from_within))%>%
    select(-CO2_Mt_covered, -CO2_within_Mt_covered, - Electricity_MUSD_covered, - Electricity_MUSD)%>%
    mutate(share_CO2 = CO2_Mt/sum_CO2)%>%
    mutate(share_CO2w = CO2_Mt_from_within/sum_CO2w)%>%
    arrange(share_CO2)
  
  Analysis_2 <- Analysis_1 %>%
    summarise(
      CO2_Mt                   = sum(CO2_Mt),
      CO2_Mt_from_within       = sum(CO2_Mt_from_within),
      Electricity_MUSD         = sum(Electricity_MUSD),
      
      CO2_Mt_covered           = sum(CO2_Mt_covered),
      CO2_within_Mt_covered    = sum(CO2_within_Mt_covered),
      Electricity_MUSD_covered = sum(Electricity_MUSD_covered)
    )%>%
    mutate(CO2_not_covered = CO2_Mt - CO2_Mt_covered)%>%
    mutate(CO2_within_not_covered = CO2_Mt_from_within - CO2_within_Mt_covered)%>%
    mutate(Electricity_not_covered = Electricity_MUSD - Electricity_MUSD_covered)
  
  Analysis_3 <- Analysis_2 %>%
    gather(key = "variable", value = "value")%>%
    mutate(measure = ifelse(variable == "CO2_Mt" | variable == "CO2_Mt_covered" | variable == "CO2_not_covered", "CO2", ifelse(variable == "Electricity_MUSD" | variable == "Electricity_not_covered" | variable == "Electricity_MUSD_covered", "Electricity", "CO2 within")))%>%
    mutate(type    = ifelse(variable == "CO2_Mt" | variable == "CO2_Mt_from_within" | variable == "Electricity_MUSD", "total", ifelse(variable == "CO2_Mt_covered" | variable == "CO2_within_Mt_covered" | variable == "Electricity_MUSD_covered", "covered", "not_covered")))%>%
    mutate(country = Country.Name)
  
  return(Analysis_3)
  # return(Analysis_1.1) for Information on which sector drives the share
}

pre_ban <- pre_analyse_1("Bangladesh")
pre_ini <- pre_analyse_1("India")
pre_ino <- pre_analyse_1("Indonesia")
pre_pak <- pre_analyse_1("Pakistan")
pre_phi <- pre_analyse_1("Philippines")
pre_tha <- pre_analyse_1("Thailand")
pre_tur <- pre_analyse_1("Turkey")
pre_vie <- pre_analyse_1("Vietnam")

jointly <- rbind(pre_ban, pre_ini, pre_ino, pre_phi, pre_pak, pre_tha, pre_tur, pre_vie)%>%
  filter(type != "total")%>%
  filter(measure != "Electricity")%>%
  mutate(measure_1 = ifelse(measure == "CO2 within", "national", "global"))

relle <- 0.8

P.1 <- ggplot(jointly, aes(x = measure_1, y = value, fill = type))+
  geom_col(position = position_stack(reverse = TRUE), colour = "black", width = 0.75)+
  facet_wrap(~ country, nrow = 1, strip.position = "bottom")+
  theme_bw()+
  scale_fill_npg(labels = c("Covered", "Not covered"))+
  guides(fill = guide_legend(title = NULL))+
  ylab(bquote(~CO[2]~ 'Emissions in Mt')) + xlab("")+ 
  scale_y_continuous(labels = function(x)format(x, decimal.mark = ".", big.mark = ",", scientific = FALSE))+
  theme(legend.background = element_rect(fill = "white", colour = "black"),legend.position = c(0.8, 0.8), panel.border = element_rect(size = 1), legend.text = element_text(size = 12), strip.text = element_text(size = 9), strip.background = element_rect(colour = "black", size = 1), axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 7), axis.ticks = element_blank(), axis.title.y = element_text(size = 10))

P.1

png("Figure_S5_%d.png", width = 20, height = 10, unit = "cm", res = ppi)

P.1

dev.off()

# 14       Correlation Figure on Food, Services, Goods      (Figures S6 to S9) ####

add_BAN <- read_csv("Aggregated_Data/Decomposition_files/Expenditure_types_Bangladesh.csv")
add_INI <- read_csv("Aggregated_Data/Decomposition_files/Expenditure_types_India.csv")
add_INO <- read_csv("Aggregated_Data/Decomposition_files/Expenditure_types_Indonesia.csv")
add_PAK <- read_csv("Aggregated_Data/Decomposition_files/Expenditure_types_Pakistan.csv")
add_PHI <- read_csv("Aggregated_Data/Decomposition_files/Expenditure_types_Philippines.csv")
add_THA <- read_csv("Aggregated_Data/Decomposition_files/Expenditure_types_Thailand.csv")
add_TUR <- read_csv("Aggregated_Data/Decomposition_files/Expenditure_types_Turkey.csv")
add_VIE <- read_csv("Aggregated_Data/Decomposition_files/Expenditure_types_Vietnam.csv")

function_9.1 <- function(Incidence_0, add_0, Country.Name, fill0 = FALSE, ATY = element_blank(), ATX = element_text(size = 20), ATT = element_text(size = 25), XLAB = "", YLAB = ""){
  Inci_0 <- Incidence_0 %>%
    select(hh_id, hh_size, hh_weights, hh_expenditure_USD_pc, Income_Group_5, burden_CO2_within_per_capita)%>%
    mutate(IG_100 = as.numeric(binning(hh_expenditure_USD_pc, bins=100,   method=c("wtd.quantile"), labels=seq(1,100,length.out=100),     weights = hh_weights)))%>%
    left_join(add_0, by = "hh_id")%>%
    mutate(share_goods    = ifelse(is.na(share_goods) , 0, share_goods),
           share_food     = ifelse(is.na(share_food)  , 0, share_food),
           share_energy   = ifelse(is.na(share_energy), 0, share_energy),
           share_services = ifelse(is.na(share_services), 0, share_services))
  
  Inci_0.1 <- Inci_0 %>%
    group_by(IG_100)%>%
    summarise(IG_5 = round(median(Income_Group_5)),
              share_goods    = wtd.quantile(share_goods,                  probs = 0.5, weights = hh_weights),
              share_food     = wtd.quantile(share_food,                   probs = 0.5, weights = hh_weights),
              share_energy   = wtd.quantile(share_energy,                 probs = 0.5, weights = hh_weights),
              share_services = wtd.quantile(share_services,               probs = 0.5, weights = hh_weights),
              burden_within  = wtd.quantile(burden_CO2_within_per_capita, probs = 0.5, weights = hh_weights))%>%
    ungroup()

  Inci_2 <- Inci_0 %>%
    filter(!is.na(burden_CO2_within_per_capita))%>%
    filter(!is.na(share_goods))%>%
    filter(!is.na(share_food))%>%
    filter(!is.na(share_energy))%>%
    filter(!is.na(share_services))
  
t_goods    <- round(cor(Inci_2$burden_CO2_within_per_capita, Inci_2$share_goods, method = "pearson")   ,2)
t_food     <- round(cor(Inci_2$burden_CO2_within_per_capita, Inci_2$share_food, method = "pearson")    ,2)
t_services <- round(cor(Inci_2$burden_CO2_within_per_capita, Inci_2$share_services, method = "pearson"),2)
t_energy   <- round(cor(Inci_2$burden_CO2_within_per_capita, Inci_2$share_energy, method = "pearson")  ,2)

t_final <- data.frame(Country = Country.Name, Goods = t_goods, Food = t_food, Services = t_services, Energy = t_energy)

b_goods    <- round(tidy(lm(burden_CO2_within_per_capita ~ share_goods, data = Inci_0, weights = hh_weights))$estimate[[2]], 3)
b_food     <- round(tidy(lm(burden_CO2_within_per_capita ~ share_food, data = Inci_0, weights = hh_weights))$estimate[[2]], 3)
b_services <- round(tidy(lm(burden_CO2_within_per_capita ~ share_services, data = Inci_0, weights = hh_weights))$estimate[[2]], 3)
b_energy   <- round(tidy(lm(burden_CO2_within_per_capita ~ share_energy, data = Inci_0, weights = hh_weights))$estimate[[2]], 3)

r_2_goods    <- round(glance(lm(burden_CO2_within_per_capita ~ share_goods, data = Inci_0, weights = hh_weights))$r.squared[[1]]   ,2)
r_2_food     <- round(glance(lm(burden_CO2_within_per_capita ~ share_food, data = Inci_0, weights = hh_weights))$r.squared[[1]]    ,2)
r_2_services <- round(glance(lm(burden_CO2_within_per_capita ~ share_energy, data = Inci_0, weights = hh_weights))$r.squared[[1]]  ,2)
r_2_energy   <- round(glance(lm(burden_CO2_within_per_capita ~ share_services, data = Inci_0, weights = hh_weights))$r.squared[[1]],2)
 
Plot_1 <-  ggplot()+
  stat_smooth(data = Inci_0, aes(y = burden_CO2_within_per_capita, x = share_goods, weight = hh_weights), formula = y ~ x, method = "lm", colour = "black", fill = "black", fullrange = TRUE)+
  theme_bw()+
  geom_point(data = Inci_0.1, aes(y = burden_within, x = share_goods, fill = factor(IG_5)), shape = 22, size = 4, colour = "black", alpha = 0.8)+
  coord_cartesian(xlim = c(0.0,0.525), ylim = c(0.0,0.06))+
  scale_x_continuous(label = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = seq(0,0.50,0.1))+
  scale_y_continuous(label = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = seq(0,0.05, 0.01))+
  scale_colour_manual(values = c("#BC3C29FF","#424242","#000000",  "#A4A4A4", "#0072B5FF"))+
  scale_fill_manual  (values = c("#BC3C29FF","#424242","#000000",  "#A4A4A4", "#0072B5FF"))+
  theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 35), legend.position = "bottom", panel.border = element_rect(size = 1.5), panel.grid.major = element_line(size = 1), strip.text = element_text(size = 25), strip.text.y = element_text(angle = 180), axis.ticks.y = element_blank())+
  xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", linetype = "", fill = "")+
  ggtitle(Country.Name)+
  #guides(fill = guide_legend("Expenditure Quintile"), colour = FALSE, linetype = FALSE)+
  guides(fill = fill0, colour = fill0, linetype = fill0)+
  annotate("text", x = 0.079, y = 0.055, label = paste("~beta == ", b_goods), parse = TRUE, size = 8 )

Plot_2 <-  ggplot()+
  stat_smooth(data = Inci_0, aes(y = burden_CO2_within_per_capita, x = share_food, weight = hh_weights), formula = y ~ x, method = "lm", colour = "black", fill = "black", fullrange = TRUE)+
  theme_bw()+
  geom_point(data = Inci_0.1, aes(y = burden_within, x = share_food, fill = factor(IG_5)), shape = 22, size = 4, colour = "black", alpha = 0.8)+
  coord_cartesian(xlim = c(0.0,0.665), ylim = c(0.0,0.06))+
  scale_x_continuous(label = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = seq(0,0.7,0.1))+
  scale_y_continuous(label = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = seq(0,0.05, 0.01))+
  scale_colour_manual(values = c("#BC3C29FF","#424242","#000000",  "#A4A4A4", "#0072B5FF"))+
  scale_fill_manual  (values = c("#BC3C29FF","#424242","#000000",  "#A4A4A4", "#0072B5FF"))+
  theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 35), legend.position = "bottom", panel.border = element_rect(size = 1.5), panel.grid.major = element_line(size = 1), strip.text = element_text(size = 25), strip.text.y = element_text(angle = 180), axis.ticks.y = element_blank())+
  xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", linetype = "", fill = "")+
  ggtitle(Country.Name)+
  #guides(fill = guide_legend("Expenditure Quintile"))+
  guides(fill = fill0, colour = fill0, linetype = fill0)+
  annotate("text", x = 0.125, y = 0.055, label = paste("~beta == ", b_food), parse = TRUE, size = 8 )

Plot_3 <-  ggplot()+
  stat_smooth(data = Inci_0, aes(y = burden_CO2_within_per_capita, x = share_services, weight = hh_weights), formula = y ~ x, method = "lm", colour = "black", fill = "black", fullrange = TRUE)+
  theme_bw()+
  geom_point(data = Inci_0.1, aes(y = burden_within, x = share_services, fill = factor(IG_5)), shape = 22, size = 4, colour = "black", alpha = 0.8)+
  coord_cartesian(xlim = c(0.0,0.565), ylim = c(0.0,0.06))+
  scale_x_continuous(label = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = seq(0,0.60,0.1))+
  scale_y_continuous(label = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = seq(0,0.05, 0.01))+
  scale_colour_manual(values = c("#BC3C29FF","#424242","#000000",  "#A4A4A4", "#0072B5FF"))+
  scale_fill_manual  (values = c("#BC3C29FF","#424242","#000000",  "#A4A4A4", "#0072B5FF"))+
  theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 35), legend.position = "bottom", panel.border = element_rect(size = 1.5), panel.grid.major = element_line(size = 1), strip.text = element_text(size = 25), strip.text.y = element_text(angle = 180), axis.ticks.y = element_blank())+
  xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", linetype = "", fill = "")+
  ggtitle(Country.Name)+
  #guides(fill = guide_legend("Expenditure Quintile"))+
  guides(fill = fill0, colour = fill0, linetype = fill0)+
  annotate("text", x = 0.106, y = 0.055, label = paste("~beta == ", b_services), parse = TRUE, size = 8 )

Plot_4 <-  ggplot()+
  stat_smooth(data = Inci_0, aes(y = burden_CO2_within_per_capita, x = share_energy, weight = hh_weights), formula = y ~ x, method = "lm", colour = "black", fill = "black", fullrange = TRUE)+
  theme_bw()+
  geom_point(data = Inci_0.1, aes(y = burden_within, x = share_energy, fill = factor(IG_5)), shape = 22, size = 4, colour = "black", alpha = 0.8)+
  coord_cartesian(xlim = c(0.0,0.265), ylim = c(0.0,0.06))+
  scale_x_continuous(label = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = seq(0,0.25,0.05))+
  scale_y_continuous(label = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = seq(0,0.05, 0.01))+
  scale_colour_manual(values = c("#BC3C29FF","#424242","#000000",  "#A4A4A4", "#0072B5FF"))+
  scale_fill_manual  (values = c("#BC3C29FF","#424242","#000000",  "#A4A4A4", "#0072B5FF"))+
  theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 35), legend.position = "bottom", panel.border = element_rect(size = 1.5), panel.grid.major = element_line(size = 1), strip.text = element_text(size = 25), strip.text.y = element_text(angle = 180), axis.ticks.y = element_blank())+
  xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", linetype = "", fill = "")+
  ggtitle(Country.Name)+
  #guides(fill = guide_legend("Expenditure Quintile"))+
  guides(fill = fill0, colour = fill0, linetype = fill0)+
  annotate("text", x = 0.05, y = 0.055, label = paste("~beta == ", b_energy), parse = TRUE, size = 8 )

final <- list("P_1" = Plot_1, "P_2" = Plot_2, "P_3" = Plot_3, "P_4" = Plot_4, "t" = t_final)

return(final)
}

P.BAN.G <- function_9.1(Incidence.Bangladesh, add_BAN, "Bangladesh", ATY = element_text(size = 20, vjust = 0.1), YLAB = "Carbon Pricing Incidence", ATX = element_blank())$P_1
P.BAN.F <- function_9.1(Incidence.Bangladesh, add_BAN, "Bangladesh", ATY = element_text(size = 20, vjust = 0.1), YLAB = "Carbon Pricing Incidence", ATX = element_blank())$P_2
P.BAN.S <- function_9.1(Incidence.Bangladesh, add_BAN, "Bangladesh", ATY = element_text(size = 20, vjust = 0.1), YLAB = "Carbon Pricing Incidence", ATX = element_blank())$P_3
P.BAN.E <- function_9.1(Incidence.Bangladesh, add_BAN, "Bangladesh", ATY = element_text(size = 20, vjust = 0.1), YLAB = "Carbon Pricing Incidence", ATX = element_blank())$P_4

P.INI.G <- function_9.1(Incidence.India, add_INI, "India", ATX = element_blank())$P_1
P.INI.F <- function_9.1(Incidence.India, add_INI, "India", ATX = element_blank())$P_2
P.INI.S <- function_9.1(Incidence.India, add_INI, "India", ATX = element_blank())$P_3
P.INI.E <- function_9.1(Incidence.India, add_INI, "India", ATX = element_blank())$P_4

P.INO.G <- function_9.1(Incidence.Indonesia, add_INO, "Indonesia", ATX = element_blank())$P_1
P.INO.F <- function_9.1(Incidence.Indonesia, add_INO, "Indonesia", ATX = element_blank())$P_2
P.INO.S <- function_9.1(Incidence.Indonesia, add_INO, "Indonesia", ATX = element_blank())$P_3
P.INO.E <- function_9.1(Incidence.Indonesia, add_INO, "Indonesia", ATX = element_blank())$P_4

P.PAK.G <- function_9.1(Incidence.Pakistan, add_PAK, "Pakistan", ATX = element_blank())$P_1
P.PAK.F <- function_9.1(Incidence.Pakistan, add_PAK, "Pakistan", ATX = element_blank())$P_2
P.PAK.S <- function_9.1(Incidence.Pakistan, add_PAK, "Pakistan", ATX = element_blank())$P_3
P.PAK.E <- function_9.1(Incidence.Pakistan, add_PAK, "Pakistan", ATX = element_blank())$P_4

P.PHI.G <- function_9.1(Incidence.Philippines, add_PHI, "Philippines", ATY = element_text(size = 20, vjust = 0.1), YLAB = "Carbon Pricing Incidence", XLAB = "Goods Expenditure Share"   )$P_1
P.PHI.F <- function_9.1(Incidence.Philippines, add_PHI, "Philippines", ATY = element_text(size = 20, vjust = 0.1), YLAB = "Carbon Pricing Incidence", XLAB = "Food Expenditure Share"    )$P_2
P.PHI.S <- function_9.1(Incidence.Philippines, add_PHI, "Philippines", ATY = element_text(size = 20, vjust = 0.1), YLAB = "Carbon Pricing Incidence", XLAB = "Services Expenditure Share")$P_3
P.PHI.E <- function_9.1(Incidence.Philippines, add_PHI, "Philippines", ATY = element_text(size = 20, vjust = 0.1), YLAB = "Carbon Pricing Incidence", XLAB = "Energy Expenditure Share"  )$P_4

P.THA.G <- function_9.1(Incidence.Thailand, add_THA, "Thailand", XLAB = "Goods Expenditure Share"   )$P_1
P.THA.F <- function_9.1(Incidence.Thailand, add_THA, "Thailand", XLAB = "Food Expenditure Share"    )$P_2
P.THA.S <- function_9.1(Incidence.Thailand, add_THA, "Thailand", XLAB = "Services Expenditure Share")$P_3
P.THA.E <- function_9.1(Incidence.Thailand, add_THA, "Thailand", XLAB = "Energy Expenditure Share"  )$P_4

P.TUR.G <- function_9.1(Incidence.Turkey, add_TUR, "Turkey", XLAB = "Goods Expenditure Share"   )$P_1
P.TUR.F <- function_9.1(Incidence.Turkey, add_TUR, "Turkey", XLAB = "Food Expenditure Share"    )$P_2
P.TUR.S <- function_9.1(Incidence.Turkey, add_TUR, "Turkey", XLAB = "Services Expenditure Share")$P_3
P.TUR.E <- function_9.1(Incidence.Turkey, add_TUR, "Turkey", XLAB = "Energy Expenditure Share"  )$P_4

P.VIE.G <- function_9.1(Incidence.Vietnam, add_VIE, "Vietnam", XLAB = "Goods Expenditure Share"   )$P_1
P.VIE.F <- function_9.1(Incidence.Vietnam, add_VIE, "Vietnam", XLAB = "Food Expenditure Share"    )$P_2
P.VIE.S <- function_9.1(Incidence.Vietnam, add_VIE, "Vietnam", XLAB = "Services Expenditure Share")$P_3
P.VIE.E <- function_9.1(Incidence.Vietnam, add_VIE, "Vietnam", XLAB = "Energy Expenditure Share"  )$P_4

P.9.1 <- cowplot::align_plots(P.BAN.G, P.BAN.F, P.BAN.S, P.BAN.E, P.INI.G, P.INI.F, P.INI.S, P.INI.E, P.INO.G, P.INO.F, P.INO.S, P.INO.E, P.PAK.G, P.PAK.F, P.PAK.S, P.PAK.E, P.PHI.G, P.PHI.F, P.PHI.S, P.PHI.E, P.THA.G, P.THA.F, P.THA.S, P.THA.E, P.TUR.G, P.TUR.F, P.TUR.S, P.TUR.E, P.VIE.G, P.VIE.F, P.VIE.S, P.VIE.E, align = "hv")
s.1  <- ggdraw(P.9.1[[1]])
s.2  <- ggdraw(P.9.1[[2]])
s.3  <- ggdraw(P.9.1[[3]])
s.4  <- ggdraw(P.9.1[[4]])
s.5  <- ggdraw(P.9.1[[5]])
s.6  <- ggdraw(P.9.1[[6]])
s.7  <- ggdraw(P.9.1[[7]])
s.8  <- ggdraw(P.9.1[[8]])
s.9  <- ggdraw(P.9.1[[9]])
s.10 <- ggdraw(P.9.1[[10]])
s.11 <- ggdraw(P.9.1[[11]])
s.12 <- ggdraw(P.9.1[[12]])
s.13 <- ggdraw(P.9.1[[13]])
s.14 <- ggdraw(P.9.1[[14]])
s.15 <- ggdraw(P.9.1[[15]])
s.16 <- ggdraw(P.9.1[[16]])
s.17 <- ggdraw(P.9.1[[17]])
s.18 <- ggdraw(P.9.1[[18]])
s.19 <- ggdraw(P.9.1[[19]])
s.20 <- ggdraw(P.9.1[[20]])
s.21 <- ggdraw(P.9.1[[21]])
s.22 <- ggdraw(P.9.1[[22]])
s.23 <- ggdraw(P.9.1[[23]])
s.24 <- ggdraw(P.9.1[[24]])
s.25 <- ggdraw(P.9.1[[25]])
s.26 <- ggdraw(P.9.1[[26]])
s.27 <- ggdraw(P.9.1[[27]])
s.28 <- ggdraw(P.9.1[[28]])
s.29 <- ggdraw(P.9.1[[29]])
s.30 <- ggdraw(P.9.1[[30]])
s.31 <- ggdraw(P.9.1[[31]])
s.32 <- ggdraw(P.9.1[[32]])

png("Regression/Figure_S6_Goods_%d.png", width = 15, height = 16, units = "cm", res = ppi)

s.1
s.5
s.9
s.13
s.17
s.21
s.25
s.29

dev.off()

png("Regression/Figure_S7_Food_%d.png", width = 15, height = 16, units = "cm", res = ppi)

s.2
s.6
s.10
s.14
s.18
s.22
s.26
s.30

dev.off()

png("Regression/Figure_S8_Services_%d.png", width = 15, height = 16, units = "cm", res = ppi)

s.3
s.7
s.11
s.15
s.19
s.23
s.27
s.31

dev.off()

png("Regression/Figure_S5_Energy_%d.png", width = 15, height = 16, units = "cm", res = ppi)

s.4
s.8
s.12
s.16
s.20
s.24
s.28
s.32

dev.off()

# 15       Figure 1 including Power Sector Instruments      (Figure S10) ####

normalize_new <- function(Incidence.X, Country.Name, limit_low, limit_up, step_0, XLAB = "", YLAB = "", ATX = element_text(size = 20), ATY = element_text(size = 20)){
  #Incidence.X <- Incidence.X %>%
  #  select(-value.urban, - value.rural)
  Incidence.X <- Incidence.X %>%
    rename(type = Type_0)%>%
    mutate(Label_2 = ifelse(Country.Name == "Pakistan" & type == "TRSP" & Income_Group_5 > 3, round(pure,2), NA))
  
  
  Incidence.X$help <- paste(Incidence.X$Income_Group_5, "_", Incidence.X$type)
  
  if(Country.Name == "Thailand" | Country.Name == "Turkey" | Country.Name == "Indonesia" | Country.Name == "India") nudge_0 <- -0.25 else nudge_0 <- -0.25
  
  P.1 <- ggplot(Incidence.X, aes(x = factor(Income_Group_5)))+
    geom_hline(yintercept = 1, size = 1, colour = "black")+
    #geom_ribbon(aes(ymin = low, ymax = upper, group = type, fill = type), alpha = 0.2)+
    geom_label_repel(aes(y = 1,    group = type, segment.linetype = 1, label = label, segment.size = 1), size = 7, direction = "y", min.segment.length = 0, nudge_y = nudge_0)+
    geom_label_repel(aes(y = 3,    group = type, segment.linetype = 1, label = Label_2, segment.size = 1), size = 7, direction = "y", min.segment.length = 0, nudge_y = -0.4)+
    #geom_label_repel(aes(y = pure, group = type, segment.linetype = 1, label = label_emissions_coverage, segment.size = 1, size = 15), min.segment.length = 0, hjust = 1, force_pull = 0, nudge_x = 1)+
    geom_line(aes( y = pure, group = type, colour = type, alpha = type), size = 1.5, position = position_dodge(0.2))+
    geom_point(aes(y = pure, group = type, fill = type, shape = type, alpha = type), size = 5, colour = "black", position = position_dodge(0.2))+
    scale_colour_npg(  labels = c("International Carbon Price","National Carbon Price", "Power Sector Instruments", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price")) +
    scale_fill_npg  (  labels = c("International Carbon Price","National Carbon Price", "Power Sector Instruments", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price"))+
    scale_shape_manual(labels = c("International Carbon Price","National Carbon Price", "Power Sector Instruments", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price"), values = c(21,22,23,24,25))+
    scale_alpha_manual(labels = c("International Carbon Price","National Carbon Price", "Power Sector Instruments", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price"), values = c(0.5,0.5,1,0.5,0.5))+
    labs(fill = "", colour = "", shape = "", alpha = "", linetype = "")+
    theme_bw() + 
    scale_x_discrete(labels = c("1","2","3","4","5"))+
    scale_y_continuous(breaks = seq(limit_low, limit_up, step_0))+
    theme(axis.text.x = ATX, axis.text.y = ATY, axis.title = element_text(size = 25), legend.position = "bottom" , plot.title = element_text(size = 35), panel.border = element_rect(size = 1.5), panel.grid.major = element_line(size = 1))+
    coord_cartesian(ylim = c(limit_low-0.0, (limit_up+0.0)))+
    #guides(fill = guide_legend(nrow = 2, order = 1), colour = guide_legend(nrow = 2, order = 1), shape = guide_legend(nrow = 2, order = 1), alpha = FALSE, size = FALSE)+
    guides(fill = FALSE, colour = FALSE, shape = FALSE, size = FALSE, alpha = FALSE)+
    xlab(XLAB)+ylab(YLAB)+ ggtitle(Country.Name)
  
  return(P.1)
}

P.1.BAN   <- normalize_new(Incidence.Bangladesh.2,  "Bangladesh",  0.5, 2.5, 0.50, ATX = element_blank(), YLAB = "Incidence normalized by first Quintile")
P.1.India <- normalize_new(Incidence.India.2,       "India",       0.5, 2.5, 0.50, ATX = element_blank(), ATY = element_blank())
P.1.IDN   <- normalize_new(Incidence.Indonesia.2,   "Indonesia",   0.5, 2.5, 0.50, ATX = element_blank(), ATY = element_blank())
P.1.PAK   <- normalize_new(Incidence.Pakistan.2,    "Pakistan",    0.5, 2.5, 0.50, ATX = element_blank(), ATY = element_blank())
P.1.PHI   <- normalize_new(Incidence.Philippines.2, "Philippines", 0.5, 2.5, 0.50, XLAB = "Expenditure Quintile", YLAB = "Incidence normalized by first Quintile")
P.1.THA   <- normalize_new(Incidence.Thailand.2,    "Thailand",    0.5, 2.5, 0.50, XLAB = "Expenditure Quintile", ATY = element_blank())
P.1.TUR   <- normalize_new(Incidence.Turkey.2,      "Turkey",      0.5, 2.5, 0.50, XLAB = "Expenditure Quintile", ATY = element_blank())
P.1.VIE   <- normalize_new(Incidence.Vietnam.2,     "Vietnam",     0.5, 2.5, 0.50, XLAB = "Expenditure Quintile", ATY = element_blank())

P.10 <- cowplot::align_plots(P.1.BAN, P.1.India, P.1.IDN, P.1.PAK, P.1.PHI, P.1.THA, P.1.TUR, P.1.VIE, align = "hv")
s.1 <- ggdraw(P.10[[1]])
s.2 <- ggdraw(P.10[[2]])
s.3 <- ggdraw(P.10[[3]])
s.4 <- ggdraw(P.10[[4]])
s.5 <- ggdraw(P.10[[5]])
s.6 <- ggdraw(P.10[[6]])
s.7 <- ggdraw(P.10[[7]])
s.8 <- ggdraw(P.10[[8]])

png("Figures/Figure_S10_SI_%d.png", width = 15, height = 16, unit = "cm", res = ppi)

s.1
s.2
s.3
s.4
s.5
s.6
s.7
s.8

dev.off()

# 16       Energy vs. Non-Energy                            (Figure S11) ####

# Data on carbon pricing incidences vs. expenditures on energy vs non-energy items is not part of this repository
# Confidential Data. Available upon request and conditional on approval by responsible statistics authority.
# 17       Correlation Coefficients                         (Table S9) ####

t_BAN <- function_9.1(Incidence.Bangladesh,  add_BAN, "Bangladesh")$t
t_INI <- function_9.1(Incidence.India,       add_INI, "India")$t
t_INO <- function_9.1(Incidence.Indonesia,   add_INO, "Indonesia")$t
t_PAK <- function_9.1(Incidence.Pakistan,    add_PAK, "Pakistan")$t
t_PHI <- function_9.1(Incidence.Philippines, add_PHI, "Philippines")$t
t_THA <- function_9.1(Incidence.Thailand,    add_THA, "Thailand")$t
t_TUR <- function_9.1(Incidence.Turkey,      add_TUR, "Turkey")$t
t_VIE <- function_9.1(Incidence.Vietnam,     add_VIE, "Vietnam")$t

t_total <- rbind(t_BAN, t_INI)%>%
  rbind(t_INO)%>%
  rbind(t_PAK)%>%
  rbind(t_PHI)%>%
  rbind(t_THA)%>%
  rbind(t_TUR)%>%
  rbind(t_VIE)

# write.xlsx(t_total, "Table_S9.xlsx")

# 18       Summary Statistics                               (Tables S2, S7, S8) ####

function_sum_1 <- function(x0, x1, Country.Name){
  if(Country.Name == "Indonesia" | Country.Name == "Thailand" | Country.Name == "Vietnam"){
    urban_code <- Urban.New.2
  }else{
    urban_code <- Urban.New.1
  }
  
  
  y1 <- x1 %>%
    filter(type == "exp_Energy")%>%
    select(hh_id, exp_USD)%>%
    rename(exp_energy_USD = exp_USD)
  
  y2 <- left_join(x0, y1, by = "hh_id")%>%
    left_join(urban_code, by = "urban")%>%
    summarise(units = n(),
              hh_size_mean        = wtd.mean(    hh_size, hh_weights),
              hh_size_median      = wtd.quantile(hh_size, hh_weights, probs = 0.5),
              hh_size_sd          = sqrt(wtd.var(hh_size, hh_weights)),
              Urban               = wtd.mean(urban_01, hh_weights),
              expenditures_mean   = wtd.mean(    hh_expenditures_USD, hh_weights),
              expenditures_median = wtd.quantile(hh_expenditures_USD, hh_weights, probs = 0.5),
              expenditures_sd     = sqrt(wtd.var(hh_expenditures_USD, hh_weights)),
              exp_energy_mean     = wtd.mean(    exp_energy_USD, hh_weights),
              exp_energy_median   = wtd.quantile(exp_energy_USD, hh_weights, probs = 0.5),
              exp_energy_sd       = sqrt(wtd.var(exp_energy_USD, hh_weights)))%>%
    mutate(Country = Country.Name)%>%
    select(Country, everything())
  return(y2)
}

Sum_BAN <- function_sum_1(Incidence.Bangladesh , dec_3_BAN, "Bangladesh")
Sum_INI <- function_sum_1(Incidence.India      , dec_3_INI, "India")
Sum_INO <- function_sum_1(Incidence.Indonesia  , dec_3_INO, "Indonesia")
Sum_PAK <- function_sum_1(Incidence.Pakistan   , dec_3_PAK, "Pakistan")
Sum_PHI <- function_sum_1(Incidence.Philippines, dec_3_PHI, "Philippines")
Sum_THA <- function_sum_1(Incidence.Thailand   , dec_3_THA, "Thailand")
Sum_TUR <- function_sum_1(Incidence.Turkey     , dec_3_TUR, "Turkey")
Sum_VIE <- function_sum_1(Incidence.Vietnam    , dec_3_VIE, "Vietnam")
Sum_TOT <- bind_rows(Sum_BAN, Sum_INI, Sum_INO, Sum_PAK,Sum_PHI, Sum_THA,Sum_TUR,Sum_VIE)

# write.xlsx(Sum_TOT, "Summary_Statistics_HHs.xlsx")

function_sum_2 <- function(x0, Country.Name){
  x0 <- x0 %>%
    group_by(Income_Group_5)%>%
    summarise(
      number = n(),
      CO2_intl_mean   = wtd.mean(tCO2_per_capita, hh_weights),
      CO2_intl_median = wtd.quantile(tCO2_per_capita, hh_weights, probs = 0.5),
      CO2_intl_sd     = sqrt(wtd.var(tCO2_per_capita, hh_weights)),
      CO2_wthn_mean   = wtd.mean(tCO2_from_within_per_capita, hh_weights),
      CO2_wthn_median = wtd.quantile(tCO2_from_within_per_capita, hh_weights, probs = 0.5),
      CO2_wthn_sd     = sqrt(wtd.var(tCO2_from_within_per_capita, hh_weights)),
      Ely_embd_mean   = wtd.mean(electricity_USD_per_capita, hh_weights),
      Ely_embd_median = wtd.quantile(electricity_USD_per_capita, hh_weights, probs = 0.5),
      Ely_embd_sd     = sqrt(wtd.var(electricity_USD_per_capita, hh_weights))
    )%>%
    ungroup()%>%
    mutate(Country = Country.Name)%>%
    select(Country, everything())
  
  return(x0)
}

Sum_BAN_1 <- function_sum_2(Incidence.Bangladesh , "Bangladesh")
Sum_INI_1 <- function_sum_2(Incidence.India      , "India")
Sum_INO_1 <- function_sum_2(Incidence.Indonesia  , "Indonesia")
Sum_PAK_1 <- function_sum_2(Incidence.Pakistan   , "Pakistan")
Sum_PHI_1 <- function_sum_2(Incidence.Philippines, "Philippines")
Sum_THA_1 <- function_sum_2(Incidence.Thailand   , "Thailand")
Sum_TUR_1 <- function_sum_2(Incidence.Turkey     , "Turkey")
Sum_VIE_1 <- function_sum_2(Incidence.Vietnam    , "Vietnam")
Sum_TOT_1 <- bind_rows(Sum_BAN_1, Sum_INI_1, Sum_INO_1, Sum_PAK_1,Sum_PHI_1, Sum_THA_1,Sum_TUR_1,Sum_VIE_1)

# write.xlsx(Sum_TOT_1, "Summary_Statistics_HHs_footprints.xlsx")

function_sum_3 <- function(x0, Country.Name){
  x0 <- x0 %>%
    group_by(Income_Group_5)%>%
    summarise(
      number = n(),
      CO2_intl_mean   = wtd.mean(    burden_CO2_per_capita, hh_weights),
      CO2_intl_median = wtd.quantile(burden_CO2_per_capita, hh_weights, probs = 0.5),
      CO2_intl_sd     = sqrt(wtd.var(burden_CO2_per_capita, hh_weights)),
      CO2_wthn_mean   = wtd.mean(    burden_CO2_within_per_capita, hh_weights),
      CO2_wthn_median = wtd.quantile(burden_CO2_within_per_capita, hh_weights, probs = 0.5),
      CO2_wthn_sd     = sqrt(wtd.var(burden_CO2_within_per_capita, hh_weights)),
      Ely_embd_mean   = wtd.mean(    burden_electricity_per_capita, hh_weights),
      Ely_embd_median = wtd.quantile(burden_electricity_per_capita, hh_weights, probs = 0.5),
      Ely_embd_sd     = sqrt(wtd.var(burden_electricity_per_capita, hh_weights))
    )%>%
    ungroup()%>%
    mutate(Country = Country.Name)%>%
    select(Country, everything())
  
  return(x0)
}

Sum_BAN_2 <- function_sum_3(Incidence.Bangladesh , "Bangladesh")
Sum_INI_2 <- function_sum_3(Incidence.India      , "India")
Sum_INO_2 <- function_sum_3(Incidence.Indonesia  , "Indonesia")
Sum_PAK_2 <- function_sum_3(Incidence.Pakistan   , "Pakistan")
Sum_PHI_2 <- function_sum_3(Incidence.Philippines, "Philippines")
Sum_THA_2 <- function_sum_3(Incidence.Thailand   , "Thailand")
Sum_TUR_2 <- function_sum_3(Incidence.Turkey     , "Turkey")
Sum_VIE_2 <- function_sum_3(Incidence.Vietnam    , "Vietnam")
Sum_TOT_2 <- bind_rows(Sum_BAN_2, Sum_INI_2, Sum_INO_2, Sum_PAK_2, Sum_PHI_2, Sum_THA_2, Sum_TUR_2, Sum_VIE_2)

# write.xlsx(Sum_TOT_2, "Summary_Statistics_HHs_incidences.xlsx")

# 19       Carbon Intensities Expenditure Types             (Table S5) ####

transform_1 <- transform_0 %>%
  select(GTAP, Type)

fun_14 <- function(x, Country.Name){

intensities <- read.xlsx("Carbon_intensities.xlsx", sheet = Country.Name)%>%
  rename(CO2_within_intensity_t_per_dollar = "National.Carbon.Intensity.[tCO2/100USD]")%>%
  select(GTAP, CO2_within_intensity_t_per_dollar)

y <- x %>%
  select(hh_id, hh_size, hh_weights, hh_tot_inc_USD, starts_with("exp_USD"))%>%
  rename_at(vars(starts_with("exp_USD")), list(~ str_replace(., "exp_USD_", "")))%>%
  pivot_longer(!(hh_id:hh_tot_inc_USD), names_to = "GTAP", values_to = "expenditures")%>%
  filter(!is.na(expenditures))%>%
  mutate(weighted_expenditures = expenditures*hh_weights)%>%
  group_by(GTAP)%>%
  summarise(weighted_expenditures = sum(weighted_expenditures))%>%
  ungroup()%>%
  left_join(transform_1, by = "GTAP")%>%
  filter(Type != "Other" & Type != "Deleted")%>%
  left_join(intensities, by = "GTAP")%>%
  group_by(Type)%>%
  summarise(Co2_intensity_within_t_per_dollar = wtd.mean(CO2_within_intensity_t_per_dollar, weights = weighted_expenditures))%>%
  ungroup()%>%
  mutate(Co2_intensity_within_t_per_100_dollar = Co2_intensity_within_t_per_dollar*100)%>%
  select(Type, Co2_intensity_within_t_per_100_dollar)

colnames(y) <- c("Type",Country.Name)

return(y)
}

int_BAN <- fun_14(dec_1_BAN, "Bangladesh")
int_INI <- fun_14(dec_1_INI, "India")
int_INO <- fun_14(dec_1_INO, "Indonesia")
int_PAK <- fun_14(dec_1_PAK, "Pakistan")
int_PHI <- fun_14(dec_1_PHI, "Philippines")
int_THA <- fun_14(dec_1_THA, "Thailand")
int_TUR <- fun_14(dec_1_TUR, "Turkey")
int_VIE <- fun_14(dec_1_VIE, "Vietnam")
int_FULL <- left_join(int_BAN, int_INI, by = "Type")%>%
  left_join(int_INO, by = "Type")%>%
  left_join(int_PAK, by = "Type")%>%
  left_join(int_PHI, by = "Type")%>%
  left_join(int_THA, by = "Type")%>%
  left_join(int_TUR, by = "Type")%>%
  left_join(int_VIE, by = "Type")

#write.xlsx(int_FULL, "Table S5.xlsx")




