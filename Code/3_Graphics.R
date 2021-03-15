# 0        General ####

# Authors: L. Missbach - missbach@mcc-berlin.net, L. Montrone - montrone@mcc-berlin.net, I. Dorband - dorband@mcc-berlin.net

# 1        Packages ####

library("cowplot")
library("data.table")
library("foreign")
library("ggthemes")
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
    select(hh_id, hh_size, hh_weights, Urban, Income_Group_5, burden_CO2_per_capita, burden_CO2_within_per_capita, burden_electricity_per_capita)
  
  File_1 <- File %>%
    group_by(Income_Group_5)%>%
    summarise(
      wtd.median_CO2        = wtd.quantile(burden_CO2_per_capita,         weight = hh_weights, probs = 0.5),
      wtd.median_CO2_within = wtd.quantile(burden_CO2_within_per_capita,  weight = hh_weights, probs = 0.5),
      wtd.median_ely        = wtd.quantile(burden_electricity_per_capita, weight = hh_weights, probs = 0.5),
      
      wtd.25_CO2        = wtd.quantile(burden_CO2_per_capita,         weight = hh_weights, probs = 0.25),
      wtd.25_CO2_within = wtd.quantile(burden_CO2_within_per_capita,  weight = hh_weights, probs = 0.25),
      wtd.25_ely        = wtd.quantile(burden_electricity_per_capita, weight = hh_weights, probs = 0.25),
      
      wtd.75_CO2        = wtd.quantile(burden_CO2_per_capita,         weight = hh_weights, probs = 0.75),
      wtd.75_CO2_within = wtd.quantile(burden_CO2_within_per_capita,  weight = hh_weights, probs = 0.75),
      wtd.75_ely        = wtd.quantile(burden_electricity_per_capita, weight = hh_weights, probs = 0.75)
      
    )%>%
    ungroup()
  
  File_2 <- File_1 %>%
    mutate(CO2              = wtd.median_CO2       /File_1$wtd.median_CO2[1],
           CO2_within       = wtd.median_CO2_within/File_1$wtd.median_CO2_within[1],
           ELY              = wtd.median_ely       /File_1$wtd.median_ely[1])%>%
    mutate(CO2_low          = wtd.25_CO2           /File_1$wtd.median_CO2[1],
           CO2_within_low   = wtd.25_CO2_within    /File_1$wtd.median_CO2_within[1],
           ELY_low          = wtd.25_ely           /File_1$wtd.median_ely[1])%>%
    mutate(CO2_upper        = wtd.75_CO2           /File_1$wtd.median_CO2[1],
           CO2_within_upper = wtd.75_CO2_within    /File_1$wtd.median_CO2_within[1],
           ELY_upper        = wtd.75_ely           /File_1$wtd.median_ely[1])
  
  File_3 <- File_2 %>%
    select(Income_Group_5, CO2:ELY_upper)
  
  File_3a <- File_3 %>%
    select(Income_Group_5, CO2, CO2_low, CO2_upper)%>%
    mutate(Type_0 = "CO2")%>%
    rename(pure = CO2, low = CO2_low, upper = CO2_upper)

  File_3b <- File_3 %>%
    select(Income_Group_5, CO2_within, CO2_within_low, CO2_within_upper)%>%
    mutate(Type_0 = "CO2_within")%>%
    rename(pure = CO2_within, low = CO2_within_low, upper = CO2_within_upper)
  
  File_3c <- File_3 %>%
    select(Income_Group_5, ELY, ELY_low, ELY_upper)%>%
    mutate(Type_0 = "ELY")%>%
    rename(pure = ELY, low = ELY_low, upper = ELY_upper)
  
  File_4 <- rbind(File_3a, File_3b)%>%
    rbind(File_3c)
  
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

Incidence.Bangladesh   <- read_csv("/Aggregated_Data/Incidence.Analysis.Bangladesh_11_2020.csv")
I.B <- left_join(Incidence.Bangladesh, Urban.New.1, by = "urban")
Incidence.Bangladesh.1 <- (shape.for.graphical.analysis.1(Incidence.Bangladesh, Urban.New.1))$File_4
Incidence.Bangladesh.1.1 <- shape.for.graphical.analysis.1.1(Incidence.Bangladesh.1)$File_1
Incidence.Bangladesh.1.2 <- shape.for.graphical.analysis.3(Incidence.Bangladesh)
Incidence.Bangladesh.1.3 <- shape.for.graphical.analysis.4(Incidence.Bangladesh)
Incidence.Bangladesh.1.4 <- shape.for.graphical.analysis.5(Incidence.Bangladesh)
Incidence.Bangladesh.1.5 <- shape.for.graphical.analysis.6(Incidence.Bangladesh, Urban.New.1)
Incidence.Bangladesh.2 <- shape.for.graphical.analysis.2(Incidence.Bangladesh, Urban.New.1)
Incidence.Bangladesh.2.1 <- shape.for.graphical.analysis.2.1(Incidence.Bangladesh.2)

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
Incidence.India.2.1 <- shape.for.graphical.analysis.2.1(Incidence.India.2)

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
Incidence.Indonesia.2.1 <- shape.for.graphical.analysis.2.1(Incidence.Indonesia.2)

# 2.4      Pakistan ####

Incidence.Pakistan   <- read_csv("Aggregated_Data/Incidence.Analysis.Pakistan_11_2020.csv")
I.P <- left_join(Incidence.Pakistan, Urban.New.1, by = "urban")
Incidence.Pakistan.1 <- (shape.for.graphical.analysis.1(Incidence.Pakistan, Urban.New.1))$File_4
Incidence.Pakistan.1.1 <- shape.for.graphical.analysis.1.1(Incidence.Pakistan.1)$File_1
Incidence.Pakistan.1.2 <- shape.for.graphical.analysis.3(Incidence.Pakistan)
Incidence.Pakistan.1.3 <- shape.for.graphical.analysis.4(Incidence.Pakistan)
Incidence.Pakistan.1.4 <- shape.for.graphical.analysis.5(Incidence.Pakistan)
Incidence.Pakistan.1.5 <- shape.for.graphical.analysis.6(Incidence.Pakistan, Urban.New.1)
# Weights.Pakistan     <- unlist((shape.for.graphical.analysis.1(Incidence.Pakistan, Urban.New.2))$File_0, use.names = FALSE)

Incidence.Pakistan.2 <- shape.for.graphical.analysis.2(Incidence.Pakistan, Urban.New.1)
Incidence.Pakistan.2.1 <- shape.for.graphical.analysis.2.1(Incidence.Pakistan.2)

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
Incidence.Philippines.2.1 <- shape.for.graphical.analysis.2.1(Incidence.Philippines.2)

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
Incidence.Thailand.2.1 <- shape.for.graphical.analysis.2.1(Incidence.Thailand.2)

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
Incidence.Turkey.2.1 <- shape.for.graphical.analysis.2.1(Incidence.Turkey.2)

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
Incidence.Vietnam.2.1 <- shape.for.graphical.analysis.2.1(Incidence.Vietnam.2)

# 3        Functions / Graphical Output ####

setwd("..") # Select final repository for graphics

ppi <- 400

# 4        Normalized                                       (Figure 1)  ####

normalize_new <- function(Incidence.X, Country.Name, limit_low, limit_up, step_0, XLAB = "", YLAB = "", ATX = element_text(size = 20), ATY = element_text(size = 20)){

  Incidence.X <- Incidence.X %>%
    rename(type = Type_0)
  
  
  Incidence.X$help <- paste(Incidence.X$Income_Group_5, "_", Incidence.X$type)
  
  P.1 <- ggplot(Incidence.X, aes(x = factor(Income_Group_5)))+
    geom_hline(yintercept = 1, size = 1, colour = "black")+
    #geom_ribbon(aes(ymin = low, ymax = upper, group = type, fill = type), alpha = 0.2)+
    geom_line(aes(y = pure,  group = type, colour = type), size = 2, position = position_dodge(0.2))+
    geom_point(aes(y = pure, group = type, fill = type, shape = type  ), size = 5, colour = "black", position = position_dodge(0.2))+
    scale_colour_npg(labels = c("International Carbon Price","National Carbon Price", "Power Sector Instruments")) +
    scale_fill_npg  (labels = c("International Carbon Price","National Carbon Price", "Power Sector Instruments"))+
    scale_shape_manual(values = c(21,22,23), labels = c("International Carbon Price","National Carbon Price", "Power Sector Instruments"))+
    labs(fill = "", colour = "", shape = "", alpha = "", linetype = "")+
    theme_bw() + 
    scale_x_discrete(labels = c("1","2","3","4","5"))+
    scale_y_continuous(breaks = seq(limit_low, limit_up, step_0))+
    theme(axis.text.x = ATX, axis.text.y = ATY, axis.title = element_text(size = 25), legend.position = "bottom" , plot.title = element_text(size = 35), panel.border = element_rect(size = 1.5), panel.grid.major = element_line(size = 1))+
    coord_cartesian(ylim = c(limit_low-0.0, (limit_up+0.0)))+
    #guides(fill = FALSE, colour = guide_legend(nrow = 1, order = 1), shape = FALSE)+
    guides(fill = FALSE, colour = FALSE, shape = FALSE)+
    xlab(XLAB)+ylab(YLAB)+ ggtitle(Country.Name)
  
  return(P.1)
}

P.1.BAN <- normalize_new(Incidence.Bangladesh.2,  "Bangladesh",  0.5, 2.5, 0.50, ATX = element_blank(), YLAB = "Incidence normalized by first Quintile")
P.1.India <- normalize_new(Incidence.India.2,     "India",       0.5, 1.3, 0.25, XLAB = "Expenditure Quintile", YLAB = "Incidence normalized by first Quintile")
P.1.IDN <- normalize_new(Incidence.Indonesia.2,   "Indonesia",   0.5, 1.3, 0.25, XLAB = "Expenditure Quintile", ATY = element_blank())
P.1.PAK <- normalize_new(Incidence.Pakistan.2,    "Pakistan",    0.5, 2.5, 0.50, ATX = element_blank(), ATY = element_blank())
P.1.PHI <- normalize_new(Incidence.Philippines.2, "Philippines", 0.5, 2.5, 0.50, ATX = element_blank(), ATY = element_blank(), XLAB = "", YLAB = "")
P.1.THA <- normalize_new(Incidence.Thailand.2,    "Thailand",    0.5, 1.3, 0.25, XLAB = "Expenditure Quintile", ATY = element_blank())
P.1.TUR <- normalize_new(Incidence.Turkey.2,      "Turkey",      0.5, 1.3, 0.25, XLAB = "Expenditure Quintile", ATY = element_blank())
P.1.VIE <- normalize_new(Incidence.Vietnam.2,     "Vietnam",     0.5, 2.5, 0.50, ATX = element_blank(), XLAB = "", ATY = element_blank())

P.10 <- cowplot::align_plots(P.1.BAN, P.1.India, P.1.IDN, P.1.PAK, P.1.PHI, P.1.THA, P.1.TUR, P.1.VIE, align = "hv")
s.1 <- ggdraw(P.10[[1]])
s.2 <- ggdraw(P.10[[2]])
s.3 <- ggdraw(P.10[[3]])
s.4 <- ggdraw(P.10[[4]])
s.5 <- ggdraw(P.10[[5]])
s.6 <- ggdraw(P.10[[6]])
s.7 <- ggdraw(P.10[[7]])
s.8 <- ggdraw(P.10[[8]])

png("Figure_1_%d.png", width = 15, height = 16, unit = "cm", res = ppi)

s.1
s.2
s.3
s.4
s.5
s.6
s.7
s.8

dev.off()

# L.1 <- ggdraw(get_legend(P.1.BAN))
# 
# png("Legend_Fig_1.png", width = 15, height = 3, unit = "cm", res = ppi)
# 
# L.1
# 
# dev.off()
# 
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
  
  P_1 <- ggplot(Incidence.X1.1, aes(group = factor(Income_Group_5), colour = factor(Income_Group_5), linetype = factor(Income_Group_5)))+
    theme_bw()+
    theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 35), legend.position = "bottom", panel.border = element_rect(size = 1.5), panel.grid.major = element_line(size = 1), strip.text = element_text(size = 25), strip.text.y = element_text(angle = 180))+
    geom_point(aes(x = median.x, y = median.y, group = factor(Income_Group_5), fill = factor(Income_Group_5)), shape = 21, size = 5)+
    geom_smooth(aes(x = burden_CO2_within_per_capita, y = share), size = 1, method = "loess", span = adjust_0, se = FALSE)+
    xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", linetype = "")+
    scale_y_continuous(breaks = c(0,0.0005,0.01), labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
    scale_x_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1), breaks = seq(0,0.08, 0.02))+
    coord_cartesian(xlim = c(0,0.085), ylim = c(0,0.01))+
    #geom_segment(aes(x = median, xend = median, y = 0, yend = 100, colour = factor(Income_Group_5), linetype = factor(Income_Group_5)), size = 1)+
    scale_colour_manual(values = c("#BC3C29FF","#424242","#000000",  "#A4A4A4", "#0072B5FF"))+
    scale_fill_manual(values = c("#BC3C29FF","#424242","#000000",  "#A4A4A4", "#0072B5FF"))+
    scale_linetype_manual(values = c("solid", "dashed", "solid", "dotdash", "solid"))+
    ggtitle(Country.Name)+
    guides(fill = fill0, colour = fill0, linetype = fill0)
  
  P_2 <- ggplot(Incidence.X2.1, aes(group = factor(Income_Group_5), colour = factor(Income_Group_5), linetype = factor(Income_Group_5)))+
    theme_bw()+
    theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 35), legend.position = "bottom", panel.border = element_rect(size = 1.5), panel.grid.major = element_line(size = 1), strip.text = element_text(size = 25), strip.text.y = element_text(angle = 180))+
    geom_point(aes(x = median.x, y = median.y, group = factor(Income_Group_5), fill = factor(Income_Group_5)), shape = 21, size = 5)+
    geom_smooth(aes(x = burden_CO2_within_per_capita, y = share), size = 1, method = "loess", span = adjust_0, se = FALSE)+
    xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", linetype = "")+
    scale_y_continuous(breaks = c(0,0.005,0.01), expand = c(0,0), labels = scales::percent_format(accuracy = 0.1))+
    scale_x_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1), breaks = seq(0,0.08, 0.02))+
    coord_cartesian(xlim = c(0,0.085), ylim = c(0,0.01))+
    #geom_segment(aes(x = median, xend = median, y = 0, yend = 100, colour = factor(Income_Group_5), linetype = factor(Income_Group_5)), size = 1)+
    scale_colour_manual(values = c("#BC3C29FF","#424242","#000000",  "#A4A4A4", "#0072B5FF"))+
    scale_fill_manual(values = c("#BC3C29FF","#424242","#000000",  "#A4A4A4", "#0072B5FF"))+
    scale_linetype_manual(values = c("solid", "dashed", "solid", "dotdash", "solid"))+
    ggtitle(Country.Name)+
    guides(fill = fill0, colour = fill0, linetype = fill0)
  
  P_3 <- ggplot(Incidence.X3.1, aes(group = factor(Income_Group_5), colour = factor(Income_Group_5), linetype = factor(Income_Group_5)))+
    theme_bw()+
    theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 35), legend.position = "bottom", panel.border = element_rect(size = 1.5), panel.grid.major = element_line(size = 1), strip.text = element_text(size = 25), strip.text.y = element_text(angle = 180))+
    geom_smooth(aes(x = burden_CO2_within_per_capita, y = share), size = 1, method = "loess", n = 160, span = adjust_0, se = FALSE, fullrange = TRUE)+
    geom_point(aes(x = median.x, y = median.y, group = factor(Income_Group_5), fill = factor(Income_Group_5)), shape = 21, size = 5, colour = "black")+
    xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", linetype = "", fill = "")+
    scale_y_continuous(breaks = c(0,0.05,0.1), expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
    scale_x_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1), breaks = seq(0,0.08, 0.02))+
    coord_cartesian(xlim = c(0,0.085), ylim = c(0,0.1))+
    #geom_segment(aes(x = median, xend = median, y = 0, yend = 100, colour = factor(Income_Group_5), linetype = factor(Income_Group_5)), size = 1)+
    scale_colour_manual(  values = c("#BC3C29FF","#424242","#000000",  "#A4A4A4", "#0072B5FF"))+
    scale_fill_manual(    values = c("#BC3C29FF","#424242","#000000",  "#A4A4A4", "#0072B5FF"))+
    scale_linetype_manual(values = c("solid", "dashed", "solid", "dotdash", "solid"))+
    ggtitle(Country.Name)+
    #guides(fill = FALSE, colour = guide_legend("Expenditure Quintile"), linetype = guide_legend("Expenditure Quintile"))
    guides(fill = fill0, colour = fill0, linetype = fill0)
  
  Incidence.X.4 <- rbind(Incidence.X1.1, Incidence.X2.1)%>%
    filter(Income_Group_5 == 1 | Income_Group_5 == 5)
  
  
  P_4 <- ggplot(Incidence.X.4, aes(colour = factor(Income_Group_5), linetype = factor(Urban)))+
    theme_bw()+
    theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 35), legend.position = "bottom", panel.border = element_rect(size = 1.5), panel.grid.major = element_line(size = 1), strip.text = element_text(size = 25), strip.text.y = element_text(angle = 180))+
    geom_smooth(aes(x = burden_CO2_within_per_capita, y = share), size = 1, method = "loess", span = adjust_0, se = FALSE, n = 160, fullrange = TRUE)+
    geom_point(aes(x = median.x, y = median.y, fill = factor(Income_Group_5), alpha = factor(Urban)), shape = 21, size = 5, colour = "black")+
    xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", linetype = "", fill = "")+
    scale_y_continuous(breaks = c(0,0.05, 0.1), expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
    scale_x_continuous(expand = c(0,0), breaks = seq(0,0.08, 0.02), labels = scales::percent_format(accuracy = 1))+
    coord_cartesian(xlim = c(0,0.085), ylim = c(0,0.11))+
    #geom_segment(aes(x = median, xend = median, y = 0, yend = 100, colour = factor(Income_Group_5), linetype = factor(Income_Group_5)), size = 1)+
    scale_colour_manual(  values = c("#BC3C29FF", "#0072B5FF"))+
    scale_fill_manual(    values = c("#BC3C29FF", "#0072B5FF"))+
    scale_linetype_manual(values = c("solid", "dotted"))+
    scale_alpha_manual(   values = c(1,0.5))+
    ggtitle(Country.Name)+
    #guides(fill = guide_legend("Expenditure Quintile"), alpha = FALSE, colour = guide_legend("Expenditure Quintile"))
    guides(fill = fill0, colour = fill0, linetype = fill0, alpha = fill0)
  
  Files <- list("Plot_1" = P_1, "Plot_2" = P_2, "Plot_3" = P_3, "Plot_4" = P_4)
  
  return(Files)
}

P.10.Ba.3 <- plotting_ten.1(Incidence.Bangladesh.1,  "Bangladesh",  ATY = element_text(size = 20, vjust = 0.1), ATX = element_blank(),         ATT = element_text(size = 25),          XLAB = "", YLAB = "Share of Households per Quintile")$Plot_3
P.10.Ii.3 <- plotting_ten.1(Incidence.India.1,      adjust_1 = 0.1, "India",       ATY = element_blank(),                      ATX = element_blank(),         ATT = element_blank(),          XLAB = "")$Plot_3
P.10.Io.3 <- plotting_ten.1(Incidence.Indonesia.1,  adjust_1 = 0.1, "Indonesia",   ATY = element_blank(),                      ATX = element_blank(),         ATT = element_blank(),          XLAB = "")$Plot_3
P.10.Pa.3 <- plotting_ten.1(Incidence.Pakistan.1,   adjust_1 = 0.1, "Pakistan",    ATY = element_blank(),                      ATX = element_blank() ,        ATT = element_blank(),          XLAB = "")$Plot_3
P.10.Ph.3 <- plotting_ten.1(Incidence.Philippines.1, adjust_1 = 0.35, "Philippines", ATY = element_text(size = 20, vjust = 0.1), ATX = element_text(size = 20), ATT = element_text(size = 25),  XLAB = "Carbon Price Incidence", YLAB = "Share of Households per Quintile")$Plot_3
P.10.Th.3 <- plotting_ten.1(Incidence.Thailand.1,   adjust = 0.15, "Thailand",    ATY = element_blank(),                      ATX = element_text(size = 20), ATT = element_text(size = 25),  XLAB = "Carbon Price Incidence")$Plot_3
P.10.Tu.3 <- plotting_ten.1(Incidence.Turkey.1,     adjust_1 = 0.12, "Turkey",      ATY = element_blank(),                      ATX = element_text(size = 20), ATT = element_text(size = 25),  XLAB = "Carbon Price Incidence")$Plot_3
P.10.Vi.3 <- plotting_ten.1(Incidence.Vietnam.1,     "Vietnam",     ATY = element_blank(),                      ATX = element_text(size = 20), ATT = element_text(size = 25),  XLAB = "Carbon Price Incidence")$Plot_3

P.10 <- cowplot::align_plots(P.10.Ba.3, P.10.Ii.3, P.10.Io.3, P.10.Pa.3, P.10.Ph.3, P.10.Th.3, P.10.Tu.3, P.10.Vi.3, align = "hv")
s.1 <- ggdraw(P.10[[1]])
s.2 <- ggdraw(P.10[[2]])
s.3 <- ggdraw(P.10[[3]])
s.4 <- ggdraw(P.10[[4]])
s.5 <- ggdraw(P.10[[5]])
s.6 <- ggdraw(P.10[[6]])
s.7 <- ggdraw(P.10[[7]])
s.8 <- ggdraw(P.10[[8]])

png("Figure_2_1%d.png", width = 15, height = 16, unit = "cm", res = ppi)

s.1
s.2
s.3
s.4
s.5
s.6
s.7
s.8

dev.off()

P.10.Ba.4 <- plotting_ten.1(Incidence.Bangladesh.1, adjust_1 = 0.25, "Bangladesh",  ATY = element_text(size = 20, vjust = 0.1), ATX = element_blank(),         ATT = element_text(size = 25),          XLAB = "", YLAB = "Share of Households per Quintile")$Plot_4
P.10.Ii.4 <- plotting_ten.1(Incidence.India.1,      adjust_1 = 0.12, "India",       ATY = element_blank(),                      ATX = element_blank(),         ATT = element_blank(),          XLAB = "")$Plot_4
P.10.Io.4 <- plotting_ten.1(Incidence.Indonesia.1,  adjust_1 = 0.15, "Indonesia",   ATY = element_blank(),                      ATX = element_blank(),         ATT = element_blank(),          XLAB = "")$Plot_4
P.10.Pa.4 <- plotting_ten.1(Incidence.Pakistan.1,   adjust_1 = 0.15, "Pakistan",    ATY = element_blank(),                      ATX = element_blank() ,        ATT = element_blank(),          XLAB = "")$Plot_4
P.10.Ph.4 <- plotting_ten.1(Incidence.Philippines.1,adjust_1 = 0.3, "Philippines",  ATY = element_text(size = 20, vjust = 0.1), ATX = element_text(size = 20), ATT = element_text(size = 25),  XLAB = "Carbon Price Incidence", YLAB = "Share of Households per Quintile")$Plot_4
P.10.Th.4 <- plotting_ten.1(Incidence.Thailand.1,   adjust_1 = 0.15, "Thailand",    ATY = element_blank(),                      ATX = element_text(size = 20), ATT = element_text(size = 25),  XLAB = "Carbon Price Incidence")$Plot_4
P.10.Tu.4 <- plotting_ten.1(Incidence.Turkey.1,     adjust_1 = 0.15, "Turkey",      ATY = element_blank(),                      ATX = element_text(size = 20), ATT = element_text(size = 25),  XLAB = "Carbon Price Incidence")$Plot_4
P.10.Vi.4 <- plotting_ten.1(Incidence.Vietnam.1,    adjust_1 = 0.3, "Vietnam",     ATY = element_blank(),                      ATX = element_text(size = 20), ATT = element_text(size = 25),  XLAB = "Carbon Price Incidence")$Plot_4

P.10 <- cowplot::align_plots(P.10.Ba.4, P.10.Ii.4, P.10.Io.4, P.10.Pa.4, P.10.Ph.4, P.10.Th.4, P.10.Tu.4, P.10.Vi.4, align = "hv")
s.1 <- ggdraw(P.10[[1]])
s.2 <- ggdraw(P.10[[2]])
s.3 <- ggdraw(P.10[[3]])
s.4 <- ggdraw(P.10[[4]])
s.5 <- ggdraw(P.10[[5]])
s.6 <- ggdraw(P.10[[6]])
s.7 <- ggdraw(P.10[[7]])
s.8 <- ggdraw(P.10[[8]])

png("Figure_3_1%d.png", width = 15, height = 16, unit = "cm", res = ppi)

s.1
s.2
s.3
s.4
s.5
s.6
s.7
s.8

dev.off()


# 6        Non-parametric Engel-Curves                      (Figure 4) ####

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


plot_engel <- function(data, Country.Name, fill0 = FALSE, ATY = element_blank(), ATX = element_text(size = 20, hjust = 1), ATT = element_text(size = 25), XLAB = "", YLAB = "", YLIM = 100){
  
  P <- ggplot(data)+
    geom_smooth(formula = as.formula (y ~ x), aes(x = Income_Group_100, y = share, weight = hh_weights, colour = factor(type), fill = factor(type)), size = 1, method = "loess", se = TRUE, fullrange = TRUE, span = 0.75)+
    theme_bw()+
    theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 35), legend.position = "bottom", panel.border = element_rect(size = 1.5), panel.grid.major = element_line(size = 1), strip.text = element_text(size = 25), strip.text.y = element_text(angle = 180))+
    xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", linetype = "")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    scale_x_continuous(expand = c(0,0), breaks = seq(0,100,25))+
    coord_cartesian(ylim = c(0,YLIM), xlim = c(0,100))+
    scale_colour_npg(name = "", labels = c("Biomass", "Coal", "Other Cooking Fuels", "Electricity", "Transport Fuels"))+
    scale_fill_npg  (name = "", labels = c("Biomass", "Coal", "Other Cooking Fuels", "Electricity", "Transport Fuels"))+
    ggtitle(Country.Name)+
    guides(colour = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))+
    labs(fill = "", colour = "")
  #guides(fill = fill0, colour = fill0, linetype = fill0)
  
  return(P)
  
}

P.11.BAN <- plot_engel(dec_2_BAN, "Bangladesh",  YLAB = "Expenditure Share", YLIM = 0.07, ATY = element_text(size = 20, vjust = 0.1), ATX = element_blank())
P.11.INI <- plot_engel(dec_2_INI, "India",       YLIM = 0.07, ATX = element_blank())
P.11.INO <- plot_engel(dec_2_INO, "Indonesia",   YLIM = 0.07, ATX = element_blank())
P.11.PAK <- plot_engel(dec_2_PAK, "Pakistan",    YLIM = 0.07, ATX = element_blank())
P.11.PHI <- plot_engel(dec_2_PHI, "Philippines", YLAB = "Expenditure Share", XLAB = "Expenditure Centile", YLIM = 0.07, ATY = element_text(size = 20, vjust = 0.1))
P.11.THA <- plot_engel(dec_2_THA, "Thailand",    XLAB = "Expenditure Centile", YLIM = 0.15, ATY = element_text(size = 20, vjust = 0.1))
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

png("Figure_4_%d.png", width = 15, height = 16, unit = "cm", res = ppi)

s.1
s.2
s.3
s.4
s.5
s.6
s.7
s.8

dev.off()

# L.1 <- ggdraw(get_legend(P.11.BAN))
# 
# png("Legend_Engel_%d.png", width = 7*ppi, height = 2*ppi, res = ppi)
# 
# L.1
# 
# dev.off()


# 7        Non-parametric Engel-Curves on expenditure types (Figure S3) ####

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

# 8        Boxplots on Expenditure Shares                   (Figure 5) ####

fun_15 <- function(Incidence_0, Country.Name, fill0 = FALSE, ATY = element_blank(), ATX = element_text(size = 20), ATT = element_text(size = 25), XLAB = "", YLAB = ""){
  
  # Confidential Data. Available upon request and conditional on approval by responsible statistics authority.
  t <- read_csv(sprintf("../Expenditure_types_%s.csv", Country.Name))
  
  y <- left_join(Incidence_0, t, by = "hh_id")%>%
    select(hh_id, hh_size, hh_weights, expenditures_USD_2014, Income_Group_5, starts_with("share"))%>%
    pivot_longer(starts_with("share"), names_to = "Type", values_to = "Share")%>%
    group_by(Income_Group_5, Type)%>%
    summarise(y5 = wtd.quantile(Share, probs = 0.05, weights = hh_weights),
              y25 = wtd.quantile(Share, probs = 0.25, weights = hh_weights),
              y50 = wtd.quantile(Share, probs = 0.5, weights = hh_weights),
              y75 = wtd.quantile(Share, probs = 0.75, weights = hh_weights),
              y95 = wtd.quantile(Share, probs = 0.95, weights = hh_weights),
              mean = wtd.mean(Share, weights = hh_weights))%>%
    ungroup()%>%
    filter(Income_Group_5 == 1 | Income_Group_5 == 5)
  
  y$Type <- factor(y$Type, levels = c("share_energy", "share_food", "share_goods", "share_services"), labels = c("Energy", "Food", "Goods", "Services"))
  
  
  Plot_1 <-  ggplot(data = y)+
    theme_bw()+
    geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, x = factor(Type), fill = factor(Income_Group_5)), stat = "identity", position = position_dodge(0.7), outlier.shape = NA, width = 0.5) +
    stat_summary(aes(y = mean, group = interaction(Income_Group_5, Type), x = factor(Type)), fun = "mean", geom = "point", position =  position_dodge(0.7), shape = 23, size = 2, fill = "white")+
    coord_cartesian(ylim = c(0.0,0.85))+
    scale_y_continuous(label = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = seq(0,0.8, 0.2))+
    scale_colour_npg()+
    scale_fill_npg()+
    theme(axis.text.y = ATY, axis.text.x= ATX, axis.title = ATT, plot.title = element_text(size = 35), legend.position = "bottom", panel.border = element_rect(size = 1.5), panel.grid.major = element_line(size = 1), strip.text = element_text(size = 25), strip.text.y = element_text(angle = 180), axis.ticks.y = element_blank())+
    xlab(XLAB)+ ylab(YLAB)+ labs(colour = "", linetype = "", fill = "")+
    ggtitle(Country.Name)+
    #guides(fill = guide_legend("Expenditure Quintile"))#+
    guides(fill = fill0, colour = fill0, linetype = fill0)
  
  return(Plot_1)
}

P.15.BAN <- fun_15(Incidence.Bangladesh,  "Bangladesh", ATY = element_text(size = 20, vjust = 0.1), YLAB = "Expenditure Share")
P.15.INI <- fun_15(Incidence.India,       "India"     )
P.15.INO <- fun_15(Incidence.Indonesia,   "Indonesia" )
P.15.PAK <- fun_15(Incidence.Pakistan,    "Pakistan", )
P.15.PHI <- fun_15(Incidence.Philippines, "Philippines", ATY = element_text(size = 20, vjust = 0.1), YLAB = "Expenditure Share")
P.15.THA <- fun_15(Incidence.Thailand,    "Thailand")
P.15.TUR <- fun_15(Incidence.Turkey,      "Turkey")
P.15.VIE <- fun_15(Incidence.Vietnam,     "Vietnam")

P.15 <- cowplot::align_plots(P.15.BAN, P.15.INI, P.15.INO, P.15.PAK, P.15.PHI, P.15.THA, P.15.TUR, P.15.VIE, align = "hv")
s.1 <- ggdraw(P.15[[1]])
s.2 <- ggdraw(P.15[[2]])
s.3 <- ggdraw(P.15[[3]])
s.4 <- ggdraw(P.15[[4]])
s.5 <- ggdraw(P.15[[5]])
s.6 <- ggdraw(P.15[[6]])
s.7 <- ggdraw(P.15[[7]])
s.8 <- ggdraw(P.15[[8]])

png("Figure_6_%d.png", width = 15, height = 16, unit = "cm", res = ppi)

s.1
s.2
s.3
s.4
s.5
s.6
s.7
s.8

dev.off()

# L.1 <- ggdraw(get_legend(P.15.BAN))
# 
# png("Boxplots/Legend_all_15_%d.png", width = 5*ppi, height = 2*ppi, res = ppi)
# 
# L.1
# 
# dev.off()

# 9        Analysis on Shares on Electricity                (Figure S2) ####

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

# 10       Analysis on included Emissions                   (Figure S4) ####


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

# 11       Vertical vs horizontal equity                    (Table 2) ####

vertical_horizontal <- function(Incidence_Country, Country.Name){
  
  Incidence.X <- Incidence_Country %>%
    select(hh_id, hh_weights, burden_CO2_within_per_capita, Income_Group_5)%>%
    group_by(Income_Group_5)%>%
    summarise(y5  = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.05),
              y50 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.50),
              y95 = wtd.quantile(burden_CO2_within_per_capita, weights = hh_weights, probs = 0.95))%>%
    ungroup()
  
  Incidence.1 <- Incidence.X %>%
    select(Income_Group_5, y5, y95)%>%
    mutate(dif = y95-y5)%>%
    select(-y5, - y95)%>%
    spread(key = Income_Group_5, value = dif)%>%
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
    mutate(Difference_Median = max - min)%>%
    select(-max, -min)
  
  Incidence_0 <- left_join(Incidence.2.0, Incidence.1, by = "Country")
  
  return(Incidence_0)
}

vh_BAN <- vertical_horizontal(Incidence.Bangladesh, "Bangladesh")
vh_INI <- vertical_horizontal(Incidence.India,      "India")
vh_INO <- vertical_horizontal(Incidence.Indonesia,  "Indonesia")
vh_PAK <- vertical_horizontal(Incidence.Pakistan,   "Pakistan")
vh_PHI <- vertical_horizontal(Incidence.Philippines, "Philippines")
vh_THA <- vertical_horizontal(Incidence.Thailand,   "Thailand")
vh_TUR <- vertical_horizontal(Incidence.Turkey,     "Turkey")
vh_VIE <- vertical_horizontal(Incidence.Vietnam,    "Vietnam")

vh_total <- rbind(vh_BAN, vh_INI, vh_INO, vh_PAK, vh_PHI, vh_THA, vh_TUR, vh_VIE)

# write.xlsx(vh_total, "Vertical_horizontal_equity.xlsx")

# 12       Correlation Figure on Food, Services, Goods      (Figures S5 to S8) ####

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

# 13       Correlation Coefficients                         (Table S9) ####

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

# 14       Summary Statistics                               (Tables S2, S7, S8) ####

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

# 15       Coal Pipeline                                    (Figure S1) ####

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

# 16       Carbon Intensities Expenditure Types             (Table S5) ####

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



