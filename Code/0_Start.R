# This script serves the purpose of running the entire analysis of
# Climate Policy and Distributional Impacts in Developing Asia by Steckel et al.

# Author of this Code: L. Missbach, Mercator Research Institute on Global Commons and Climate Change
# Contact: missbach@mcc-berlin.net

# Packages ####

library("cowplot")
library("data.table")
library("foreign")
library("ggthemes")
library("ggsci")
library("haven")
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

# Scripts ####

# Clean Raw Data

source("1_Cleaning_Bangladesh.R")
source("1_Cleaning_India.R")
source("1_Cleaning_Indonesia.R")
source("1_Cleaning_Pakistan.R")
source("1_Cleaning_Philippines.R")
# Note that we draw on raw data for Turkey.
source("1_Cleaning_Thailand.R")
source("1_Cleaning_Vietnam.R")

# Run Analysis

source("2_Analysis.R")

# Figures and Tables

source("3_Graphics.R")