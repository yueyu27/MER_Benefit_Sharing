#####################################
# Project: Benefit sharing for MER
#
# Part 0: Data cleaning
#
# 2026 June 09
# by: Yue Yu
#
#####################################


# Local R studio
# R version: 4.5.2

getwd()
setwd("/Users/yueyu/Desktop/MER/1_Raw_Data/")
library(dplyr)
library(tidyverse)
library(PNWColors)


# ----------
#   2023
# ----------
checked_2023 <- read.delim("Raw_cleaned_for_code_2023.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

species_2023 <- checked_2023$organism
unique(species_2023)
# "ecosystem"    "other"        "invertebrate" "seed_plant"   "vertebrate"   "fungi"  

# Yue's note: no bacteria for 2023 organisms used in the papers


# ----------
#   2024
# ----------
checked_2024 <- read.delim("Raw_cleaned_for_code_2024.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

# ----------
#   2025
# ----------
checked_2025 <- read.delim("Raw_cleaned_for_code_2025.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

# ---------
#   2026
# ----------
checked_2026 <- read.delim("Raw_cleaned_for_code_2026.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)


# END