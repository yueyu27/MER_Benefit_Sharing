#####################################
# Project: Benefit sharing for MER
#
# Part 2: Benefit VS NO-Benefit for countries & species
#
# 2026 June 10
# by: Yue Yu
#
#####################################

# Local R studio
# R version: 4.5.2

library(dplyr)
library(tidyr)
library(stringr)
library(PNWColors)
library(ggplot2)
library(patchwork)



# -----------------------------------------------
#
#    Step 1: Load data
#
# -----------------------------------------------

getwd()
setwd("/Users/yueyu/Desktop/MER/1_Raw_Data/")

# ----------
#   2023
# ----------
checked_2023 <- read.delim("Raw_cleaned_for_code_2023.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

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


# -----------------------------------------------
#
#    Step 2: Load 142 countires signed Nygoya Protocol
#
# -----------------------------------------------

sign <- read.delim("Signed_country_list.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)
dim(sign) # 142 countries signed




# -------------------------------------------
#
# -- Step 3: Pool --> Categorize countries according ot Nagoya
#
# -------------------------------------------

pool <- rbind(checked_2023,checked_2024,checked_2025,checked_2026)
pool <- pool[,c(1,2,4,5)]

pool$organism <- gsub("Other", "other", pool$organism)
pool$organism <- gsub("Invertebrate ", "invertebrate", pool$organism)
unique(pool$organism)
# "ecosystem"    "other"        "invertebrate" "seed_plant"   "vertebrate"   "fungi"       
# "bacteria" 

# 7 SPECIES CATGORIES


sign_countries <- str_trim(sign$countries)

pool <- pool %>%
  rowwise() %>%
  mutate(
    # split and trim, then wrap in list()
    country_list = list(str_split(countries, ",\\s*")[[1]] %>% str_trim()),
    all_in_sign = all(country_list %in% sign_countries),
    any_in_sign = any(country_list %in% sign_countries),
    country_type = case_when(
      all_in_sign ~ "Party (All)",
      !any_in_sign ~ "Non-Party",
      TRUE ~ "Party (Mixed)"
    )
  ) %>%
  ungroup() %>%
  select(-country_list, -all_in_sign, -any_in_sign)




# -------------------------------------------
#
# -- Step 4: with/withOUT benefit sharing
#
# -------------------------------------------

# Create a new column to indicate whether 'benefit' is blank or not
pool <- pool %>%
  mutate(benefit_status = ifelse(benefit == "" | is.na(benefit),
                                 "blank", "not_blank"))


# 1. Summarize counts by country_type

country_summary <- pool %>%
  group_by(country_type, benefit_status) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = benefit_status, values_from = count, values_fill = 0) %>%
  mutate(
    sum = blank + not_blank,
    non_benefit_ratio = round(not_blank / sum * 100, digits = 1),
    country_type = factor(country_type, levels = country_type[order(-not_blank)])
  )


country_summary
# A tibble: 3 × 5
  country_type  blank not_blank   sum non_benefit_ratio
  <fct>         <int>     <int> <int>             <dbl>
1 Non-Party        96        56   152              36.8
2 Party (All)     148        74   222              33.3
3 Party (Mixed)    87        53   140              37.9


# 2. Summarize counts by species
species_summary <- pool %>%
  group_by(organism, benefit_status) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = benefit_status, values_from = count, values_fill = 0) %>%
  mutate(
    sum = blank + not_blank,
    non_benefit_ratio = round(not_blank / sum * 100, digits = 1),
    organism = factor(organism, levels = organism[order(-not_blank)])
  )


species_summary
# A tibble: 7 × 5
  organism     blank not_blank   sum non_benefit_ratio
  <fct>        <int>     <int> <int>             <dbl>
1 bacteria         4         0     4               0  
2 ecosystem       22        13    35              37.1
3 fungi            6         5    11              45.5
4 invertebrate    62        37    99              37.4
5 other          136        53   189              28  
6 seed_plant      33        10    43              23.3
7 vertebrate      68        65   133              48.9





# -------------------------------------------
#
# -- Step 5: Plotting
#
# -------------------------------------------


#  == Species plot

species_long <- species_summary %>%
  select(organism, blank, not_blank) %>%
  pivot_longer(cols = c(blank, not_blank), names_to = "benefit_status", values_to = "count") %>%
  mutate(count = ifelse(benefit_status == "blank", -count, count))
# Blank == No benefit == negative

# Vertical bar plot
pal2 <- pnw_palette("Moth", 9)

a <- ggplot(species_long, aes(x = organism, y = count, fill = organism)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_fill_manual(values = pal2) +        # defined color palette
    scale_y_continuous(
    limits = c(-150, 80),                     # set y-axis range
    breaks = seq(-150, 80, by = 50),          # define breaks
    labels = abs(seq(-150, 80, by = 50))      # show all labels as positive
  ) +     
  labs(
    x = "Taxon",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 15, margin = margin(t = 8), color = "black"),
    axis.title.y = element_text(size = 15, margin = margin(r = 8), color = "black"),
    axis.text.x = element_text(size = 12, angle = 30, hjust = 1, color = "black"),
    axis.text.y = element_text(size = 12, color = "black")
  )




#  == Country plot

country_long <- country_summary %>%
  select(country_type, blank, not_blank) %>%
  pivot_longer(cols = c(blank, not_blank), names_to = "benefit_status", values_to = "count") %>%
  mutate(count = ifelse(benefit_status == "blank", -count, count))
# Blank == No benefit == negative

# Vertical bar plot
pal1 <- c("#81a9ad","#537380","#334540")

b <- ggplot(country_long, aes(x = country_type, y = count, fill = country_type)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_fill_manual(values = pal1) +        # defined color palette
    scale_y_continuous(
    limits = c(-150, 80),                     # set y-axis range
    breaks = seq(-150, 80, by = 50),          # define breaks
    labels = abs(seq(-150, 80, by = 50))      # show all labels as positive
  ) +     
  labs(
    x = "Nagoya Protocol Status",
    y = "Number of publications"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 15, margin = margin(t = 8), color = "black"),
    axis.title.y = element_text(size = 15, margin = margin(r = 8), color = "black"),
    axis.text.x = element_text(size = 12, angle = 30, hjust = 1, color = "black"),
    axis.text.y = element_text(size = 12, color = "black")
  )



 # Combine plots next to each other

combined_plot <- b | a  # use p1 | p2 for side-by-side
combined_plot



# Figure 2  


# END
