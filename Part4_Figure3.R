#####################################
# Project: Benefit sharing
#
# Part 4: Plot manually checked final benefit types (plot 2023-2026 in one) - Figure 3
#
# 2026 June 10
# by: Yue Yu
#
#####################################


# -----------------------------------------------
#
#    Step 1: Load manual checked data 
#
# -----------------------------------------------

# NOTE!!
# Some ariticles in MER have benefit sharing statement but it says vague things such as "This study was conducted in accordance with the principles of the Nagoya Protocol on Access to Genetic Resources and the Fair and Equitable Sharing of Benefits Arising from their Utilization"
# So some "benefit_summary" are NULL (need to plot these)


# -----------------------------------------------
#
#    Step 1: Load manual checked data (done during 2025 Oct 29-31)
#
# -----------------------------------------------

# Local R studio
# R version: 4.4.0

getwd()
setwd("/Users/yueyu/Desktop/MER/2_Data/")
library(dplyr)
library(tidyverse)
library(PNWColors)
library(stringr)



# ----------
#   2023
# ----------
checked_2023 <- read.delim("ABS_code_2023_manually_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

# colClasses = "character": force all column to be characters to aviod "F" being read in as FALSE
checked_2023$year <- "2023"
checked_2023$year <- as.numeric(checked_2023$year)

head(checked_2023)
dim(checked_2023)
# 128 10


# ----------
#   2024
# ----------
checked_2024 <- read.delim("ABS_code_2024_manually_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)
checked_2024$year <- "2024"
checked_2024$year <- as.numeric(checked_2024$year)


head(checked_2024)
dim(checked_2024)
# 118 10


# ----------
#   2025
# ----------
checked_2025 <- read.delim("ABS_code_2025_manually_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)
checked_2025$year <- "2025"
checked_2025$year <- as.numeric(checked_2025$year)


head(checked_2025)
dim(checked_2025)
# 188 10


# ----------
#   2026
# ----------
checked_2026 <- read.delim("ABS_code_2026_manually_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)
checked_2026$year <- "2026"
checked_2026$year <- as.numeric(checked_2026$year)


head(checked_2026)
dim(checked_2026)
# 80 10


# -----------------------------------------------
#
#    Step 2: Benefit category occurence among year
#
# -----------------------------------------------

checked_2023 <- checked_2023[,c(10,9)]
checked_2024 <- checked_2024[,c(10,9)]
checked_2025 <- checked_2025[,c(10,9)]
checked_2026 <- checked_2026[,c(10,9)]

checked_all <- rbind(checked_2023,checked_2024,checked_2025,checked_2026)
dim(checked_all)
# 514 2

# Reshape to long format
df_long <- checked_all %>%
  pivot_longer(cols = c(benefit_summary),
    		   names_to = "category_type",
    		   values_to = "category_value") %>%
  separate_rows(category_value, sep = ",")  # split comma-separated benefits

df_long_clean <- df_long %>%
  filter(!is.na(category_value) & category_value != "")

table(df_long_clean$category_value)
#   A   B   C   D   E   F 
# 154 147  31   1  35  22 


# Extract information from above dataframe
category_counts <- as.data.frame(table(df_long_clean$category_value))
colnames(category_counts) <- c("category", "count")

  category count
1        A   154
2        B   147
3        C    31
4        D     1
5        E    35
6        F    22

# Calculate percentage: Calculated as: 154/183 (total number of articles with benefit from 2023 to 2026)

# -----------------------------------------------
#
#    Step 3: Plot benefit category (Figure 3a)
#
# -----------------------------------------------

# BENEFIT CATEGORY

pal <- pnw_palette("Sailboat", 7)
p1 <- ggplot(category_counts, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = pal) +
  labs(
    x = "Benefit category",
    y = "Number of publications",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14, margin = margin(t = 8)),
    axis.title.y = element_text(size = 14, margin = margin(r = 8)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )













# COMPLETED, NEED TO DO THE FOLLOWING AND PLOT 3B


# -----------------------------------------------
#
#    Step 4.1: Plot benefit category per country type (Figure 3b)
#
# -----------------------------------------------


# ----------
#   2023
# ----------
checked_2023 <- read.delim("ABS_code_2023_manually_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

title_country_2023 <- read.delim("/Users/yueyu/Desktop/MER/1_Raw_Data/Raw_cleaned_for_code_2023.txt",
                      header = TRUE,
                      colClasses = "character",
                      fileEncoding = "Latin1",
                      stringsAsFactors = FALSE)

# merge two dataframes based on "title"
merged_2023_all <- merge(checked_2023, title_country_2023, by = "title", all.x = TRUE)
merged_2023_subset <- merged_2023_all[,c(1,2,9,11,12,13)]
merged_2023_subset$year <- "2023"
merged_2023_subset$year <- as.numeric(merged_2023_subset$year)


# data cleaning
merged_2023_subset$authors <- gsub("Ê","", merged_2023_subset$authors)
head(merged_2023_subset)
dim(merged_2023_subset)

write.table(merged_2023_subset, file = "merged_2023_subset.txt", sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
#export and manually add country to all rows with a benefit type

rm(merged_2023_subset)

merged_2023_subset <- read.delim("merged_2023_subset.txt",
                      header = TRUE,
                      colClasses = "character",
                      fileEncoding = "Latin1",
                      stringsAsFactors = FALSE)



# ----------
#   2024
# ----------
checked_2024 <- read.delim("ABS_code_2024_manually_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

title_country_2024 <- read.delim("/Users/yueyu/Desktop/MER/1_Raw_Data/Raw_cleaned_for_code_2024.txt",
                      header = TRUE,
                      colClasses = "character",
                      fileEncoding = "Latin1",
                      stringsAsFactors = FALSE)

# merge two dataframes based on "title"
merged_2024_all <- merge(checked_2024, title_country_2024, by = "title", all.x = TRUE)
merged_2024_subset <- merged_2024_all[,c(1,2,9,11,12,13)]
merged_2024_subset$year <- "2024"
merged_2024_subset$year <- as.numeric(merged_2024_subset$year)


# data cleaning
merged_2024_subset$authors <- gsub("Ê","", merged_2024_subset$authors)

head(merged_2024_subset)
dim(merged_2024_subset)


write.table(merged_2024_subset, file = "merged_2024_subset.txt", sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE) 

rm(merged_2024_subset)

merged_2024_subset <- read.delim("merged_2024_subset.txt",
                      header = TRUE,
                      colClasses = "character",
                      fileEncoding = "Latin1",
                      stringsAsFactors = FALSE)


# ----------
#   2025
# ----------
checked_2025 <- read.delim("ABS_code_2025_manually_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

title_country_2025 <- read.delim("/Users/yueyu/Desktop/MER/1_Raw_Data/Raw_cleaned_for_code_2025.txt",
                      header = TRUE,
                      colClasses = "character",
                      fileEncoding = "Latin1",
                      stringsAsFactors = FALSE)

# merge two dataframes based on "title"
merged_2025_all <- merge(checked_2025, title_country_2025, by = "title", all.x = TRUE)
merged_2025_subset <- merged_2025_all[,c(1,2,9,11,12,13)]
merged_2025_subset$year <- "2025"
merged_2025_subset$year <- as.numeric(merged_2025_subset$year)


# data cleaning
merged_2025_subset$authors <- gsub("Ê","", merged_2025_subset$authors)

head(merged_2025_subset)
dim(merged_2025_subset)

write.table(merged_2025_subset, file = "merged_2025_subset.txt", sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE) 

rm(merged_2025_subset)

merged_2025_subset <- read.delim("merged_2025_subset.txt",
                      header = TRUE,
                      colClasses = "character",
                      fileEncoding = "Latin1",
                      stringsAsFactors = FALSE)



# ----------
#   2026
# ----------
checked_2026 <- read.delim("ABS_code_2026_manually_checked.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)

title_country_2026 <- read.delim("/Users/yueyu/Desktop/MER/1_Raw_Data/Raw_cleaned_for_code_2026.txt",
                      header = TRUE,
                      colClasses = "character",
                      fileEncoding = "Latin1",
                      stringsAsFactors = FALSE)

# merge two dataframes based on "title"
merged_2026_all <- merge(checked_2026, title_country_2026, by = "title", all.x = TRUE)
merged_2026_subset <- merged_2026_all[,c(1,2,9,11,12,13)]
merged_2026_subset$year <- "2026"
merged_2026_subset$year <- as.numeric(merged_2026_subset$year)


# data cleaning
merged_2026_subset$authors <- gsub("Ê","", merged_2026_subset$authors)

head(merged_2026_subset)
dim(merged_2026_subset)


write.table(merged_2026_subset, file = "merged_2026_subset.txt", sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE) 



# -----------------------------------------------
#
#    Step 4.2: Pool across 2023-2025  --->   Plot Figure 3b
#
# -----------------------------------------------

pool <- rbind(merged_2023_subset,merged_2024_subset,merged_2025_subset,merged_2026_subset)
pool <- pool[,c(3,5)]
pool <- pool %>%
  filter(!is.na(benefit_summary),
         benefit_summary != "")

 benefit_summary                                                       countries
1               B                                                Italy, Spain, UK
2             A,B Australia, Canada, China, France, Japan, Spain, Sweden, UK, USA
3             A,B                                  Brazil, Canada, France , Spain
4             A,B                                   France, Germany, Italy, Spain
5             A,C                                                          Spain 
6             A,B                                                          France


# -- Recode all country name based on if they are in signed country list
# -- If all countires for a publication is within sign list -> YES_ABS_country
# -- If all countires for a publication is NOT within sign list -> NO_ABS_country
# -- If all countires for a publication is mixed with both -> mixed_ABS_country

sign <- read.delim("/Users/yueyu/Desktop/MER/1_Raw_Data/Signed_country_list.txt",
                      header = TRUE,
                      colClasses = "character",
                      stringsAsFactors = FALSE)
dim(sign) # 142 countries signed

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



# -- LONG FORMAT

# Split benefit_summary into multiple rows (one per benefit)
pool_long <- pool %>%
  separate_rows(benefit_summary, sep = ",\\s*")

# Summarize counts per benefit and country_type
summary_counts <- pool_long %>%
  group_by(benefit_summary, country_type) %>%
  summarise(count = n(), .groups = "drop")


summary_counts

   benefit_summary country_type  count
   <chr>           <chr>         <int>
 1 A               Non-Party        45
 2 A               Party (All)      64
 3 A               Party (Mixed)    45
 4 B               Non-Party        42
 5 B               Party (All)      62
 6 B               Party (Mixed)    43
 7 C               Non-Party         9
 8 C               Party (All)      11
 9 C               Party (Mixed)    11
10 D               Party (All)       1
11 E               Non-Party        11
12 E               Party (All)      13
13 E               Party (Mixed)    11
14 F               Non-Party         5
15 F               Party (All)       7
16 F               Party (Mixed)    10



# -- Plotting!!

summary_prop <- summary_counts %>%
        group_by(country_type) %>%
        mutate(
          total = sum(count),
          prop = round((count / total * 100), digits = 1))

summary_prop$country_type <- factor(
  summary_prop$country_type,
  levels = c("Party (Mixed)", "Non-Party", "Party (All)")
)

pal1 <- c("#81a9ad","#537380","#334540")

p2 <- ggplot(summary_prop, aes(x = benefit_summary, y = prop, fill = country_type)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(
    values = pal1,
    name = ""     # legend title "Nagoya Protocol Status"
  ) +
  labs(
    x = "Benefit category",
    y = "Proportion of publications"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.title.x = element_text(size = 14, margin = margin(t = 8)),
    axis.title.y = element_text(size = 14, margin = margin(r = 8)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )





# Combine plots next to each other
library(patchwork)

combined_plot <- p1 | p2  # use p1 | p2 for side-by-side
combined_plot

# Figure 3 done





# -----------------------------------------------
#
#    Step 5: Finalize Table
#
# -----------------------------------------------

# TO DO 


# END