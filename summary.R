library(dplyr)
library(stringr)
library(ggplot2)

# Loading in data set (3rd)
spl_df <- read.csv("2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Creating date column that has month and year with default first day of month
# then converts them to date
spl_df <- spl_df %>% mutate(date = as.Date(paste0(CheckoutYear, '-', CheckoutMonth,'-01'),'%Y-%m-%d'))

# First Summary Info:
# Question: What month has the least amount of physical book checkouts?

least_physical_month <- spl_df %>% 
  group_by(date) %>%
  filter(MaterialType == "BOOK") %>%
  summarize(total_physical_checkouts = sum(Checkouts)) %>%
  filter(total_physical_checkouts == min(total_physical_checkouts)) %>%
  pull(date)

least_physical_month

# Second Summary Info:
# Question: How has Rick Riordan's checkouts changed over the months?

rick_riordan_df <- spl_df %>% 
  filter(str_detect(Creator, "Rick")) %>%
  filter(str_detect(Creator, 'Riordan'))
rr_checkouts_per_month <- rick_riordan_df %>% 
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts))
rr_checkouts_per_month

# Third Summary Info:
# Question: How many total checkouts are there for each material type? 
# which type(s) has the most least checkouts? How many checkouts were there?

material_totals <- spl_df %>% 
  group_by(MaterialType) %>%
  summarize(totals =  sum(Checkouts)) %>%
  arrange(-totals)
material_totals_min <- spl_df %>% 
  group_by(MaterialType) %>%
  summarize(totals =  sum(Checkouts)) %>%
  filter(totals == min(totals))

material_totals
material_totals_min

# Fourth Summary Info:
# Question: 

# Fifth Summary Info:
# Question: 
