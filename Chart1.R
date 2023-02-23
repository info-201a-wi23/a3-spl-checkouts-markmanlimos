library(dplyr)
library(stringr)
library(ggplot2)

# load in data
spl_df <- read.csv("2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Creating date column that has month and year with default first day of month
# then converts them to date
spl_df <- spl_df %>% mutate(date = as.Date(paste0(CheckoutYear, '-', CheckoutMonth,'-01'),'%Y-%m-%d'))

# Purpose of chart:
# show how 3 authors that I read as a kid trended in 2022

# filtering for the 3 authors and selecting just the relevant data
pj_hp_lotr_df <- spl_df %>%
  filter(CheckoutYear == 2022) %>%
 # select(Title, Checkouts) %>%
  filter(Creator %in% c("J.R.R. Tolkien", "Tolkien, J.R.R.", "Rick Riordan", "Riordan, Rick", "J. K. Rowling", "Rowling, J. K.")) %>%
  mutate(Creator = str_replace(Creator, "Tolkien, J.R.R.", "J.R.R. Tolkien")) %>%
  mutate(Creator = str_replace(Creator, "Riordan, Rick", "Rick Riordan")) %>%
  mutate(Creator = str_replace(Creator, "Rowling, J. K.", "J. K. Rowling")) %>%
  group_by(CheckoutMonth, Creator) %>%
  summarize(creator_total = sum(Checkouts))

# Creating the PLot:
ggplot(data = pj_hp_lotr_df) +
  geom_point(mapping = aes(x = CheckoutMonth, y = creator_total, col = Creator)) +
  geom_line(mapping = aes(x = CheckoutMonth, y = creator_total, col = Creator)) +
  labs(
    title = "Monthly Checkouts by These 3 Authors in 2022",
    x = "Month",
    y = "Number of Checkouts",
    col = "Author"
  ) +
  scale_x_continuous(breaks = seq(1,12,1))

