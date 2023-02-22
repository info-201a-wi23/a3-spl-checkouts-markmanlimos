library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
# load in data
spl_df <- read.csv("2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Creating date column that has month and year with default first day of month
# then converts them to date
spl_df <- spl_df %>% mutate(date = as.Date(paste0(CheckoutYear, '-', CheckoutMonth,'-01'),'%Y-%m-%d'))

# Purpose of chart: compare checkout rates of 3 checkout types (Book, Ebook, and Audiobook) over months in 2022

# filter for the 3 book types and other relevant data

material_checkouts <- spl_df %>%
  filter(CheckoutYear == 2022) %>%
  filter(MaterialType %in% c("BOOK", "AUDIOBOOK", "EBOOK")) %>%
  group_by(CheckoutMonth, MaterialType) %>%
  summarize(material_total = sum(Checkouts))

# Creating the Plot

ggplot(data = material_checkouts, aes(fill = MaterialType, y = material_total, x = CheckoutMonth)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(
    title = "Comparing Medium of Checkouts by Month in 2022",
    x = "Checkout Month",
    y = "Number of Checkouts"
  ) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  scale_y_continuous(labels = label_number_si())