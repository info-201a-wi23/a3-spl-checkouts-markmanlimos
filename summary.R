library(dplyr)
library(stringr)

# Loading in data set (3rd)
spl_df <- read.csv("2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Creating date column that has month and year with default first day of month
# then converts them to date
spl_df <- spl_df %>% mutate(date = as.Date(paste0(CheckoutYear, '-', CheckoutMonth,'-01'),'%Y-%m-%d'))

# First Summary Info:
# Question: What month has the least amount of physical book checkouts?
# making a new dataframe for names of months just for this summary statistic I dont want to graph with categorical
# months.
month_df <- spl_df
month_df["CheckoutMonth"][month_df["CheckoutMonth"] == 1] <- "January"
month_df["CheckoutMonth"][month_df["CheckoutMonth"] == 2] <- "February"
month_df["CheckoutMonth"][month_df["CheckoutMonth"] == 3] <- "March"
month_df["CheckoutMonth"][month_df["CheckoutMonth"] == 4] <- "April"
month_df["CheckoutMonth"][month_df["CheckoutMonth"] == 5] <- "May"
month_df["CheckoutMonth"][month_df["CheckoutMonth"] == 6] <- "June"
month_df["CheckoutMonth"][month_df["CheckoutMonth"] == 7] <- "July"
month_df["CheckoutMonth"][month_df["CheckoutMonth"] == 8] <- "August"
month_df["CheckoutMonth"][month_df["CheckoutMonth"] == 9] <- "September"
month_df["CheckoutMonth"][month_df["CheckoutMonth"] == 10] <- "October"
month_df["CheckoutMonth"][month_df["CheckoutMonth"] == 11] <- "November"
month_df["CheckoutMonth"][month_df["CheckoutMonth"] == 12] <- "December"
                
least_physical_month <- month_df %>% 
  filter(CheckoutYear == 2022) %>%
  group_by(CheckoutMonth) %>%
  filter(MaterialType == "BOOK") %>%
  summarize(total_physical_checkouts = sum(Checkouts)) %>%
  filter(total_physical_checkouts == min(total_physical_checkouts)) %>%
  pull(CheckoutMonth)

# Second Summary Info:
# Question: what was rick riordans most checked out book?

rick_riordan_df <- spl_df %>% 
  filter(str_detect(Creator, "Rick")) %>%
  filter(str_detect(Creator, 'Riordan'))
rr_checkouts_per_month <- rick_riordan_df %>% 
  group_by(Title) %>%
  summarize(total_checkouts = sum(Checkouts)) %>%
  filter(total_checkouts == max(total_checkouts)) %>%
  pull(Title)

# Third Summary Info:
# Question: How many total checkouts are there for each material type? 
# which type(s) has the most most checkouts?

material_totals <- spl_df %>% 
  group_by(MaterialType) %>%
  summarize(totals =  sum(Checkouts)) %>%
  arrange(-totals)
material_totals_max <- spl_df %>% 
  group_by(MaterialType) %>%
  summarize(totals =  sum(Checkouts)) %>%
  filter(totals == max(totals)) %>%
  pull(MaterialType)

# Fourth Summary Info:
# Question: What Book was the most checked out in 2022?

book_most_checked <- spl_df %>%
  filter(date == as.Date("2022-09-01")) %>%
  group_by(Title) %>%
  summarize(most_checkedout = max(Checkouts)) %>%
  filter(most_checkedout == max(most_checkedout)) %>%
  pull(Title)


# Fifth Summary Info:
# Question: What month had the biggest largest increase in checkouts in 2022?

largest_monthly_checkouts <- month_df %>%
  filter(CheckoutYear == 2022) %>%
  group_by(CheckoutMonth) %>%
  summarize(total_checkouts = sum(Checkouts)) %>%
  mutate(delta_checkout = total_checkouts - lag(total_checkouts)) %>%
  filter(delta_checkout == max(delta_checkout, na.rm = TRUE)) %>%
  pull(CheckoutMonth)
