library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
# load in data
spl_df <- read.csv("2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Creating date column that has month and year with default first day of month
# then converts them to date
spl_df <- spl_df %>% mutate(date = as.Date(paste0(CheckoutYear, '-', CheckoutMonth,'-01'),'%Y-%m-%d'))

# Purpose of chart:
# To show how on average we tend to check out less and less physical copies of books, even the most popular ones 

# creating a dataframe of all the harry potter books
# i may have to hardcode the names just because I dont know how to do it otherwise
# THIS IS the tedius work to get the names the same for all books
hp_df <- spl_df %>%
  mutate(Title = tolower(Title)) %>%
  filter(str_detect(Title, "harry potter and the sorcerer's stone") | str_detect(Title, "harry potter and the chamber of secrets") | str_detect(Title, "harry potter and the prisoner of azkaban") | str_detect(Title, "harry potter and the goblet of fire") | str_detect(Title, "harry potter and the order of the phoenix") |str_detect(Title, "harry potter and the half-blood prince") | str_detect(Title, "harry potter and the deathly hallows")) %>%
  mutate(title_simple = '')

# removed "harry potter and the" so that my graphics labels didn't overlap
hp_df$title_simple[str_detect(hp_df$Title, "sorcerer")] <- "Sorcerer's Stone"
hp_df$title_simple[str_detect(hp_df$Title, "chamber")] <- "Chamber of Secrets"
hp_df$title_simple[str_detect(hp_df$Title, "prisoner")] <- "Prisoner of Azkaban"
hp_df$title_simple[str_detect(hp_df$Title, "goblet")] <- "Goblet of Fire"
hp_df$title_simple[str_detect(hp_df$Title, "phoenix")] <- "Order of the Phoenix"
hp_df$title_simple[str_detect(hp_df$Title, "prince")] <- "Half-blood Prince"
hp_df$title_simple[str_detect(hp_df$Title, "hallows")] <- "Deathly Hallows"

hp_df_class <- hp_df %>% group_by(title_simple, UsageClass) %>%
  summarize(total_book_checkouts = sum(Checkouts))
hp_df_class$title_simple <- factor(hp_df_class$title_simple, levels = c("Sorcerer's Stone", "Chamber of Secrets", "Prisoner of Azkaban", "Goblet of Fire", "Order of the Phoenix", "Half-blood Prince", "Deathly Hallows"))
# Creating the Plot

# pie charts didn't work out, they look too funny
#ggplot(data = hp_df_class,aes(x = '', y = total_book_checkouts, group = UsageClass, fill = UsageClass)) +
  #geom_bar(width = 1, stat = "identity") +
  #geom_text(aes(label = total_book_checkouts), position = position_stack(vjust = 0.5), col = "white", size = 4) +
  #coord_polar(theta = "y", start = 0) +
  #facet_wrap(~ title_simple, ncol = 4) + 
  #labs(
    #title = "Medium of Checkouts for the 7 Harry Potter Books",
  #) +
  #theme_void()

ggplot(data = hp_df_class, aes(fill = UsageClass, y = total_book_checkouts, x = title_simple)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(
    title = "Medium of Checkouts for the 7 Harry Potter Books",
    x = "Book Name",
    y = "Number of Checkouts",
    fill = "Checkout Type"
  ) 