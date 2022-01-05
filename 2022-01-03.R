###First TidyTuesday###
###1/3/2022- BYOD###

###Library###
library('tidytuesdayR')
library('ggplot2')
library('dplyr')
library('tidyverse')
library('lubridate')

###Knowing an ultra trail runner on an (indirectly) personal level, when are their races held during the year?###

###Load in data###
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

###Bar chart of number of events per month per country
race_country <- race %>%
  select(country) %>%
  mutate(month = month(race$date))

###Update month numbers to full names###
race_country$month <- month.name[race_country$month]

###Look up countries for potential duplicates / misspellings###
unique(race_country$country)

###Clean up countries###
race_country$country <- replace(race_country$country, race_country$country == 'Hong Kong, China', 'Hong Kong')
race_country$country <- replace(race_country$country, race_country$country == 'LA, United States', 'United States')
race_country$country <- replace(race_country$country, race_country$country == 'FL, United States', 'United States')
race_country$country <- replace(race_country$country, race_country$country == 'PA, United States', 'United States')
race_country$country <- replace(race_country$country, race_country$country == 'Myoko, Japan', 'Japan')

###Just include US & Canada###
race_country <- race_country %>%
  filter(country == 'United States' | country == 'Canada')

###Grab counts per country & month###
race_country <- race_country %>% count(country, month)


###Create order for yearly progression###
###race_country$month <- factor(race_country$month, levels = month.name)
###race_country <- race_country[order(race_country$country,race_country$month),]
###race_country <- race_country %>%
###arrange(country,match(month, month.name))
race_country$month <- factor(race_country$month, levels = c('January',
                             'February', 'March', 'April', 'May', 'June',
                             'July', 'August', 'September', 'October', 'November', 'December'))

###Counts per month per country bar chart###
ggplot(race_country, aes(n, month, fill = country)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(country), scales = 'free_y') +
  xlab('Number of Races') +
  ylab('Race Month') +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Ultra Trail Races in Canada & the United States (2012 - 2021)") +
  theme_light() +
  scale_y_discrete(limits = rev) +
  geom_text(aes(label = n), hjust = -.2) +
  labs(subtitle = 'Count of Ultra Trail Races per Month in the United States & Canada',
       caption = '
       Data: International Trail Running Association (ITRA) 
       2022 TidyTuesday Week 1
       Twitter: @devinnehart | GitHub: devhart95')
