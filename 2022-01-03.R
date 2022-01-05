###First TidyTuesday###
###1/3/2022- BYOD###

###Library###
library('tidytuesdayR')
library('ggplot2')
library('dplyr')
library('tidyverse')
library('lubridate')

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

race_country <- race_country %>%
  filter(country == 'United States' | country == 'Canada')

race_country <- race_country %>% count(country, month)


###Create order for yearly progression###
###race_country$month <- factor(race_country$month, levels = month.name)
###race_country <- race_country[order(race_country$country,race_country$month),]
###race_country <- race_country %>%
###arrange(country,match(month, month.name))
race_country$month <- factor(race_country$month, levels = c('January',
                             'February', 'March', 'April', 'May', 'June',
                             'July', 'August', 'September', 'October', 'November', 'December'))

###Add in color palette###
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

###Counts per month per country###
ggplot(race_country, aes(n, month, fill = country)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(country), scales = 'free_y') +
  xlab('Number of Races') +
  ylab('Race Month') +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Ultra Trail Races in Canada & the United States (2012 - 2021)") +
  theme_light() +
  scale_y_discrete(limits = rev) +
  geom_text(aes(label = n), hjust = -.5) +
  labs(subtitle = 'Count of Ultra Trail Races per Month in the United States & Canada',
       caption = '
       Data: International Trail Running Association (ITRA) 
       2022 TidyTuesday Week 1
       Twitter: @devinnehart | GitHub: devhart95')
