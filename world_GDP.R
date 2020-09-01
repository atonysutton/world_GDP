library(tidyverse)
library(lubridate)
library(gganimate)
library(gifski)
library(png)
library(scales)

# Load data ----

 ## Data from World Bank at https://datacatalog.worldbank.org/
 ## downloaded on 8 February 2020

gdp_wide <- read_csv('world_GDP.csv')

gdp <- gdp_wide %>% pivot_longer(cols = c(-Country, -Country_Code), names_to = "year", values_to = "GDP")

gdp$year <- mdy(gdp$year)

gdp$year <- as.integer(year(gdp$year))

# Shape data ----

##remove small countries
pop <- read_csv('world_population.csv', )
pop$population <- as.integer(pop$population)
smalls <- pop %>% filter(population < 1000000)

gdp <- gdp %>% anti_join(smalls, by = 'Country_Code')


##remove initially high income
start_high <- gdp %>% 
  group_by(Country_Code) %>%
  filter(min(GDP, na.rm = TRUE) > 12000)

gdp <- gdp %>% anti_join(start_high, by = 'Country_Code')


##add rank per year and income level
gdp <- gdp %>%
  group_by(year) %>%
  arrange(desc(GDP)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

gdp <- gdp %>% mutate(level = if_else(GDP < 1000, 'low',
                                      if_else(GDP < 12000, 'middle', 'high')))

gdp$level <- as.factor(gdp$level)
gdp$level <- gdp$level %>% fct_relevel(c('high', 'middle', 'low'))


# Plot middle income trap ----
gdp <- gdp %>% filter(Country_Code != 'VEN')
  ##Venezuela starts as high income but later falls back to middle, giving an initial high-income dot

p <-
ggplot(data = gdp %>% filter (!is.na(GDP)),
       aes(x = rank,
           y = GDP))+
  geom_point(aes(color = level), size = 4, shape = 1)+
  geom_hline(yintercept = 12000, color = 'firebrick')+
  scale_x_reverse(name = NULL, labels = NULL, breaks = NULL, minor_breaks = NULL)+
  scale_y_continuous(labels = comma_format())+
  theme_minimal()+
  ggtitle('Middle-Income Trap',
          subtitle = ' GDP per capita in {frame_time}')+
  theme(title = element_text(size = 22),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16))+
  transition_time(year)+
  ease_aes('cubic-in-out')

animate(p, fps = 5, end_pause = 30)
anim_save('middle_income_trap.gif')

 ##outlier at top in late years is Singapore, which is properly coded as starting below the threshold
