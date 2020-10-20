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

midlow <- gdp %>% anti_join(start_high, by = 'Country_Code')


##add rank per year and income level
midlow <- midlow %>%
  group_by(year) %>%
  arrange(desc(GDP)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

midlow <- midlow %>% mutate(level = if_else(GDP < 1000, 'low',
                                      if_else(GDP < 12000, 'middle', 'high')))

midlow$level <- as.factor(midlow$level)
midlow$level <- midlow$level %>% fct_relevel(c('high', 'middle', 'low'))


# Plot middle income trap ----
midlow <- midlow %>% filter(Country_Code != 'VEN')
  ##Venezuela starts as high income but later falls back to middle, giving an initial high-income dot

 ##isolate US as a reference point
usa <- gdp %>% filter(Country_Code == 'USA')

p <-
ggplot(data = midlow %>% filter (!is.na(GDP)),
       aes(x = rank,
           y = GDP))+
  geom_point(aes(color = level), size = 4, shape = 1)+
  geom_hline(yintercept = 12000, color = 'firebrick')+
  geom_point(data = usa, aes(x = 30, y = GDP), size = 4, shape = 1, color = 'black')+
  geom_text(data = usa, label = 'USA', aes(x = 38, y = GDP))+
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

## repeat with focus on South Korea

 ###add rank for lowest income
midlow <- midlow %>%
  group_by(Country_Code) %>%
  mutate(lowpoint = min(GDP, na.rm = TRUE)) %>%
  ungroup()

lowpoint <- unique(midlow$lowpoint)
startpoints <- data.frame(lowpoint) %>% arrange(desc(lowpoint)) %>% mutate(starting_rank = row_number())

midlow <- midlow %>%
  left_join(startpoints, by = 'lowpoint')

kor <- midlow %>% filter(Country_Code == 'KOR')  

income_colors <- c(high = 'forestgreen', middle = 'skyblue2', low = 'palevioletred1')
show_col(income_colors)
midlow$korea <- 'no'
kor$korea <- 'yes'
kor_colors <- c(yes = 'grey50', no = 'white')  ##necessary to avoid filling color in legend

kor_rank <- min(kor$starting_rank)

p <-
  ggplot(data = midlow %>% filter (!is.na(GDP)),
         aes(x = starting_rank,
             y = GDP))+
  geom_point(aes(color = level, fill = korea), size = 4, shape = 1, stroke = 1.5)+
  scale_color_manual(values = income_colors)+
  scale_fill_manual(values = kor_colors)+
  geom_point(data = kor, aes(x = starting_rank, y = GDP, color = level, fill = korea), size = 4, shape = 21, stroke = 1.5)+
  geom_hline(yintercept = 12000, color = 'firebrick')+
  geom_text(data = kor, label = 'Korea', aes(x = (kor_rank + 3), y = GDP), size = 5, hjust = 1)+
  theme_minimal()+
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16))+
  scale_x_reverse(limits = c(130, -11), name = NULL, labels = NULL, breaks = NULL, minor_breaks = NULL)+
  scale_y_continuous(labels = comma_format())+
  ggtitle('Korea Escapes Middle-Income Trap',
          subtitle = ' GDP per capita in {frame_time}')+
  labs(color = 'Income\n Level',
       y = 'GDP Per Capita')+
  guides(fill = FALSE)+
  transition_time(year)+
  ease_aes('cubic-in-out')

animate(p, fps = 2, end_pause = 50)
anim_save('Korea_escapes_middle_income_trap.gif')
