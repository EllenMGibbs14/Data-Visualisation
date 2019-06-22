bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

library(tidyverse)
library(magrittr)

str(bird_counts)
View(bird_counts)

Count_per_year_species <- bird_counts %>% 
  group_by(year, species) %>% 
  summarise(num_count = sum(how_many_counted)) %>% 
  arrange(species)
View(Count_per_year_species)

str(Count_per_year_species)
dim(Count_per_year_species)

Diff_count <- Count_per_year_species %>% 
  group_by(species) %>% 
  summarise(variation = (max(num_count) - min(num_count))) %>% 
  arrange(desc(variation))
View(Diff_count)

Count_per_year_species <- Count_per_year_species %>% 
  mutate(species = factor(species, levels = Diff_count$species)) %>% 
  arrange(species)

with(bird_counts, length(unique(species)))
with(Count_per_year_species, length(year)/length(unique(year)))
with(Count_per_year_species, length(unique(year)))

a <- 1; b <- 5
ggplot(data = Count_per_year_species[((a-1)*94 + 1):(b*94),], 
       aes(x = year, y = num_count, group = species, colour = species)) + 
  geom_line() + 
  labs(x = "Year", y = "Number Seen/Heard per Year", colour = "Species", 
       title = "Number of Each Species Seen/Heard per Year in Ontario, Canada",
       subtitle = "Top Five in Variation")
ggsave("TT_18062019.png")

ggplot(data = Diff_count) + 
  geom_line(aes(x = seq_along(variation), y = variation))

ggplot(data = Count_per_year_species[1:94,], 
       aes(x = year, y = num_count, group = species, colour = species)) + 
  geom_line()

plot(Diff_count$variation, type='l')
